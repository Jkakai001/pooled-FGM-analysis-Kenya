import json, uuid, sys
import os
S = os.path.dirname(os.path.abspath(__file__))

V1, OUT = sys.argv[1], sys.argv[2]
ref = json.load(open(f"{V1}/FGM_Prevalence_Incidence.ipynb"))

def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]
def md(t): return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(t)}
def code(t): return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "execution_count": None, "metadata": {}, "outputs": [], "source": lines(t)}

cells = [
md("""
# FGM Prevalence, Kenya, all survey rounds (V2)

Same methodology as V1 (`V1/01_Notebooks/FGM_Prevalence_Incidence.ipynb`, DHS 2022 only)
applied to every Kenya DHS round: DHS 1998, 2003, 2008–09, 2014 and 2022 for women 15–49, and
the two full-roster rounds (DHS 2014, 2022) for daughters 0–14. Kenya has no national MICS;
its subnational MICS are compared in section 8A. Run `00_EDA_All_Rounds.ipynb` first.

- **Pooled data.** All rounds are analysed together. Direct estimates use one pooled
  survey design (each survey keeps its own clusters and strata), and the covariate-adjusted
  model is one survey-weighted BYM2 model on all rounds, with a survey-round term absorbing
  level differences between rounds. Maps show the pooled estimates for the 8 former provinces
  and, as in V1, estimates by birth cohort; section 8B adds county estimates (47 counties) from
  DHS 2014 and 2022.
- **Covariates not collected in a round** (wealth in DHS 1998) are kept as
  an "unknown" category, so no round is dropped.
- **Trends by survey year** (section 7) are the only outputs shown by round.
- **Definitions.** Never-heard women and their daughters are counted as not cut (DHS
  convention); women with a missing status are excluded. DHS 2014 and 2022 asked the FGM
  questions only in the long-questionnaire subsample.
"""),
md("## 0. Configuration"),
code("""
country_name <- "Kenya"
country_iso3 <- "KEN"

project_root <- if (dir.exists("../02_Data")) ".." else "."
analysis_dir <- file.path(project_root, "02_Data", "processed", country_iso3, "analysis")
output_dir <- file.path(project_root, "03_Outputs", country_iso3, "Prevalence")
dirs <- list(
  direct = file.path(output_dir, "01_Direct_and_Smoothed"),
  maps = file.path(output_dir, "02_Prevalence_Maps"),
  adjusted = file.path(output_dir, "03_Covariate_Adjusted"),
  cohort = file.path(output_dir, "04_Birth_Cohort"),
  trends = file.path(output_dir, "05_Trends"),
  validation = file.path(output_dir, "06_Validation")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

shapefile_path <- file.path(project_root, "02_Data", "processed", "KEN", "boundaries", "KEN_provinces.shp")
region_var_shapefile <- "PROVINCE"
# Region keys as in the harmonized data: upper case, hyphens as spaces ("CENTRE EST").
standardize_region <- function(x) toupper(as.character(x)) |> stringr::str_replace_all("[^A-Z ]", " ") |> stringr::str_squish()
posterior_draws <- 200L
minimum_cohort_cell_n <- 10L
run_spatial_model <- file.exists(shapefile_path)

options(repr.plot.width = 14, repr.plot.height = 9, width = 200)
cat("Configured for:", country_name, "| outputs:", normalizePath(output_dir, mustWork = FALSE), "\\n")
"""),
md("## 1. Setup and packages"),
code("""
suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(sf)
  library(spdep)
  library(INLA)
})
options(survey.lonely.psu = "adjust")
set.seed(20260101)
"""),
md("## 2. Load the prevalence data"),
code("""
round_order <- c("DHS 1998", "DHS 2003", "DHS 2008", "DHS 2014", "DHS 2022")
women <- readRDS(file.path(analysis_dir, paste0(country_iso3, "_prevalence_self.rds"))) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_order))
daughters <- readRDS(file.path(analysis_dir, paste0(country_iso3, "_prevalence_daughter.rds"))) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_order))
survey_years <- women |> dplyr::distinct(survey_id, survey_type, round_year)

IRdisplay::display(as.data.frame(dplyr::full_join(
  women |> dplyr::count(survey_id, name = "women_15_49"),
  daughters |> dplyr::count(survey_id, name = "daughters_0_14"), by = "survey_id") |> dplyr::arrange(survey_id)))
"""),
md("""
## 3. Design-based direct estimates, by region (pooled)

One pooled survey design across all rounds; each survey keeps its own clusters and strata.
Daughters inherit their mother's design variables. National estimates by survey round are
kept for the trend in section 7.
"""),
md("Shared functions: pooled model, pooled direct estimates and map layouts."),
code(open(f"{S}/pooled_model.R").read()),
code("""
direct_self <- pooled_direct(women, "fgm_self", "region")
direct_daughter <- pooled_direct(daughters, "fgm_daughter", "region")
national_direct <- dplyr::bind_rows(
  pooled_direct(women, "fgm_self", "survey_id") |> dplyr::mutate(population = "Women 15-49"),
  pooled_direct(daughters, "fgm_daughter", "survey_id") |> dplyr::mutate(population = "Girls 0-14")) |>
  dplyr::mutate(survey_id = as.character(survey_id))
readr::write_csv(direct_self, file.path(dirs$direct, paste0(country_iso3, "_prevalence_direct_by_region_self.csv")))
readr::write_csv(direct_daughter, file.path(dirs$direct, paste0(country_iso3, "_prevalence_direct_by_region_daughter.csv")))
readr::write_csv(national_direct, file.path(dirs$direct, paste0(country_iso3, "_prevalence_direct_national_by_round.csv")))
cat("Pooled direct estimates:", nrow(direct_self), "regions (women),", nrow(direct_daughter), "regions (girls)\\n")
"""),
md("""
## 4. BYM2 spatial smoothing (pooled)

V1 architecture on the pooled direct estimates: logit-scale region-adjacency BYM2 via INLA,
each estimate weighted by its sampling precision (capped at 1e6). Penalised-complexity
priors and explicit starting values are used for the BYM2 parameters, because INLA's default
settings intermittently failed to converge on this model.
"""),
code("""
boundaries <- sf::st_read(shapefile_path, quiet = TRUE) |> sf::st_make_valid()
region_keys <- standardize_region(boundaries[[region_var_shapefile]])
adjacency <- spdep::poly2nb(boundaries, row.names = region_keys)
adjacency_matrix <- spdep::nb2mat(adjacency, style = "B", zero.policy = TRUE)
stopifnot(all(unique(c(women$region, daughters$region)) %in% region_keys))

fit_bym2_prevalence <- function(direct_estimates) {
  eps <- 1e-4
  d <- direct_estimates |>
    dplyr::mutate(region_id = match(region, region_keys), p = pmin(pmax(direct, eps), 1 - eps),
                  se_p = pmax(se, eps), logit_est = qlogis(p), logit_prec = pmin(((p * (1 - p))^2) / (se_p^2), 1e6))
  # Penalised-complexity priors and explicit starting values keep INLA's optimizer stable
  # on this small 13-region model (default priors can fail to converge).
  bym2_hyper <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)), phi = list(prior = "pc", param = c(0.5, 2 / 3)))
  fit_once <- function(control_inla) INLA::inla(
    logit_est ~ f(region_id, model = "bym2", graph = adjacency_matrix, scale.model = TRUE, hyper = bym2_hyper),
    family = "gaussian", data = d, scale = d$logit_prec,
    control.family = list(hyper = list(prec = list(fixed = TRUE, initial = 0))),
    control.predictor = list(compute = TRUE), control.mode = list(theta = c(0, 0), restart = TRUE),
    control.inla = control_inla, num.threads = "1:1")
  # INLA's optimizer intermittently fails to converge on the first calls of a session;
  # retry, alternating default and stabilised settings (cmin = 0, empirical Bayes).
  settings <- list(list(), list(cmin = 0, int.strategy = "eb"), list(), list(cmin = 0, int.strategy = "eb"))
  model <- NULL
  for (k in seq_along(settings)) {
    model <- tryCatch(fit_once(settings[[k]]), error = function(e) NULL)
    if (!is.null(model)) break
    message("BYM2 smoothing: attempt ", k, " did not converge, retrying")
  }
  if (is.null(model)) stop("BYM2 smoothing failed after ", length(settings), " attempts")
  d |> dplyr::mutate(smoothed_prevalence = plogis(model$summary.fitted.values$mean),
                     smoothed_lower = plogis(model$summary.fitted.values$`0.025quant`),
                     smoothed_upper = plogis(model$summary.fitted.values$`0.975quant`))
}
smoothed_self <- fit_bym2_prevalence(direct_self)
smoothed_daughter <- fit_bym2_prevalence(direct_daughter)
readr::write_csv(smoothed_self, file.path(dirs$direct, paste0(country_iso3, "_prevalence_smoothed_by_region_self.csv")))
readr::write_csv(smoothed_daughter, file.path(dirs$direct, paste0(country_iso3, "_prevalence_smoothed_by_region_daughter.csv")))
"""),
md("""
## 5. Prevalence maps (pooled)

Smoothed region prevalence pooling all rounds, on one scale for women and girls.
"""),
code("""
fill_limits <- c(0, max(smoothed_self$smoothed_prevalence, smoothed_daughter$smoothed_prevalence))
self_prevalence_map <- state_map(smoothed_self, "smoothed_prevalence", paste("Subnational FGM prevalence,", country_name),
                                 "Self-reported, women 15-49, all survey rounds pooled", "FGM\\nprevalence", fill_limits)
daughter_prevalence_map <- state_map(smoothed_daughter, "smoothed_prevalence", paste("Subnational FGM prevalence,", country_name),
                                     "Daughter-reported, girls 0-14, all survey rounds pooled", "FGM\\nprevalence", fill_limits)
print(self_prevalence_map); print(daughter_prevalence_map)
ggplot2::ggsave(file.path(dirs$maps, paste0(country_iso3, "_prevalence_map_self.png")), self_prevalence_map, width = 9, height = 8, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$maps, paste0(country_iso3, "_prevalence_map_daughter.png")), daughter_prevalence_map, width = 9, height = 8, dpi = 300, bg = "white")
"""),
md("""
## 6. Survey-weighted covariate-adjusted spatial prevalence model (pooled)

V1 model fitted once to all rounds for each population: unit-level binomial with normalized
survey weights, ethnicity, wealth, education, religion and urban/rural as fixed effects,
age as RW2, region as BYM2, plus a survey-round term. For each posterior draw, fitted
probabilities are averaged with the survey weights within region, region × birth cohort, birth
cohort, survey round and nationally. Intervals are pseudo-posterior, not fully design-based.
"""),
code("""
prevalence_covariates <- c("ethnicity", "wealth", "education", "religion", "urban_rural")
t0 <- Sys.time()
model_self <- fit_pooled(prepare_pooled(women, "fgm_self", prevalence_covariates), "fgm_self", prevalence_covariates, seed = 2026L)
model_daughter <- fit_pooled(prepare_pooled(daughters, "fgm_daughter", prevalence_covariates), "fgm_daughter", prevalence_covariates, seed = 2027L)
cat("Pooled models fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\\n")
IRdisplay::display(as.data.frame(dplyr::bind_rows(model_self$fit_stats |> dplyr::mutate(population = "Women 15-49"),
                                                  model_daughter$fit_stats |> dplyr::mutate(population = "Girls 0-14"))))
fixed_effects <- dplyr::bind_rows(model_self$fixed |> dplyr::mutate(population = "Women 15-49"),
                                  model_daughter$fixed |> dplyr::mutate(population = "Girls 0-14"))
readr::write_csv(fixed_effects, file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_model_fixed_effects.csv")))
"""),
md("### 6.1 Model-assisted region prevalence (pooled)"),
code("""
adjusted_self <- model_self$state |> dplyr::rename(model_assisted_prevalence = estimate) |>
  dplyr::left_join(direct_self |> dplyr::select(region, design_based_direct = direct), by = "region")
adjusted_daughter <- model_daughter$state |> dplyr::rename(model_assisted_prevalence = estimate) |>
  dplyr::left_join(direct_daughter |> dplyr::select(region, design_based_direct = direct), by = "region")
readr::write_csv(adjusted_self, file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_covariate_adjusted_self.csv")))
readr::write_csv(adjusted_daughter, file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_covariate_adjusted_daughter.csv")))

adjusted_fill <- c(0, max(adjusted_self$model_assisted_prevalence, adjusted_daughter$model_assisted_prevalence))
adjusted_self_map <- state_map(adjusted_self, "model_assisted_prevalence", paste("Subnational FGM prevalence,", country_name),
                               "Self-reported, women 15-49, all survey rounds pooled (covariate-adjusted)", "Adjusted FGM\\nprevalence", adjusted_fill)
adjusted_daughter_map <- state_map(adjusted_daughter, "model_assisted_prevalence", paste("Subnational FGM prevalence,", country_name),
                                   "Daughter-reported, girls 0-14, all survey rounds pooled (covariate-adjusted)", "Adjusted FGM\\nprevalence", adjusted_fill)
print(adjusted_self_map); print(adjusted_daughter_map)
ggplot2::ggsave(file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_covariate_adjusted_map_self.png")), adjusted_self_map, width = 9, height = 8, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_covariate_adjusted_map_daughter.png")), adjusted_daughter_map, width = 9, height = 8, dpi = 300, bg = "white")
"""),
md("""
### 6.1A Covariate-adjusted prevalence by birth cohort (pooled)

As in V1, the same fitted model aggregated within region × five-year birth cohort, pooling
every round in which a cohort appears. Cells with fewer than 10 respondents are suppressed.
"""),
code("""
cohort_cells <- function(m) m$state_cohort |> dplyr::filter(n_unweighted >= minimum_cohort_cell_n) |>
  dplyr::rename(model_assisted_prevalence = estimate)
adjusted_self_cohort <- cohort_cells(model_self)
adjusted_daughter_cohort <- cohort_cells(model_daughter)
readr::write_csv(adjusted_self_cohort, file.path(dirs$cohort, paste0(country_iso3, "_prevalence_covariate_adjusted_by_cohort_self.csv")))
readr::write_csv(adjusted_daughter_cohort, file.path(dirs$cohort, paste0(country_iso3, "_prevalence_covariate_adjusted_by_cohort_daughter.csv")))
cohort_fill <- c(0, max(adjusted_self_cohort$model_assisted_prevalence, adjusted_daughter_cohort$model_assisted_prevalence))
self_cohort_map <- cohort_map(adjusted_self_cohort, "model_assisted_prevalence",
  paste("Subnational FGM prevalence by birth cohort,", country_name), "Self-reported, women 15-49, all survey rounds pooled",
  "Adjusted FGM\\nprevalence", cohort_fill)
daughter_cohort_map <- cohort_map(adjusted_daughter_cohort, "model_assisted_prevalence",
  paste("Subnational FGM prevalence by birth cohort,", country_name), "Daughter-reported, girls 0-14, all survey rounds pooled",
  "Adjusted FGM\\nprevalence", cohort_fill)
print(self_cohort_map); print(daughter_cohort_map)
ggplot2::ggsave(file.path(dirs$cohort, paste0(country_iso3, "_prevalence_covariate_adjusted_map_by_cohort_self.png")), self_cohort_map, width = 14, height = 12, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$cohort, paste0(country_iso3, "_prevalence_covariate_adjusted_map_by_cohort_daughter.png")), daughter_cohort_map, width = 14, height = 7, dpi = 300, bg = "white")
"""),
md("""
### 6.2 Residual geographic pattern (pooled)

The BYM2 region effect after accounting for composition and survey round: positive means
higher than composition alone predicts. It is a relative index, not a prevalence.
"""),
code("""
readr::write_csv(dplyr::bind_rows(model_self$residual |> dplyr::mutate(population = "Women 15-49"),
                                  model_daughter$residual |> dplyr::mutate(population = "Girls 0-14")),
                 file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_residual_spatial.csv")))
residual_self_map <- residual_map(model_self$residual, paste("Residual geographic pattern after adjusting for composition,", country_name),
                                  "Self-reported, women 15-49, all survey rounds pooled")
residual_daughter_map <- residual_map(model_daughter$residual, paste("Residual geographic pattern after adjusting for composition,", country_name),
                                      "Daughter-reported, girls 0-14, all survey rounds pooled")
print(residual_self_map); print(residual_daughter_map)
ggplot2::ggsave(file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_residual_map_self.png")), residual_self_map, width = 9, height = 8, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$adjusted, paste0(country_iso3, "_prevalence_residual_map_daughter.png")), residual_daughter_map, width = 9, height = 8, dpi = 300, bg = "white")
"""),
md("""
## 7. Trends

(1) National prevalence by survey year, design-based and model-based; (2) prevalence by
birth cohort from the pooled model; (3) design-based prevalence by single birth year.
"""),
code("""
national_trend <- dplyr::bind_rows(
  national_direct |> dplyr::transmute(survey_id, population, estimate = direct, lower_ci, upper_ci, method = "Design-based direct"),
  dplyr::bind_rows(model_self$survey |> dplyr::mutate(population = "Women 15-49"),
                   model_daughter$survey |> dplyr::mutate(population = "Girls 0-14")) |>
    dplyr::transmute(survey_id, population, estimate, lower_ci, upper_ci, method = "Covariate-adjusted model")
) |>
  dplyr::left_join(survey_years |> dplyr::mutate(survey_id = as.character(survey_id)), by = "survey_id") |>
  dplyr::mutate(population = factor(population, levels = c("Women 15-49", "Girls 0-14")))
readr::write_csv(national_trend, file.path(dirs$trends, paste0(country_iso3, "_prevalence_national_by_round.csv")))
survey_colours <- c(DHS = "#1E4FBF", MICS = "#D7301F")
direct_trend <- national_trend |> dplyr::filter(method == "Design-based direct")
national_trend_plot <- ggplot2::ggplot(direct_trend, ggplot2::aes(round_year, estimate, color = survey_type)) +
  ggplot2::geom_line(ggplot2::aes(group = 1), color = "grey55", linetype = "dotted", linewidth = 0.7) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), width = 0.35, linewidth = 0.8) +
  ggplot2::geom_point(size = 3.6) +
  ggplot2::geom_text(ggplot2::aes(label = scales::percent(estimate, accuracy = 0.1)), nudge_x = 0.45, hjust = 0,
                     fontface = "bold", size = 4, show.legend = FALSE) +
  ggplot2::facet_wrap(~population, ncol = 1) +
  ggplot2::scale_color_manual(values = survey_colours, name = "Survey") +
  ggplot2::scale_x_continuous(breaks = sort(unique(direct_trend$round_year)), expand = ggplot2::expansion(add = c(0.8, 1.6))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.1),
                              limits = c(0, max(0.30, max(direct_trend$upper_ci) + 0.02))) +
  ggplot2::labs(title = paste("National FGM Prevalence by Survey Year,", country_name),
                subtitle = "Design-based estimates with 95% CI", x = "Survey year", y = "FGM prevalence") +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 22, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(size = 15, hjust = 0.5, color = "grey40", margin = ggplot2::margin(b = 10)),
                 strip.text = ggplot2::element_text(face = "bold", size = 17, margin = ggplot2::margin(t = 10, b = 6)),
                 strip.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(color = "grey88", linetype = "dashed", linewidth = 0.35),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(color = "grey45", linewidth = 0.5),
                 axis.ticks.x = ggplot2::element_line(color = "grey45"),
                 axis.title = ggplot2::element_text(size = 15), axis.text = ggplot2::element_text(size = 12, color = "grey25"),
                 legend.title = ggplot2::element_text(face = "bold", size = 15), legend.text = ggplot2::element_text(size = 13),
                 panel.spacing.y = grid::unit(1.2, "lines"))
print(national_trend_plot)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_prevalence_national_by_round.png")), national_trend_plot,
                width = 13, height = 9, dpi = 300, bg = "white")
"""),
code("""
adjusted_prevalence_trend <- dplyr::bind_rows(model_self$cohort |> dplyr::mutate(population = "Women 15-49"),
                                              model_daughter$cohort |> dplyr::mutate(population = "Girls 0-14")) |>
  dplyr::filter(n_unweighted >= minimum_cohort_cell_n) |>
  dplyr::rename(adjusted_prevalence = estimate) |>
  dplyr::mutate(population = factor(population, levels = c("Women 15-49", "Girls 0-14")))
# V1 layout: facet labels by reporting source, national average with a labelled arrow.
adjusted_prevalence_trend <- adjusted_prevalence_trend |>
  dplyr::mutate(record_type = factor(dplyr::if_else(population == "Women 15-49", "Self-reported", "Daughter-reported"),
                                     levels = c("Self-reported", "Daughter-reported")))
cohort_national_average <- adjusted_prevalence_trend |> dplyr::group_by(record_type) |>
  dplyr::summarise(national_average = stats::weighted.mean(adjusted_prevalence, sum_weight), .groups = "drop")
readr::write_csv(adjusted_prevalence_trend, file.path(dirs$trends, paste0(country_iso3, "_prevalence_trend_by_cohort.csv")))
readr::write_csv(cohort_national_average, file.path(dirs$trends, paste0(country_iso3, "_prevalence_national_average.csv")))

# Annotation at the second cohort: label below the line for women, above it for girls.
annotations <- adjusted_prevalence_trend |>
  dplyr::arrange(record_type, birth_cohort) |>
  dplyr::group_by(record_type) |> dplyr::slice(min(2L, dplyr::n())) |> dplyr::ungroup() |>
  dplyr::select(record_type, birth_cohort) |>
  dplyr::left_join(cohort_national_average, by = "record_type") |>
  dplyr::mutate(label = paste0("National average: ", scales::percent(national_average, accuracy = 0.1)),
                label_y = dplyr::if_else(record_type == "Self-reported", pmax(national_average - 0.08, 0.025), national_average + 0.115),
                arrow_start = dplyr::if_else(record_type == "Self-reported", label_y + 0.018, label_y - 0.040),
                arrow_end = dplyr::if_else(record_type == "Self-reported", national_average - 0.004, national_average + 0.004))

cohort_trend_plot <- ggplot2::ggplot(adjusted_prevalence_trend, ggplot2::aes(birth_cohort, adjusted_prevalence)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci, group = 1), fill = "grey70", alpha = 0.40) +
  ggplot2::geom_line(ggplot2::aes(group = 1), color = "#1D4E89", linewidth = 1.2) +
  ggplot2::geom_point(color = "#1D4E89", fill = "#1D4E89", shape = 21, size = 3.2, stroke = 0.8) +
  ggplot2::geom_text(ggplot2::aes(label = scales::percent(adjusted_prevalence, accuracy = 0.1)),
                     color = "#1D4E89", fontface = "bold", size = 3.5, vjust = -1) +
  ggplot2::geom_hline(data = cohort_national_average, ggplot2::aes(yintercept = national_average),
                      color = "#D62728", linewidth = 1, linetype = "dashed") +
  ggplot2::geom_segment(data = annotations, ggplot2::aes(x = birth_cohort, xend = birth_cohort, y = arrow_start, yend = arrow_end),
                        inherit.aes = FALSE, color = "#D62728", linewidth = 1.2,
                        arrow = grid::arrow(type = "closed", length = grid::unit(0.20, "cm"))) +
  ggplot2::geom_label(data = annotations, ggplot2::aes(x = birth_cohort, y = label_y, label = label), inherit.aes = FALSE,
                      color = "#D62728", fill = "white", fontface = "bold", size = 3.4, label.size = 0.45,
                      label.padding = grid::unit(0.16, "lines")) +
  ggplot2::facet_wrap(~record_type, ncol = 1, scales = "free_x") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::breaks_width(0.05),
                              limits = c(0, NA), expand = ggplot2::expansion(mult = c(0.01, 0.18))) +
  ggplot2::labs(title = paste("FGM Prevalence Trend by Birth Cohort,", country_name),
                subtitle = "Covariate-adjusted estimates with 95% pseudo-posterior intervals, all survey rounds pooled",
                x = "Birth cohort", y = "FGM prevalence (ever cut)") +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(text = ggplot2::element_text(face = "bold", color = "black"),
                 plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(face = "bold", size = 10.5, hjust = 0.5, color = "grey35"),
                 axis.title.x = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(t = 12)),
                 axis.title.y = ggplot2::element_text(face = "bold", size = 12, margin = ggplot2::margin(r = 10)),
                 axis.text = ggplot2::element_text(face = "bold", color = "black"),
                 axis.text.x = ggplot2::element_text(face = "bold", angle = 45, hjust = 1),
                 strip.text = ggplot2::element_text(face = "bold", size = 12),
                 strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
                 panel.grid.major = ggplot2::element_line(color = "grey82", linewidth = 0.4),
                 panel.grid.minor = ggplot2::element_blank(),
                 plot.margin = ggplot2::margin(20, 25, 20, 20))
print(cohort_trend_plot)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_prevalence_trend_by_cohort.png")), cohort_trend_plot,
                width = 10, height = 9, dpi = 300, bg = "white")
"""),
code("""
year_estimates <- function(data, outcome, population) {
  pooled_direct(data |> dplyr::filter(!is.na(birth_year)), outcome, "birth_year") |>
    dplyr::rename(prevalence = direct) |> dplyr::mutate(population = population)
}
prevalence_by_year <- dplyr::bind_rows(year_estimates(women, "fgm_self", "Women 15-49"),
                                       year_estimates(daughters, "fgm_daughter", "Girls 0-14")) |>
  dplyr::mutate(population = factor(population, levels = c("Women 15-49", "Girls 0-14")))
readr::write_csv(prevalence_by_year, file.path(dirs$trends, paste0(country_iso3, "_prevalence_by_birth_year.csv")))
prevalence_year_plot <- ggplot2::ggplot(prevalence_by_year, ggplot2::aes(birth_year, prevalence)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), fill = "grey70", alpha = 0.4) +
  ggplot2::geom_line(color = "#D9534F", linewidth = 0.9) + ggplot2::geom_point(color = "#D9534F", size = 1.6) +
  ggplot2::facet_wrap(~population, ncol = 1, scales = "free_x") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  ggplot2::labs(title = paste("FGM Prevalence by Single Birth Year,", country_name),
                subtitle = "Design-based estimates with 95% CI, all survey rounds pooled", x = "Birth year", y = "FGM prevalence (ever cut)") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                 strip.text = ggplot2::element_text(face = "bold"))
print(prevalence_year_plot)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_prevalence_by_birth_year.png")), prevalence_year_plot, width = 11, height = 8, dpi = 300, bg = "white")
"""),
md("""
## 8. Validation

Pooled model-based region estimates against the pooled design-based direct estimates.
"""),
code("""
validation <- dplyr::bind_rows(adjusted_self |> dplyr::mutate(population = "Women 15-49"),
                               adjusted_daughter |> dplyr::mutate(population = "Girls 0-14")) |>
  dplyr::group_by(population) |>
  dplyr::summarise(regions = dplyr::n(), correlation = stats::cor(model_assisted_prevalence, design_based_direct),
                   mean_abs_diff_pp = 100 * mean(abs(model_assisted_prevalence - design_based_direct)),
                   max_abs_diff_pp = 100 * max(abs(model_assisted_prevalence - design_based_direct)), .groups = "drop")
readr::write_csv(validation, file.path(dirs$validation, paste0(country_iso3, "_prevalence_model_vs_direct.csv")))
IRdisplay::display(as.data.frame(validation |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))))
"""),
md("""
## Notes

- Outcome: "ever cut" prevalence (SDG 5.3.2), not the age-specific risk of the risk notebook.
- Maps pool all rounds. The pooled region estimate averages over every respondent interviewed
  in the region across 1998–2021, with weights rescaled to mean one within each survey, so it
  describes the period as a whole rather than one year; cohort maps show change over time.
- Daughters' prevalence is not lifetime risk: many girls are not yet past the ages at which
  cutting occurs.
- Model-based estimates average predictions over each region's observed weighted composition
  and use a survey-weighted pseudo-likelihood; their intervals do not fully reproduce the
  clustered design. The direct estimates remain the design-based benchmark.
"""),
]

nb = {"cells": cells, "metadata": ref["metadata"], "nbformat": 4, "nbformat_minor": 5}
json.dump(nb, open(f"{OUT}/FGM_Prevalence_Incidence.ipynb", "w"), indent=1, ensure_ascii=False)
print("cells:", len(cells))
