import json, uuid, sys
import os
S = os.path.dirname(os.path.abspath(__file__))
POOLED = open(f"{S}/pooled_model.R").read()

V1, OUT, WHO = sys.argv[1], sys.argv[2], sys.argv[3]   # WHO = "women" or "men"
ref = json.load(open(f"{V1}/FGM_Attitudinal_Incidence.ipynb"))
men = WHO == "men"

def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]
def md(t): return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(t)}
def code(t): return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "execution_count": None, "metadata": {}, "outputs": [], "source": lines(t)}

pop = "Men 15-49" if men else "Women 15-49"
pop_lower = "men" if men else "women"
rounds_text = ("DHS 2014 and 2022 (men were not asked about FGM in DHS 1998 or 2008–09, and the DHS 2003 men's file is not available)"
               if men else "DHS 1998, 2008–09, 2014 and 2022 (DHS 2003 did not ask the attitude question)")
v1_name = "FGM_Attitudinal_Incidence.ipynb"   # Kenya V1 has no separate men's notebook
out_folder = "Attitude_Men" if men else "Attitude_Women"
data_file = "attitude_men" if men else "attitude_women"
a_priori = '"age"' if men else '"own_status", "age"'
candidates = ('"age", "education", "wealth", "religion", "ethnicity", "urban_rural", "region"' if men
              else '"own_status", "age", "education", "wealth", "religion", "ethnicity", "urban_rural", "region"')
completeness_note = "" if men else ("; own FGM status is kept if at least half of respondents have it\n"
                                     "(0.5 completeness rule), other predictors if at least 70% do")
own_status_mutate = "" if men else '\n    own_status = factor(own_status, levels = c(0, 1), labels = c("Not cut", "Cut")),'

cells = [
md(f"""
# FGM Attitudinal Incidence, {pop}, Kenya, all survey rounds (V2)

Same methodology as V1 (`V1/01_Notebooks/{v1_name}`) applied to every round with the
attitude question: {rounds_text}. Run `00_EDA_All_Rounds.ipynb` first.

The outcome is support for continuing FGM among {pop_lower} who have heard of it and
answered "continue" or "stop" ("depends" and "don't know" excluded, as in V1; DHS 1998
did not offer "depends"). Kenya has no national MICS; its subnational MICS are compared
separately.

- **Pooled data.** Predictors are screened on all rounds pooled. Direct estimates use one
  pooled survey design, and the spatial-only and covariate-adjusted BYM2 models are fitted
  once to all rounds, with a survey-round term absorbing level differences between rounds.
  Maps show pooled region estimates and, as in V1, estimates by birth cohort.
- **Trend.** Section 14 shows the national trend by survey year.
""" + ("""- **Men compared with women.** Section 15 compares men with women: pooled region maps side by
  side, and a design-based test in each round that surveyed both.
""" if men else "")),
md("## 0. Configuration"),
code(f"""
country_name <- "Kenya"
country_iso3 <- "KEN"

project_root <- if (dir.exists("../02_Data")) ".." else "."
analysis_dir <- file.path(project_root, "02_Data", "processed", country_iso3, "analysis")
output_dir <- file.path(project_root, "03_Outputs", country_iso3, "{out_folder}")
dirs <- list(
  screening = file.path(output_dir, "01_Predictor_Screening"),
  direct = file.path(output_dir, "02_Direct_Estimates"),
  models = file.path(output_dir, "03_Model_Estimates"),
  maps = file.path(output_dir, "04_Maps"),
  trends = file.path(output_dir, "05_Trends"),
  validation = file.path(output_dir, "06_Validation"){',' if men else ''}
{'  comparison = file.path(output_dir, "07_Men_vs_Women")' if men else ''}
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

shapefile_path <- file.path(project_root, "02_Data", "processed", "KEN", "boundaries", "KEN_provinces.shp")
region_var_shapefile <- "PROVINCE"
# Region keys as in the harmonized data: upper case, hyphens as spaces ("CENTRE EST").
standardize_region <- function(x) toupper(as.character(x)) |> stringr::str_replace_all("[^A-Z ]", " ") |> stringr::str_squish()
posterior_draws <- 200L
minimum_cohort_cell_n <- 10L
vif_threshold <- 2
missingness_threshold <- 0.30
round_order <- c("DHS 1998", "DHS 2003", "DHS 2008", "DHS 2014", "DHS 2022")
set.seed(20260918)
options(repr.plot.width = 14, repr.plot.height = 9, width = 200)
cat("Configured for:", country_name, "| outputs:", normalizePath(output_dir, mustWork = FALSE), "\\n")
"""),
md("## 1. Setup"),
code("""
suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(sf)
  library(spdep)
  library(INLA)
  library(randomForest)
  library(car)
})
options(survey.lonely.psu = "adjust")
theme_set(ggplot2::theme_minimal(base_size = 12))
"""),
md("## 2. Load the attitude data"),
code(f"""
attitude_all <- readRDS(file.path(analysis_dir, paste0(country_iso3, "_{data_file}.rds"))) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = intersect(round_order, unique(survey_id))))
if (!"birth_cohort" %in% names(attitude_all)) {{
  # Men's files give age only; birth year is approximated as survey year minus age.
  attitude_all <- attitude_all |>
    dplyr::mutate(birth_year = round_year - age,
                  birth_cohort = as.character(cut(birth_year, breaks = seq(1950, 2025, by = 5), right = FALSE,
                                                  labels = paste0(seq(1950, 2020, by = 5), " to ", seq(1954, 2024, by = 5)))))
}}
survey_years <- attitude_all |> dplyr::distinct(survey_id, survey_type, round_year) |> dplyr::arrange(survey_id)

IRdisplay::display(as.data.frame(attitude_all |> dplyr::group_by(survey_id) |>
  dplyr::summarise(respondents = dplyr::n(), supports_continuation = sum(supports_continuation == 1),
                   supports_stopping = sum(supports_continuation == 0),
                   unweighted_pct_continue = round(100 * mean(supports_continuation), 1), .groups = "drop")))
"""),
md(f"""
## 3. Attitude-specific predictor screening

Missingness is checked by round. Random-forest permutation importance is computed on a
pooled sample (up to 3,000 respondents per round) and used only for ranking.
{"Age" if men else "Own FGM status and age"} are retained a priori{completeness_note}.
"""),
code(f"""
candidate_predictors <- c({candidates})

missingness <- attitude_all |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(candidate_predictors), ~ round(mean(is.na(.)), 3)), .groups = "drop")
readr::write_csv(missingness, file.path(dirs$screening, paste0(country_iso3, "_attitude_missingness_by_round.csv")))
IRdisplay::display(as.data.frame(missingness))

# Own FGM status uses a 0.5 completeness rule so a strict threshold cannot drop it in older rounds.
predictor_thresholds <- ifelse(candidate_predictors == "own_status", 0.5, missingness_threshold)
eligible_predictors <- candidate_predictors[colMeans(is.na(attitude_all[candidate_predictors])) < predictor_thresholds]

rf_data <- attitude_all |>
  dplyr::group_by(survey_id) |> dplyr::slice_sample(n = 3000) |> dplyr::ungroup() |>
  dplyr::select(supports_continuation, dplyr::all_of(eligible_predictors)) |>
  tidyr::drop_na() |>
  dplyr::mutate(supports_continuation = factor(supports_continuation),{own_status_mutate}
                dplyr::across(dplyr::where(is.character), as.factor))

rf_model <- randomForest::randomForest(supports_continuation ~ ., data = rf_data, ntree = 500, importance = TRUE)
importance_ranked <- randomForest::importance(rf_model) |> as.data.frame() |>
  tibble::rownames_to_column("variable") |> dplyr::arrange(dplyr::desc(MeanDecreaseAccuracy))
cat("Rows in feature screen:", nrow(rf_data), "\\n")
print(importance_ranked)
readr::write_csv(importance_ranked, file.path(dirs$screening, paste0(country_iso3, "_attitude_feature_importance.csv")))
"""),
md("## 4. Collinearity and final predictor selection"),
code(f"""
vif_check_model <- stats::glm(stats::reformulate(eligible_predictors, response = "supports_continuation"),
                              data = rf_data, family = stats::binomial())
vif_raw <- car::vif(vif_check_model)
vif_adjusted <- if (is.matrix(vif_raw)) vif_raw[, "GVIF^(1/(2*Df))"] else sqrt(vif_raw)
vif_table <- tibble::tibble(variable = names(vif_adjusted), adjusted_vif = as.numeric(vif_adjusted)) |>
  dplyr::arrange(dplyr::desc(adjusted_vif))
print(vif_table)
vif_flagged <- vif_table$variable[vif_table$adjusted_vif > vif_threshold]
selected_predictors <- unique(c({a_priori}, setdiff(importance_ranked$variable, vif_flagged)))
selected_predictors <- selected_predictors[selected_predictors %in% eligible_predictors]
cat("Final attitude predictor list:", paste(selected_predictors, collapse = ", "), "\\n")
cat("Region is represented by the BYM2 effect rather than a fixed effect.\\n")
readr::write_csv(vif_table, file.path(dirs$screening, paste0(country_iso3, "_attitude_vif.csv")))
readr::write_csv(tibble::tibble(variable = selected_predictors),
                 file.path(dirs$screening, paste0(country_iso3, "_attitude_selected_predictors.csv")))
"""),
md("""
## 5. Mixed-variable association matrix

Absolute Spearman correlation (numeric pairs), bias-corrected Cramér's V (categorical
pairs) and the correlation ratio (mixed pairs), on the pooled screening sample.
"""),
code("""
cramers_v_corrected <- function(x, y) {
  keep <- stats::complete.cases(x, y); x <- droplevels(as.factor(x[keep])); y <- droplevels(as.factor(y[keep]))
  if (length(x) == 0L || nlevels(x) < 2L || nlevels(y) < 2L) return(NA_real_)
  tab <- table(x, y); chi2 <- suppressWarnings(as.numeric(stats::chisq.test(tab, correct = FALSE)$statistic))
  n <- sum(tab); r <- nrow(tab); k <- ncol(tab)
  phi2c <- max(0, chi2 / n - ((k - 1) * (r - 1)) / (n - 1))
  den <- min(r - ((r - 1)^2) / (n - 1) - 1, k - ((k - 1)^2) / (n - 1) - 1)
  if (!is.finite(den) || den <= 0) return(NA_real_)
  sqrt(phi2c / den)
}
correlation_ratio <- function(x_numeric, y_category) {
  keep <- stats::complete.cases(x_numeric, y_category)
  x <- as.numeric(x_numeric[keep]); y <- droplevels(as.factor(y_category[keep]))
  if (length(unique(x)) < 2L || nlevels(y) < 2L) return(NA_real_)
  between <- sum(vapply(split(x, y), function(z) length(z) * (mean(z) - mean(x))^2, numeric(1L)))
  sqrt(max(0, min(1, between / sum((x - mean(x))^2))))
}
mixed_association <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) return(abs(stats::cor(x, y, method = "spearman", use = "complete.obs")))
  if (!is.numeric(x) && !is.numeric(y)) return(cramers_v_corrected(x, y))
  if (is.numeric(x)) correlation_ratio(x, y) else correlation_ratio(y, x)
}
association_data <- rf_data |> dplyr::mutate(supports_continuation = factor(supports_continuation, labels = c("Stop", "Continue")))
association_names <- names(association_data)
association_matrix <- outer(seq_along(association_names), seq_along(association_names), Vectorize(function(i, j)
  if (i == j) 1 else mixed_association(association_data[[i]], association_data[[j]])))
dimnames(association_matrix) <- list(association_names, association_names)
association_long <- as.data.frame(association_matrix) |> tibble::rownames_to_column("variable_y") |>
  tidyr::pivot_longer(-variable_y, names_to = "variable_x", values_to = "association") |>
  dplyr::mutate(variable_x = factor(variable_x, levels = association_names),
                variable_y = factor(variable_y, levels = rev(association_names)))
association_plot <- ggplot2::ggplot(association_long, ggplot2::aes(variable_x, variable_y, fill = association)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", association), color = association >= 0.55), fontface = "bold", size = 3.4) +
  ggplot2::scale_fill_gradientn(colors = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"), limits = c(0, 1),
                                name = "Association\\nstrength", na.value = "grey85") +
  ggplot2::scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "white"), guide = "none") +
  ggplot2::coord_fixed() +
  ggplot2::labs(title = paste("FGM Attitude and Predictor Association Matrix, all rounds,", country_name), x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(text = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(hjust = 0.5, size = 15),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), panel.grid = ggplot2::element_blank())
print(association_plot)
ggplot2::ggsave(file.path(dirs$screening, paste0(country_iso3, "_attitude_association_matrix.png")), association_plot,
                width = 10, height = 8, dpi = 300, bg = "white")
"""),
md("""
## 6. Design-based direct estimates (pooled)

One pooled survey design across all rounds (each survey keeps its own clusters and strata):
pooled estimates by region, and national estimates by survey round for the trend in
section 14. These remain the design-based benchmark.
"""),
code(POOLED),
code("""
design_for <- function(d) survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
direct_by_region <- pooled_direct(attitude_all, "supports_continuation", "region") |>
  dplyr::rename(direct_support = direct, direct_se = se, direct_lower = lower_ci, direct_upper = upper_ci)
direct_national <- pooled_direct(attitude_all |> dplyr::mutate(survey_id = as.character(survey_id)), "supports_continuation", "survey_id") |>
  dplyr::rename(estimate = direct) |> dplyr::mutate(method = "Design-based direct")
pooled_national_direct <- survey::svymean(~supports_continuation, design_for(attitude_all))
readr::write_csv(direct_national, file.path(dirs$direct, paste0(country_iso3, "_attitude_direct_national_by_round.csv")))
readr::write_csv(direct_by_region, file.path(dirs$direct, paste0(country_iso3, "_attitude_direct_by_region.csv")))
cat("Pooled national support for continuation:", round(100 * as.numeric(stats::coef(pooled_national_direct)), 1), "%\\n")
IRdisplay::display(as.data.frame(direct_national |> dplyr::mutate(dplyr::across(c(estimate, lower_ci, upper_ci), ~ round(100 * .x, 1)))))
"""),
md("## 7. Region boundaries and BYM2 adjacency"),
code("""
boundaries <- sf::st_read(shapefile_path, quiet = TRUE) |> sf::st_make_valid()
region_keys <- standardize_region(boundaries[[region_var_shapefile]])
adjacency <- spdep::poly2nb(boundaries, row.names = region_keys, queen = TRUE)
adjacency_matrix <- spdep::nb2mat(adjacency, style = "B", zero.policy = TRUE)
stopifnot(all(unique(attitude_all$region) %in% region_keys))
cat("Boundary regions:", nrow(boundaries), "\\n")
"""),
md("""
## 8–10. Survey-weighted spatial models and posterior estimates (pooled)

As in V1, a spatial-only benchmark and a covariate-adjusted model (the selected predictors
as fixed effects, age as RW2, region as BYM2), each fitted once to all rounds with
normalized survey weights; the adjusted model also has a survey-round term. Predictors a
round did not collect are kept as an "unknown" category. For each posterior draw, fitted
probabilities are averaged with the survey weights within region, region × birth cohort,
survey round and nationally. Intervals are pseudo-posterior.
"""),
code("""
fixed_covariates <- setdiff(selected_predictors, c("region", "age"))
model_input <- attitude_all
if ("own_status" %in% names(model_input)) {
  model_input <- model_input |>
    dplyr::mutate(own_status = dplyr::case_when(own_status == 1 ~ "Cut", own_status == 0 ~ "Not cut", TRUE ~ NA_character_))
}
grouped <- prepare_pooled(model_input, "supports_continuation", fixed_covariates)
t0 <- Sys.time()
spatial_model <- fit_pooled(grouped, "supports_continuation", fixed_covariates, spatial_only = TRUE, seed = 20260918L)
adjusted_model <- fit_pooled(grouped, "supports_continuation", fixed_covariates, seed = 20260919L)
cat("Pooled models fitted in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min; fixed effects:",
    paste(c("survey round", fixed_covariates), collapse = ", "), "\\n")

adjusted_state <- adjusted_model$state |>
  dplyr::rename(adjusted_support = estimate, adjusted_lower = lower_ci, adjusted_upper = upper_ci) |>
  dplyr::left_join(spatial_model$state |> dplyr::select(region, spatial_only = estimate), by = "region") |>
  dplyr::left_join(direct_by_region, by = "region")
adjusted_cohort <- adjusted_model$state_cohort |> dplyr::filter(n_unweighted >= minimum_cohort_cell_n) |>
  dplyr::rename(adjusted_support = estimate)
national_comparison <- dplyr::bind_rows(
  direct_national |> dplyr::select(survey_id, method, estimate, lower_ci, upper_ci),
  adjusted_model$survey |> dplyr::transmute(survey_id, method = "Covariate-adjusted spatial", estimate, lower_ci, upper_ci),
  spatial_model$survey |> dplyr::transmute(survey_id, method = "Spatial only", estimate, lower_ci, upper_ci))
model_fit_comparison <- dplyr::bind_rows(spatial_model$fit_stats |> dplyr::mutate(model = "Spatial only"),
                                         adjusted_model$fit_stats |> dplyr::mutate(model = "Covariate adjusted"))
readr::write_csv(adjusted_state, file.path(dirs$models, paste0(country_iso3, "_attitude_covariate_adjusted_by_region.csv")))
readr::write_csv(adjusted_cohort, file.path(dirs$models, paste0(country_iso3, "_attitude_covariate_adjusted_by_region_cohort.csv")))
readr::write_csv(national_comparison, file.path(dirs$models, paste0(country_iso3, "_attitude_national_comparison_by_round.csv")))
readr::write_csv(model_fit_comparison, file.path(dirs$models, paste0(country_iso3, "_attitude_model_fit.csv")))
readr::write_csv(adjusted_model$fixed, file.path(dirs$models, paste0(country_iso3, "_attitude_fixed_effects.csv")))
readr::write_csv(adjusted_model$residual, file.path(dirs$models, paste0(country_iso3, "_attitude_residual_spatial_effect.csv")))
IRdisplay::display(as.data.frame(model_fit_comparison))
cat("Pooled national support (model):", round(100 * adjusted_model$national$estimate, 1), "%\\n")
"""),
md("## 11. Covariate-adjusted attitude maps (pooled)"),
code(f"""
support_fill <- c(0, max(adjusted_state$adjusted_support, adjusted_cohort$adjusted_support))
adjusted_attitude_map <- state_map(adjusted_state, "adjusted_support",
  paste("{"Men's" if men else "Women's"} Attitude towards FGM Continuation,", country_name),
  "{pop}, all survey rounds pooled (survey-weighted covariate-adjusted BYM2)", "Adjusted support\\nfor continuation", support_fill)
cohort_attitude_map <- cohort_map(adjusted_cohort, "adjusted_support",
  paste("{"Men's" if men else "Women's"} Attitude towards FGM Continuation by Birth Cohort,", country_name),
  "{pop}, all survey rounds pooled", "Adjusted support\\nfor continuation", support_fill)
print(adjusted_attitude_map); print(cohort_attitude_map)
ggplot2::ggsave(file.path(dirs$maps, paste0(country_iso3, "_attitude_covariate_adjusted_map.png")), adjusted_attitude_map,
                width = 9, height = 8, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$maps, paste0(country_iso3, "_attitude_covariate_adjusted_map_by_cohort.png")), cohort_attitude_map,
                width = 14, height = 12, dpi = 300, bg = "white")
"""),
md("""
## 12. Residual geographic pattern (pooled)

The BYM2 effect after adjustment for composition and survey round (relative log-odds, not
an attitude prevalence).
"""),
code("""
attitude_residual_map <- residual_map(adjusted_model$residual, paste("Residual Geographic Pattern in FGM Attitudes,", country_name),
                                      "All survey rounds pooled")
print(attitude_residual_map)
ggplot2::ggsave(file.path(dirs$maps, paste0(country_iso3, "_attitude_residual_map.png")), attitude_residual_map,
                width = 9, height = 8, dpi = 300, bg = "white")
"""),
md("## 13. Validation and comparison"),
code("""
validation_metrics <- adjusted_state |>
  dplyr::summarise(states_compared = sum(stats::complete.cases(direct_support, adjusted_support)),
                   correlation_direct_adjusted = stats::cor(direct_support, adjusted_support, use = "complete.obs"),
                   mean_absolute_difference = mean(abs(adjusted_support - direct_support), na.rm = TRUE),
                   maximum_absolute_difference = max(abs(adjusted_support - direct_support), na.rm = TRUE))
readr::write_csv(validation_metrics, file.path(dirs$validation, paste0(country_iso3, "_attitude_validation_metrics.csv")))
IRdisplay::display(as.data.frame(validation_metrics |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))))
validation_plot <- ggplot2::ggplot(adjusted_state, ggplot2::aes(direct_support, adjusted_support)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_point(color = "#1D4E89", size = 2.4) +
  ggrepel::geom_text_repel(ggplot2::aes(label = region), size = 2.8, seed = 1, max.overlaps = 15) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(title = paste("Direct vs Model-assisted Region Estimates, all rounds pooled,", country_name),
                x = "Design-based direct", y = "Covariate-adjusted model") +
  ggplot2::theme_bw(base_size = 12) + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
print(validation_plot)
ggplot2::ggsave(file.path(dirs$validation, paste0(country_iso3, "_attitude_validation.png")), validation_plot,
                width = 9, height = 8, dpi = 300, bg = "white")
"""),
md(f"""
## 14. National trend across survey rounds

Support for continuation among {pop_lower} who know of FGM, by survey year: design-based
estimates and the pooled model's estimates for each round.
"""),
code(f"""
trend_data <- national_comparison |>
  dplyr::left_join(survey_years |> dplyr::mutate(survey_id = as.character(survey_id)), by = "survey_id") |>
  dplyr::filter(method %in% c("Design-based direct", "Covariate-adjusted spatial"))
readr::write_csv(trend_data, file.path(dirs$trends, paste0(country_iso3, "_attitude_national_trend.csv")))
trend_plot <- ggplot2::ggplot(trend_data, ggplot2::aes(round_year, estimate, color = method, shape = survey_type)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), width = 0.35, position = ggplot2::position_dodge(0.6)) +
  ggplot2::geom_point(size = 3.2, position = ggplot2::position_dodge(0.6)) +
  ggrepel::geom_text_repel(data = trend_data |> dplyr::filter(method == "Design-based direct"),
                           ggplot2::aes(label = scales::percent(estimate, accuracy = 0.1)), size = 3.3, seed = 1, show.legend = FALSE) +
  ggplot2::scale_color_manual(values = c("Design-based direct" = "grey30", "Covariate-adjusted spatial" = "#1D4E89")) +
  ggplot2::scale_x_continuous(breaks = sort(unique(trend_data$round_year))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  ggplot2::labs(title = paste("Support for Continuing FGM by Survey Year, {pop},", country_name),
                subtitle = "Among those who have heard of FGM and answered continue or stop; 95% intervals",
                x = "Survey year", y = "Support for continuation", color = NULL, shape = "Survey") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35"), legend.position = "bottom")
print(trend_plot)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_attitude_national_trend.png")), trend_plot, width = 11, height = 7, dpi = 300, bg = "white")
"""),
]

if men:
    cells += [
md("""
## 15. Men compared with women

Pooled region estimates for men and women side by side (each pooled over the rounds that
asked them), and, for each round that surveyed both sexes, a design-based test of the
national men–women difference in one stacked design (each sex keeps its own weights; both
share the DHS clusters and strata).
"""),
code("""
women_attitude <- readRDS(file.path(analysis_dir, paste0(country_iso3, "_attitude_women.rds")))
women_state_path <- file.path(project_root, "03_Outputs", country_iso3, "Attitude_Women", "03_Model_Estimates",
                              paste0(country_iso3, "_attitude_covariate_adjusted_by_region.csv"))
if (!file.exists(women_state_path)) stop("Run Part A (women) first: ", women_state_path)

shared_rounds <- intersect(levels(attitude_all$survey_id), unique(women_attitude$survey_id))
men_vs_women <- purrr::map_dfr(shared_rounds, function(r) {
  stacked <- dplyr::bind_rows(
    women_attitude |> dplyr::filter(survey_id == r) |> dplyr::transmute(sex = "Women", cluster, strata, weight, supports_continuation),
    attitude_all |> dplyr::filter(survey_id == r) |> dplyr::transmute(sex = "Men", cluster, strata, weight, supports_continuation))
  by_sex <- survey::svyby(~supports_continuation, ~sex, design_for(stacked), survey::svymean, covmat = TRUE)
  diff <- survey::svycontrast(by_sex, c(Men = 1, Women = -1))
  tibble::tibble(survey_id = r, men = as.numeric(stats::coef(by_sex)["Men"]), women = as.numeric(stats::coef(by_sex)["Women"]),
                 difference = as.numeric(stats::coef(diff)), se = as.numeric(survey::SE(diff))) |>
    dplyr::mutate(lower_ci = difference - 1.96 * se, upper_ci = difference + 1.96 * se, p_value = 2 * stats::pnorm(-abs(difference / se)))
})
readr::write_csv(men_vs_women, file.path(dirs$comparison, paste0(country_iso3, "_attitude_national_men_vs_women_by_round.csv")))
IRdisplay::display(as.data.frame(men_vs_women |> dplyr::mutate(dplyr::across(c(men, women, difference, lower_ci, upper_ci), ~ round(100 * .x, 1)),
                                                               p_value = signif(p_value, 3))))

women_state <- readr::read_csv(women_state_path, show_col_types = FALSE)
state_by_sex <- dplyr::bind_rows(
  women_state |> dplyr::transmute(region, sex = "Women 15-49", adjusted_support),
  adjusted_state |> dplyr::transmute(region, sex = "Men 15-49", adjusted_support)) |>
  dplyr::mutate(sex = factor(sex, levels = c("Women 15-49", "Men 15-49")))
state_wide <- state_by_sex |> tidyr::pivot_wider(names_from = sex, values_from = adjusted_support) |>
  dplyr::mutate(men_minus_women = `Men 15-49` - `Women 15-49`) |> dplyr::arrange(dplyr::desc(men_minus_women))
readr::write_csv(state_wide, file.path(dirs$comparison, paste0(country_iso3, "_attitude_region_men_vs_women.csv")))
cat("Regions compared:", nrow(state_wide), "| correlation men vs women:",
    round(stats::cor(state_wide$`Men 15-49`, state_wide$`Women 15-49`), 2),
    "| regions where men are higher:", sum(state_wide$men_minus_women > 0), "\\n")

men_women_map <- ggplot2::ggplot(boundaries |> dplyr::mutate(region = region_keys) |>
    dplyr::left_join(state_by_sex, by = "region", relationship = "one-to-many")) +
  ggplot2::geom_sf(ggplot2::aes(fill = adjusted_support), color = "grey40", linewidth = 0.15) +
  ggplot2::facet_wrap(~sex, ncol = 2) +
  ggplot2::scale_fill_viridis_c(name = "Adjusted support\\nfor continuation", option = "magma", direction = -1,
                                labels = scales::percent_format(accuracy = 1), limits = c(0, max(state_by_sex$adjusted_support, na.rm = TRUE)),
                                oob = scales::squish, na.value = "grey90") +
  ggplot2::labs(title = paste("Support for FGM Continuation, Women and Men,", country_name),
                subtitle = "All survey rounds pooled", x = NULL, y = NULL) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(text = ggplot2::element_text(face = "bold"), plot.title = ggplot2::element_text(size = 18, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35", face = "plain"),
                 strip.text = ggplot2::element_text(size = 12), strip.background = ggplot2::element_rect(fill = "grey90", color = "grey50"),
                 axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
print(men_women_map)
ggplot2::ggsave(file.path(dirs$comparison, paste0(country_iso3, "_attitude_map_men_vs_women.png")), men_women_map,
                width = 15, height = 8, dpi = 300, bg = "white")
"""),
    ]

cells += [
md(f"""
## Interpretation and limitations

- The design-based national and region estimates are the survey-design benchmark.
- Models pool all rounds with normalized survey weights in a pseudo-likelihood and a
  survey-round term; their intervals are pseudo-posterior, not fully design-based. Pooled
  provincial estimates describe 1998–2022 as a whole; cohort maps and the trend show change.
- Estimates apply to {pop_lower} aged 15–49 who have heard of FGM and gave a continue/stop
  answer.
- Predictors a round does not collect (wealth in DHS 1998) are kept as an "unknown" category
  in the pooled model.
- DHS 1998 offered only "continue", "discontinue" and "don't know", with no "depends" option, so
  some respondents who would otherwise have said "depends" chose one of the two answers.
- Regions are the 8 former provinces; DHS 2022 counties are mapped to their province.
- Random-forest importance is descriptive and ranks predictors without implying causality.
"""),
code("""
cat("Attitude analysis completed. Selected predictors:", paste(selected_predictors, collapse = ", "), "\\n")
cat("All outputs saved to:", normalizePath(output_dir), "\\n")
"""),
]

nb = {"cells": cells, "metadata": ref["metadata"], "nbformat": 4, "nbformat_minor": 5}
name = "FGM_Attitudinal_Incidence_Men.ipynb" if men else "FGM_Attitudinal_Incidence.ipynb"
json.dump(nb, open(f"{OUT}/{name}", "w"), indent=1, ensure_ascii=False)
print(name, "cells:", len(cells))
