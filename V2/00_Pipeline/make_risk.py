import json, uuid, sys

S, V1, OUT = sys.argv[1], sys.argv[2], sys.argv[3]
v1 = json.load(open(f"{V1}/FGM_Risk_Incidence.ipynb"))
core = open(f"{S}/km_core.R").read().split("\n", 1)[1]

# Kenya's V1 risk notebook predates the covariate-adjusted maps (12A); those V1 cells, which
# are country-independent, are taken from the Burkina Faso V1 notebook (read only).
v1_ref = json.load(open(f"{V1}/../../../Bukina Fasso/V1/01_Notebooks/FGM_Risk_Incidence.ipynb"))
def src(needle):
    hits = [c for c in v1["cells"] if needle in "".join(c["source"])]
    if not hits:
        hits = [c for c in v1_ref["cells"] if needle in "".join(c["source"])]
    assert len(hits) == 1, (needle, len(hits))
    return "".join(hits[0]["source"])
def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]
def md(t): return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(t)}
def code(t): return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "execution_count": None, "metadata": {}, "outputs": [], "source": lines(t)}
def patch(text, pairs):
    for a, b in pairs:
        # Kenya V1 already matches regions with boundary_region_keys; that patch is then not needed.
        if text.count(a) == 0 and a.startswith("match(region, toupper(") and "boundary_region_keys" in text:
            continue
        assert text.count(a) >= 1, a[:80]
        text = text.replace(a, b)
    return text

# V1 spatial, provisional, map and 12A cells, redirected to the V2 folders.
spatial_cell = patch(src("if (run_spatial_model) {\n  boundaries <- sf::st_read"), [
    ('file.path(output_dir, paste0(country_iso3, "_smoothed_cohort_age_band_self.csv"))',
     'file.path(dirs$direct, paste0(country_iso3, "_smoothed_cohort_age_band_self.csv"))'),
    ('file.path(output_dir, paste0(country_iso3, "_smoothed_cohort_age_band_daughter.csv"))',
     'file.path(dirs$direct, paste0(country_iso3, "_smoothed_cohort_age_band_daughter.csv"))'),
    # Harmonized region keys use spaces ("CENTRE EST"); match them to the standardized boundary names.
    ('match(region, toupper(boundaries[[region_var_shapefile]]))', 'match(region, boundary_region_keys)')])
provisional_cell = patch(src("# Provisional spatial map"), [
    ('match(region, toupper(boundaries[[region_var_shapefile]]))', 'match(region, boundary_region_keys)')])
map_cell = patch(src("make_cohort_map_for_band <- function"), [('file.path(output_dir, paste0(country_iso3, "_map_ages_"',
                            'file.path(dirs$maps, paste0(country_iso3, "_map_ages_"')])
helpers_v1 = src("prepare_geo_covariate_risk <- function")
start = helpers_v1.index("prepare_geo_covariate_risk <- function")
end = helpers_v1.index("geo_covariate_band_risk <- function")
new_prepare = r'''prepare_geo_covariate_risk <- function(events, py_cells, targets, region_keys) {
  covariates <- c("ethnicity", "urban_rural")
  people <- events |>
    dplyr::mutate(region = standardize_region(region),
                  birth_cohort = as.character(birth_cohort)) |>
    dplyr::filter(is.finite(weight), weight > 0) |>
    tidyr::drop_na(dplyr::all_of(covariates), region, birth_cohort)
  if (anyDuplicated(people$id)) stop("Duplicate IDs in geospatial risk input")
  if (!nrow(people)) stop("No complete records for geospatial risk")
  if (any(!people$region %in% region_keys)) stop("Unmatched geospatial risk region")
  cohort_levels <- sort(unique(people$birth_cohort))
  cohort_years <- as.integer(substr(cohort_levels, 1, 4))
  people <- people |>
    dplyr::mutate(region_id = match(region, region_keys),
                  cohort_id = match(birth_cohort, cohort_levels))
  for (v in covariates) people[[v]] <- factor(people[[v]])
  active_covariates <- covariates[vapply(people[covariates], nlevels, integer(1)) > 1L]
  if (length(cohort_levels) < 3L) stop("At least three cohorts are required for RW2")

  # Person-years arrive already aggregated within cluster; each cell splits into an
  # event row and a non-event row. Identical predictors and outcomes share one
  # Bernoulli likelihood, so summing their normalized weights is exact.
  cells <- py_cells |>
    dplyr::mutate(region = standardize_region(region), birth_cohort = as.character(birth_cohort)) |>
    dplyr::filter(!is.na(ethnicity), !is.na(urban_rural), birth_cohort %in% cohort_levels)
  mean_weight <- sum(cells$w_risk) / sum(cells$n_risk)
  train <- dplyr::bind_rows(
    cells |> dplyr::transmute(region_id = match(region, region_keys), cohort_id = match(birth_cohort, cohort_levels),
                              age_year, ethnicity, urban_rural, event = 1L, pseudo_weight = w_event / mean_weight),
    cells |> dplyr::transmute(region_id = match(region, region_keys), cohort_id = match(birth_cohort, cohort_levels),
                              age_year, ethnicity, urban_rural, event = 0L,
                              pseudo_weight = (w_risk - w_event) / mean_weight)
  ) |>
    dplyr::filter(pseudo_weight > 0) |>
    dplyr::group_by(region_id, cohort_id, age_year, ethnicity, urban_rural, event) |>
    dplyr::summarise(pseudo_weight = sum(pseudo_weight), .groups = "drop")

  profiles <- people |>
    dplyr::group_by(region, birth_cohort, region_id, cohort_id, ethnicity, urban_rural) |>
    dplyr::summarise(profile_weight = sum(weight), .groups = "drop") |>
    dplyr::mutate(profile_id = dplyr::row_number())
  support <- people |>
    dplyr::group_by(region, birth_cohort) |>
    dplyr::summarise(complete_case_followup = max(time), n_complete = dplyr::n(),
                     .groups = "drop")
  targets <- targets |>
    dplyr::mutate(region = standardize_region(region),
                  birth_cohort = as.character(birth_cohort),
                  start_age = as.integer(sub("-.*", "", age_band)),
                  end_age = as.integer(sub(".*-", "", age_band))) |>
    dplyr::left_join(support, by = c("region", "birth_cohort")) |>
    dplyr::mutate(stop_age = pmin(observed_to_age, end_age),
                  supported = is.finite(complete_case_followup) &
                    is.finite(stop_age) & stop_age > start_age &
                    stop_age <= complete_case_followup,
                  target_id = dplyr::row_number())
  profiles <- profiles |>
    dplyr::inner_join(targets |> dplyr::filter(supported) |>
                       dplyr::group_by(region, birth_cohort) |>
                       dplyr::summarise(prediction_end = max(stop_age), .groups = "drop"),
                     by = c("region", "birth_cohort"))
  if (!nrow(profiles)) stop("No supported region-cohort profiles for prediction")
  prediction <- profiles |>
    tidyr::uncount(weights = as.integer(prediction_end) + 1L, .id = "age_index") |>
    dplyr::mutate(age_year = age_index - 1L, event = NA_integer_, pseudo_weight = 1) |>
    dplyr::arrange(profile_id, age_year)
  model_data <- dplyr::bind_rows(train, prediction |> dplyr::select(dplyr::all_of(names(train))))
  for (v in covariates) {
    model_data[[v]] <- factor(model_data[[v]], levels = levels(people[[v]]))
  }
  list(people = people, train = train, prediction = prediction,
       model_data = model_data, targets = targets, active_covariates = active_covariates,
       cohort_years = cohort_years, excluded = nrow(events) - nrow(people),
       annual_n = sum(cells$n_risk))
}

'''
helpers_cell = helpers_v1[:start] + new_prepare + helpers_v1[end:]
run12a_cell = patch(src("geo_cov_required <- c("), [
    ('geo_cov_output_dir <- file.path(output_dir, "Geospatial_Covariates")', 'geo_cov_output_dir <- dirs$covariate'),
    ('person_time |> dplyr::filter(record_type == geo_cov_source),', 'py_covariate[[geo_cov_source]],')])
map12a_cell = src("for (geo_cov_band in age_band_levels)")

cells = [
md("""
# FGM Risk Incidence, Kenya, all survey rounds (V2)

Same analysis as V1 (`V1/01_Notebooks/FGM_Risk_Incidence.ipynb`, DHS 2022 only) applied to
every round with women's age at cutting: DHS 1998, 2008–09, 2014 and 2022 for women's own
histories, and the two full-roster rounds (DHS 2014, 2022) for daughters. DHS 2003 did not ask
age at cutting and Kenya's MICS are subnational, so risk incidence uses these four DHS rounds.
Run `00_EDA_All_Rounds.ipynb` first.

In Kenya most cutting happens at ages 5–15 (only 2–4% of cut women report cutting before
age 5), so the by-round trend and the sensitivity checks use the risk of being cut **by age
15** for women's own reports and **by age 10** when comparing with mothers' reports for
daughters, who are observed only to age 14.

Differences from V1, besides the added rounds:

1. **Definitions.** Cutting "during infancy" is age 0, and never-heard women and their
   daughters are uncut (see the EDA notebook).
2. **Pooling.** Rounds are pooled by birth cohort. Each survey keeps its own clusters and
   strata, and weights are rescaled to mean one within each survey.
3. **Computation.** Person-years are aggregated within survey cluster before modelling,
   which gives the same estimates and design-based variances as one row per person-year.
4. **Kaplan–Meier throughout.** Direct regional estimates use Kaplan–Meier point estimates
   with linearized standard errors; V1 used the Aalen estimator for direct cells.
5. **By-round check.** Section 13B compares the cohort trend between rounds.
6. **Age at cutting not reported exactly.** DHS "during infancy" answers are spread over
   ages 0–4, and "don't know", missing or inconsistent ages over ages 0 to the current age,
   using the age distribution of exact reporters in the same survey and birth cohort (see
   the EDA notebook, section 7.1). Such cases appear as fractional records (`age_share`)
   that keep the respondent's survey weight; nothing is dropped except DHS 2003 women,
   whose survey did not ask age at cutting.
"""),
md("## 0. Configuration"),
code("""
country_name <- "Kenya"
country_iso3 <- "KEN"

project_root <- if (dir.exists("../02_Data")) ".." else "."
analysis_dir <- file.path(project_root, "02_Data", "processed", country_iso3, "analysis")
output_dir <- file.path(project_root, "03_Outputs", country_iso3, "Risk")
dirs <- list(
  km = file.path(output_dir, "01_Kaplan_Meier"),
  hazard = file.path(output_dir, "02_Hazard_Model"),
  direct = file.path(output_dir, "03_Direct_and_Smoothed_Estimates"),
  maps = file.path(output_dir, "04_Risk_Maps"),
  covariate = file.path(output_dir, "05_Covariate_Adjusted_Maps"),
  trends = file.path(output_dir, "06_Trends"),
  validation = file.path(output_dir, "07_Validation"),
  sensitivity = file.path(output_dir, "08_Sensitivity_Analysis")
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

shapefile_path <- file.path(project_root, "02_Data", "processed", "KEN", "boundaries", "KEN_provinces.shp")
region_var_shapefile <- "PROVINCE"
# Region keys as in the harmonized data: upper case, hyphens as spaces ("CENTRE EST").
standardize_region <- function(x) toupper(as.character(x)) |> stringr::str_replace_all("[^A-Z ]", " ") |> stringr::str_squish()

max_age_years <- 49L
age_band_cutpoints <- c(0, 5, 10, 15, 20, 25)
age_band_levels <- paste0(head(age_band_cutpoints, -1), "-", age_band_cutpoints[-1])

run_spatial_model <- file.exists(shapefile_path)
run_change_point <- TRUE
run_ml_crosscheck <- TRUE

options(repr.plot.width = 13, repr.plot.height = 8, width = 200)
cat("Configured for:", country_name, "| outputs:", normalizePath(output_dir, mustWork = FALSE), "\\n")
"""),
md("## 1. Setup and packages"),
code("""
suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(survival)
  library(data.table)
  library(sf)
  library(spdep)
  library(mgcv)
})
options(survey.lonely.psu = "adjust")
set.seed(20260101)
"""),
md("""
## 2. Load prepared data

`KEN_fgm_events.rds` holds one time-to-event record per woman (own history) or daughter
(mother's report): `fgm_event` is 1 if cut and `time` is the age at cutting, otherwise
`time` is the current age (women capped at 49).
"""),
code("""
fgm_events <- readRDS(file.path(analysis_dir, paste0(country_iso3, "_fgm_events.rds"))) |>
  dplyr::mutate(birth_cohort = factor(birth_cohort, levels = sort(unique(birth_cohort))),
                survey_id = factor(survey_id, levels = unique(survey_id[order(survey_type, round_year)])))

cat("Records:", nrow(fgm_events), "\\n")
IRdisplay::display(as.data.frame(
  fgm_events |> dplyr::count(record_type, survey_id, wt = age_share) |>
    tidyr::pivot_wider(names_from = record_type, values_from = n, values_fill = 0)))
cat("Histories:", round(sum(fgm_events$age_share)), "| records (fractional rows for estimated ages):", nrow(fgm_events), "\n")
"""),
md("""
## 3. Cohort coverage across rounds

Records by birth cohort and round. Pooling uses every round in which a cohort appears;
older cohorts come only from early rounds and the youngest cohorts only from late rounds.
"""),
code("""
coverage <- fgm_events |>
  dplyr::count(record_type, birth_cohort, survey_id, wt = age_share) |>
  dplyr::mutate(n = round(n)) |>
  tidyr::pivot_wider(names_from = survey_id, values_from = n, values_fill = 0) |>
  dplyr::arrange(record_type, birth_cohort)
readr::write_csv(coverage, file.path(dirs$km, paste0(country_iso3, "_records_by_cohort_and_round.csv")))
IRdisplay::display(as.data.frame(coverage))
"""),
md("## 4. The FGM event"),
code("""
IRdisplay::display(as.data.frame(fgm_events |> dplyr::count(record_type, fgm_event, age_source, wt = age_share) |>
  dplyr::mutate(n = round(n))))
"""),
md("""
## 5. Person-years and design-based Kaplan–Meier

Each record contributes one person-year per completed year of life up to its event or
censoring age. Person-years are summed within survey cluster and analysis cell; the
Kaplan–Meier survival is the product of weighted annual hazards, and its standard error
is the Taylor linearization from cluster totals (validated against `survey::svyby` with
`svycontrast` and against `svykm()` during development; identical to machine precision).
"""),
code(core),
code("""
design_psus <- lapply(split(fgm_events, fgm_events$record_type), function(d)
  data.table::as.data.table(d)[, .(cluster = unique(cluster)), by = strata][, .(n_h = .N), by = strata])

py_cohort <- lapply(split(fgm_events, fgm_events$record_type), aggregate_person_years,
                    group_vars = c("region", "birth_cohort", "survey_id", "survey_type"), max_age = max_age_years)
cat("Aggregated person-year cells:", vapply(py_cohort, nrow, integer(1)), "\\n")
cat("Person-years represented:", vapply(py_cohort, function(p) sum(p$n_risk), numeric(1)), "\\n")
"""),
md("""
## 6. Kaplan–Meier baseline

Survey-weighted Kaplan–Meier curves by birth cohort, pooling all rounds. Each curve stops
at the oldest age observed for that cohort.
"""),
code("""
build_km_plot <- function(plot_data, subtitle_label, xlim_max, break_by, file_suffix, y_min) {
  plot_data <- plot_data |> dplyr::mutate(birth_cohort = droplevels(birth_cohort))
  km_fit <- survival::survfit(survival::Surv(time, fgm_event) ~ birth_cohort, data = plot_data, weights = weight)
  km_tidy <- broom::tidy(km_fit) |>
    dplyr::mutate(birth_cohort = stringr::str_remove(strata, "^birth_cohort="))
  n_cohorts <- nlevels(plot_data$birth_cohort)
  p <- ggplot2::ggplot(km_tidy, ggplot2::aes(time, estimate, color = birth_cohort, fill = birth_cohort)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high), alpha = 0.10, color = NA) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = viridisLite::viridis(n_cohorts, begin = 0.05, end = 0.95)) +
    ggplot2::scale_fill_manual(values = viridisLite::viridis(n_cohorts, begin = 0.05, end = 0.95), guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(0, xlim_max, by = break_by), limits = c(0, xlim_max)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(y_min, 1)) +
    ggplot2::labs(title = paste("Probability of Remaining FGM-Free by Birth Cohort,", country_name),
                  subtitle = paste0(subtitle_label, ", all survey rounds pooled (shaded: 95% CI)"),
                  x = "Age of girl or woman", y = "Probability of remaining FGM-free", color = "Birth cohort") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35", margin = ggplot2::margin(b = 10)),
                   axis.title = ggplot2::element_text(face = "bold"),
                   axis.text = ggplot2::element_text(face = "bold", color = "grey25"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom", legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, override.aes = list(linewidth = 1.6)))
  print(p)
  ggplot2::ggsave(file.path(dirs$km, paste0(country_iso3, "_kaplan_meier_", file_suffix, ".png")),
                  p, width = 14, height = 9, dpi = 300, bg = "white")
  km_fit
}

km_fit_self <- build_km_plot(fgm_events |> dplyr::filter(record_type == "self"),
                             "Self-reported (women 15-49)", 50, 5, "self", 0)
km_fit_daughter <- build_km_plot(fgm_events |> dplyr::filter(record_type == "daughter"),
                                 "Daughter-reported (girls 0-14)", 14, 1, "daughter", 0.5)
"""),
md("## 7. Estimate FGM incidence risk"),
code("""
reference_ages <- c(1, 2, 5, 8, 10, 15, 18)

build_risk_table <- function(km_fit, record_type_label) {
  risk_table <- summary(km_fit, times = reference_ages, extend = TRUE)
  tibble::tibble(
    birth_cohort = stringr::str_remove(as.character(risk_table$strata), "^birth_cohort="),
    age = risk_table$time, n_at_risk = risk_table$n.risk,
    fgm_risk = 1 - risk_table$surv, lower_ci = 1 - risk_table$upper, upper_ci = 1 - risk_table$lower,
    record_type = record_type_label
  )
}
risk_by_age <- dplyr::bind_rows(build_risk_table(km_fit_self, "self"), build_risk_table(km_fit_daughter, "daughter"))
# Ages beyond a cohort's follow-up are carried forward by survfit; mark them.
followup_cohort <- fgm_events |> dplyr::group_by(record_type, birth_cohort = as.character(birth_cohort)) |>
  dplyr::summarise(max_observed = max(time), .groups = "drop")
risk_by_age <- risk_by_age |>
  dplyr::left_join(followup_cohort, by = c("record_type", "birth_cohort")) |>
  dplyr::mutate(beyond_followup = age > max_observed)
readr::write_csv(risk_by_age, file.path(dirs$km, paste0(country_iso3, "_km_risk_by_age.csv")))

for (rt in c("self", "daughter")) {
  cat("\\n=====", rt, "reported: cumulative FGM risk by age and birth cohort (observed ages only) =====\\n")
  IRdisplay::display(as.data.frame(
    risk_by_age |> dplyr::filter(record_type == rt, !beyond_followup) |>
      dplyr::mutate(dplyr::across(c(fgm_risk, lower_ci, upper_ci), ~ scales::percent(.x, accuracy = 0.1))) |>
      dplyr::select(birth_cohort, age, fgm_risk, lower_ci, upper_ci)))
}
"""),
md("""
## 8. Discrete-time hazard model

Survey-weighted logistic regression on annual person-time, with covariates fitted jointly.
Age enters as groups (0–4, 5–9, 10–14, 15+). Person-years are summed within cluster and
covariate cell and modelled as weighted proportions; this reproduces the person-year
estimating equations and cluster-based standard errors exactly. The few records without
ethnicity are excluded from this model. All risk records are DHS, so no survey-programme
term is needed. V1's unweighted Cox frailty model is not repeated: its cohort
coefficients were degenerate in V1 and it does not scale to the pooled data.
"""),
code("""
hazard_cells <- lapply(split(fgm_events, fgm_events$record_type), function(d) {
  py <- aggregate_person_years(d |> dplyr::filter(!is.na(ethnicity)),
                               group_vars = c("region", "urban_rural", "ethnicity", "birth_cohort", "survey_type", "record_type"),
                               max_age = max_age_years)
  py[, age_group := cut(age_year, breaks = c(0, 5, 10, 15, Inf), right = FALSE, labels = c("0-4", "5-9", "10-14", "15+"))]
  py[, .(w_risk = sum(w_risk), w_event = sum(w_event)),
     by = .(cluster, strata, region, urban_rural, ethnicity, birth_cohort, survey_type, record_type, age_group)]
})
hazard_data <- data.table::rbindlist(hazard_cells) |>
  dplyr::mutate(prop = w_event / w_risk, birth_cohort = droplevels(factor(birth_cohort)),
                ethnicity = stats::relevel(factor(ethnicity), ref = "kikuyu"))

hazard_design <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~w_risk,
                                   data = hazard_data, nest = TRUE)
# Survey programme enters only when both DHS and MICS histories are present.
hazard_terms <- c("age_group", "birth_cohort", "urban_rural", "region", "ethnicity",
                  if (dplyr::n_distinct(hazard_data$survey_type) > 1) "survey_type", "record_type")
hazard_model_design_based <- survey::svyglm(
  stats::reformulate(hazard_terms, response = "prop"),
  design = hazard_design, family = quasibinomial()
)
hazard_coefficients <- tibble::as_tibble(summary(hazard_model_design_based)$coefficients, rownames = "term") |>
  dplyr::rename(estimate = Estimate, se = `Std. Error`, p_value = `Pr(>|t|)`) |>
  dplyr::mutate(odds_ratio = exp(estimate), or_lower = exp(estimate - 1.96 * se), or_upper = exp(estimate + 1.96 * se))
readr::write_csv(hazard_coefficients, file.path(dirs$hazard, paste0(country_iso3, "_hazard_model_coefficients.csv")))

cat("Cells:", nrow(hazard_data), "| person-years (weighted):", round(sum(hazard_data$w_risk)), "\\n")
IRdisplay::display(as.data.frame(hazard_coefficients |> dplyr::filter(!grepl("^region", term)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(.x, 3)))))
"""),
md("""
## 9. Cohort and trend smoothing

Penalized splines for birth cohort and single-year age, adjusted for residence, region and
reporting source, fitted on person-years aggregated by those variables.
"""),
code("""
cohort_numeric <- data.table::rbindlist(lapply(split(fgm_events, fgm_events$record_type), aggregate_person_years,
                                               group_vars = c("region", "urban_rural", "birth_cohort", "record_type"),
                                               max_age = max_age_years))
cohort_numeric <- cohort_numeric[, .(w_risk = sum(w_risk), w_event = sum(w_event)),
                                 by = .(region, urban_rural, birth_cohort, record_type, age_year)] |>
  dplyr::mutate(cohort_mid = as.numeric(substr(as.character(birth_cohort), 1, 4)) + 2,
                record_type = factor(record_type), region = factor(region),
                urban_rural = factor(urban_rural), prop = w_event / w_risk)

trend_model <- mgcv::bam(
  prop ~ s(cohort_mid, bs = "ps", k = 10) + s(age_year, bs = "ps", k = 10) + urban_rural + region + record_type,
  family = quasibinomial(), weights = w_risk / mean(w_risk), data = cohort_numeric
)
summary(trend_model)$s.table
"""),
code("""
ref_urban_rural <- names(sort(table(cohort_numeric$urban_rural), decreasing = TRUE))[1]
ref_region <- names(sort(tapply(cohort_numeric$w_risk, cohort_numeric$region, sum), decreasing = TRUE))[1]
predict_smooth <- function(var_name, grid_values, other_fixed_at) {
  newdata <- tibble::tibble(!!var_name := grid_values) |> dplyr::bind_cols(other_fixed_at)
  pred <- predict(trend_model, newdata = newdata, type = "link", se.fit = TRUE)
  newdata |> dplyr::mutate(fit = plogis(pred$fit), lower = plogis(pred$fit - 1.96 * pred$se.fit),
                           upper = plogis(pred$fit + 1.96 * pred$se.fit))
}
fixed <- tibble::tibble(urban_rural = factor(ref_urban_rural, levels = levels(cohort_numeric$urban_rural)),
                        region = factor(ref_region, levels = levels(cohort_numeric$region)),
                        record_type = factor("self", levels = levels(cohort_numeric$record_type)))
cohort_pred <- predict_smooth("cohort_mid", seq(min(cohort_numeric$cohort_mid), max(cohort_numeric$cohort_mid), length.out = 100),
                              fixed |> dplyr::mutate(age_year = 0))
age_pred <- predict_smooth("age_year", seq(0, max_age_years, length.out = 100),
                           fixed |> dplyr::mutate(cohort_mid = stats::median(cohort_numeric$cohort_mid)))
p_cohort <- ggplot2::ggplot(cohort_pred, ggplot2::aes(cohort_mid, fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#4C6EF5", alpha = 0.2) +
  ggplot2::geom_line(color = "#4C6EF5", linewidth = 1) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(title = paste(country_name, "- smoothed cohort effect on the annual hazard at age 0"),
                x = "Birth cohort (midpoint year)", y = "Predicted annual hazard") + ggplot2::theme_minimal(base_size = 13)
p_age <- ggplot2::ggplot(age_pred, ggplot2::aes(age_year, fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#F76707", alpha = 0.2) +
  ggplot2::geom_line(color = "#F76707", linewidth = 1) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(title = paste(country_name, "- smoothed age effect on the annual hazard"),
                x = "Age (years)", y = "Predicted annual hazard") + ggplot2::theme_minimal(base_size = 13)
print(p_cohort); print(p_age)
ggplot2::ggsave(file.path(dirs$hazard, paste0(country_iso3, "_smoothed_cohort_effect.png")), p_cohort, width = 9, height = 6, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(dirs$hazard, paste0(country_iso3, "_smoothed_age_effect.png")), p_age, width = 9, height = 6, dpi = 300, bg = "white")
"""),
md("""
## 10. Subnational direct estimates

Kaplan–Meier conditional risk within each five-year age band, by region and birth cohort,
pooling rounds. A band is "complete" when the cell's follow-up reaches its upper age;
otherwise only the provisional risk to the last observed age is kept, as in V1.
"""),
code("""
compute_cohort_region_age_band_risk <- function(record_label) {
  d <- fgm_events |> dplyr::filter(record_type == record_label)
  followup <- d |> dplyr::group_by(region, birth_cohort) |>
    dplyr::summarise(max_observed = max(time), sum_weight = sum(weight), n_records = sum(age_share), .groups = "drop")
  py <- py_cohort[[record_label]][, .(w_risk = sum(w_risk), w_event = sum(w_event)),
                                  by = .(cluster, strata, region, birth_cohort, age_year)]
  res <- km_band_risk(py, c("region", "birth_cohort"), age_band_cutpoints,
                      followup |> dplyr::select(region, birth_cohort, max_observed), design_psus[[record_label]])
  tibble::as_tibble(res) |>
    dplyr::left_join(followup |> dplyr::select(region, birth_cohort, sum_weight, n_records), by = c("region", "birth_cohort")) |>
    dplyr::filter(max_observed > lo) |>
    dplyr::transmute(
      region, birth_cohort = factor(as.character(birth_cohort)), age_band,
      event = risk, se = dplyr::if_else(supported, exp(log_surv) * se_log, NA_real_),
      provisional_event = 1 - exp(log_surv), provisional_se = exp(log_surv) * se_log,
      observed_to_age = pmin(max_observed, hi), is_complete = supported,
      sum_weight, n_records, record_type = record_label)
}
full_risk_self <- compute_cohort_region_age_band_risk("self")
full_risk_daughter <- compute_cohort_region_age_band_risk("daughter")
readr::write_csv(full_risk_self, file.path(dirs$direct, paste0(country_iso3, "_cohort_age_band_risk_self.csv")))
readr::write_csv(full_risk_daughter, file.path(dirs$direct, paste0(country_iso3, "_cohort_age_band_risk_daughter.csv")))
cat("Direct cells (complete / all): self", sum(full_risk_self$is_complete), "/", nrow(full_risk_self),
    "| daughter", sum(full_risk_daughter$is_complete), "/", nrow(full_risk_daughter), "\\n")
"""),
md("""
## 11. Spatial and temporal smoothing

Precision-weighted BYM2 region effect plus RW2 over birth cohort, fitted to the logit of the
direct estimates separately for each age band and reporting source (V1 method). Bands
with fewer than three complete cohorts are skipped.
"""),
code("""
if (run_spatial_model) {
  boundaries <- sf::st_read(shapefile_path, quiet = TRUE) |> sf::st_make_valid()
}
""" + spatial_cell.replace("if (run_spatial_model) {\n  boundaries <- sf::st_read(shapefile_path, quiet = TRUE) |> sf::st_make_valid()\n", "if (run_spatial_model) {\n", 1)),
md("""
## 12. Risk maps

Smoothed regional risk by birth cohort for each age band. The youngest daughter cohort is
mapped separately as provisional.
"""),
code(provisional_cell),
code(map_cell),
md("""
## 12A. Covariate-adjusted risk maps

Same model as V1 12A: a survey-weighted discrete-time logistic hazard with ethnicity and
urban/rural residence as fixed effects, RW2 over single years of age, BYM2 over the 13
regions and RW2 over birth cohort, fitted separately to self-reported and daughter-reported
histories. The few records without ethnicity are excluded. Predictions keep each
region/cohort's observed covariate composition. Intervals summarize 200 joint
pseudo-posterior draws and do not fully reflect the clustered design.
"""),
code(helpers_cell),
code("""
py_covariate <- lapply(split(fgm_events, fgm_events$record_type), aggregate_person_years,
                       group_vars = c("region", "birth_cohort", "ethnicity", "urban_rural"), max_age = max_age_years)
""" + run12a_cell),
code(map12a_cell),
md("""
## 13. Lexis heatmap

Weighted annual hazard by five-year age band and birth cohort, pooling all rounds.
"""),
code("""
lexis_grid <- lapply(py_cohort, function(py) {
  py[, .(w_risk = sum(w_risk), w_event = sum(w_event)), by = .(birth_cohort, age_year)][
    , age_band_mid := floor(age_year / 5) * 5 + 2.5][
    , .(hazard = sum(w_event) / sum(w_risk)), by = .(birth_cohort, age_band_mid)][
    , cohort_mid := as.numeric(substr(as.character(birth_cohort), 1, 4)) + 2]
})
lexis_grid_self <- lexis_grid$self
lexis_grid_daughter <- lexis_grid$daughter
hazard_limits <- range(c(lexis_grid_self$hazard, lexis_grid_daughter$hazard), na.rm = TRUE)

build_lexis_plot <- function(grid, subtitle_label, file_suffix) {
  p <- ggplot2::ggplot(grid, ggplot2::aes(cohort_mid, age_band_mid, fill = hazard)) +
    ggplot2::geom_tile(width = 5, height = 5, color = "white", linewidth = 0.6) +
    ggplot2::scale_fill_viridis_c(name = "FGM hazard", option = "inferno", labels = scales::percent,
                                  limits = hazard_limits, oob = scales::squish) +
    ggplot2::scale_y_continuous(breaks = seq(2.5, 47.5, by = 5), labels = paste0(seq(0, 45, by = 5), "\\u2013", seq(4, 49, by = 5))) +
    ggplot2::labs(title = paste("Lexis surface of FGM cutting risk,", country_name),
                  subtitle = subtitle_label, x = "Birth cohort", y = "Age") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"))
  print(p)
  ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_lexis_heatmap_", file_suffix, ".png")), p, width = 9, height = 6, dpi = 300, bg = "white")
  p
}
lexis_plot_self <- build_lexis_plot(lexis_grid_self, "Self-reported cohorts, all rounds pooled", "self")
lexis_plot_daughter <- build_lexis_plot(lexis_grid_daughter, "Daughter-reported cohorts, all rounds pooled", "daughter")
"""),
md("""
## 13A. National trend by birth cohort

Pooled survey-weighted Kaplan–Meier risk per age band and cohort, with linearized 95%
intervals (V1 method). Bands are shown only when the cohort's follow-up reaches the
band's upper age; the youngest daughter cohort is shown as a provisional point.
"""),
code("""
national_km <- function(record_label, domain_vars = "birth_cohort") {
  d <- fgm_events |> dplyr::filter(record_type == record_label)
  followup <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(domain_vars))) |>
    dplyr::summarise(max_observed = max(time), .groups = "drop")
  py <- py_cohort[[record_label]][, .(w_risk = sum(w_risk), w_event = sum(w_event)),
                                  by = c("cluster", "strata", domain_vars, "age_year")]
  tibble::as_tibble(km_band_risk(py, domain_vars, age_band_cutpoints, followup, design_psus[[record_label]])) |>
    dplyr::mutate(record_type = record_label)
}
national_km_trend <- dplyr::bind_rows(national_km("self"), national_km("daughter")) |>
  dplyr::mutate(birth_cohort = as.character(birth_cohort), mean_risk = risk)

# Point estimates must equal the pooled weighted Kaplan-Meier from survfit().
check <- purrr::map_dfr(c("self", "daughter"), function(rt) {
  fit <- if (rt == "self") km_fit_self else km_fit_daughter
  tidy <- broom::tidy(fit) |> dplyr::mutate(birth_cohort = stringr::str_remove(strata, "^birth_cohort="))
  s_at <- function(coh, t) { x <- tidy |> dplyr::filter(birth_cohort == coh, time <= t); if (!nrow(x)) 1 else x$estimate[nrow(x)] }
  national_km_trend |> dplyr::filter(record_type == rt, supported) |>
    dplyr::rowwise() |> dplyr::mutate(survfit_risk = 1 - s_at(birth_cohort, hi) / (if (lo == 0) 1 else s_at(birth_cohort, lo))) |>
    dplyr::ungroup()
})
stopifnot(max(abs(check$survfit_risk - check$risk)) < 1e-8)
cat("Pooled KM trend matches survfit() in", nrow(check), "cohort/band cells\\n")
readr::write_csv(national_km_trend |> dplyr::select(record_type, birth_cohort, age_band, mean_risk, lower_ci, upper_ci, supported, max_observed),
                 file.path(dirs$trends, paste0(country_iso3, "_national_km_trend.csv")))

trend_all_bands <- national_km_trend |>
  dplyr::filter(supported) |>
  dplyr::mutate(record_type = factor(dplyr::recode(record_type, self = "Self-reported", daughter = "Daughter-reported"),
                                     levels = c("Self-reported", "Daughter-reported")),
                age_band = factor(age_band, levels = age_band_levels, labels = paste("Ages", age_band_levels)))
provisional_points <- national_km_trend |>
  dplyr::filter(!supported, lo == 0, n_ages > 0) |>
  dplyr::transmute(birth_cohort, mean_risk = 1 - exp(log_surv), observed_to_age = max_observed,
                   record_type = factor(dplyr::recode(record_type, self = "Self-reported", daughter = "Daughter-reported"),
                                        levels = levels(trend_all_bands$record_type)),
                   age_band = factor("Ages 0-5", levels = levels(trend_all_bands$age_band)))
cohort_levels_all <- sort(unique(c(trend_all_bands$birth_cohort, provisional_points$birth_cohort)))
trend_all_bands$birth_cohort <- factor(trend_all_bands$birth_cohort, levels = cohort_levels_all)
provisional_points$birth_cohort <- factor(provisional_points$birth_cohort, levels = cohort_levels_all)

age_band_colors <- c("Ages 0-5" = "#1D4E89", "Ages 5-10" = "#D9534F", "Ages 10-15" = "#5CB85C",
                     "Ages 15-20" = "#E5A11C", "Ages 20-25" = "#8E5DA0")
trend_plot_all_bands <- ggplot2::ggplot(trend_all_bands,
    ggplot2::aes(birth_cohort, mean_risk, color = age_band, linetype = record_type, group = interaction(age_band, record_type))) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci, ymax = upper_ci, fill = age_band), alpha = 0.12, color = NA, show.legend = FALSE) +
  ggplot2::geom_line(linewidth = 1.1) + ggplot2::geom_point(size = 2.4) +
  ggplot2::geom_point(data = provisional_points, ggplot2::aes(birth_cohort, mean_risk, color = age_band),
                      shape = 1, size = 3.2, stroke = 1.2, inherit.aes = FALSE, show.legend = FALSE) +
  ggplot2::geom_text(data = provisional_points, ggplot2::aes(birth_cohort, mean_risk, color = age_band,
                     label = paste0("to age ", observed_to_age)), vjust = -1.3, size = 3.2, inherit.aes = FALSE, show.legend = FALSE) +
  ggplot2::scale_color_manual(values = age_band_colors, name = "Age band") +
  ggplot2::scale_fill_manual(values = age_band_colors, guide = "none") +
  ggplot2::scale_linetype_manual(values = c("Self-reported" = "solid", "Daughter-reported" = "dashed"), name = "Reporting source") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, NA),
                              breaks = scales::pretty_breaks(n = 6), expand = ggplot2::expansion(mult = c(0.01, 0.08))) +
  ggplot2::labs(title = stringr::str_wrap(paste("National FGM Risk Trend by Birth Cohort and Age Band,", country_name), width = 45),
                subtitle = "All survey rounds with age at cutting pooled",
                caption = if (nrow(provisional_points)) "Hollow point: provisional; girls born 2020-2024 are observed only to the age shown, not yet to age 5." else NULL,
                x = "Birth cohort", y = "Risk of first FGM per age band") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 17, hjust = 0.5, lineheight = 1.15, margin = ggplot2::margin(b = 4)),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35", margin = ggplot2::margin(b = 8)),
    plot.caption = ggplot2::element_text(size = 9, color = "grey40", hjust = 0.5, margin = ggplot2::margin(t = 10)),
    axis.title.x = ggplot2::element_text(face = "bold", size = 13, margin = ggplot2::margin(t = 12)),
    axis.title.y = ggplot2::element_text(face = "bold", size = 13, margin = ggplot2::margin(r = 12)),
    axis.text.x = ggplot2::element_text(face = "bold", angle = 45, hjust = 1, vjust = 1, size = 10.5),
    axis.text.y = ggplot2::element_text(face = "bold", size = 10.5),
    legend.position = "bottom", legend.box = "vertical",
    legend.title = ggplot2::element_text(face = "bold", size = 11.5), legend.text = ggplot2::element_text(size = 10.5),
    panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "grey80", linewidth = 0.4),
    plot.margin = ggplot2::margin(t = 15, r = 20, b = 15, l = 15)) +
  ggplot2::guides(color = ggplot2::guide_legend(order = 1, nrow = 2), linetype = ggplot2::guide_legend(order = 2))
print(trend_plot_all_bands)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_national_risk_trend_all_bands.png")),
                trend_plot_all_bands, width = 12, height = 8, dpi = 300, bg = "white")
"""),
md("""
## 13B. Trend by survey round

The same cohort's cumulative risk estimated separately in each round: by age 15 for women's own
reports and by age 10 for mothers' reports on daughters. Agreement between rounds that observe
the same cohort indicates consistent reporting; systematic gaps between early and late rounds
indicate reporting differences rather than real change. Cohorts not yet followed to the key age
in a round are not shown.
"""),
code("""
key_age_self <- 15L       # most cutting in Kenya happens at ages 5-15
key_age_daughter <- 10L   # daughters are observed only to age 14
km_by_age <- function(d, domain, a) {
  psus <- data.table::as.data.table(d)[, .(cluster = unique(cluster)), by = strata][, .(n_h = .N), by = strata]
  py <- aggregate_person_years(d, domain, max_age = a)
  fu <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(domain))) |>
    dplyr::summarise(max_observed = max(time), n = sum(age_share), .groups = "drop")
  tibble::as_tibble(km_band_risk(py, domain, c(0, a), fu |> dplyr::select(-n), psus)) |>
    dplyr::left_join(fu |> dplyr::select(-max_observed), by = domain) |>
    dplyr::filter(supported) |>
    dplyr::transmute(dplyr::across(dplyr::all_of(domain), as.character), risk, lower_ci, upper_ci, n)
}
self_label <- paste0("Self-reported (women): risk by age ", key_age_self)
daughter_label <- paste0("Daughter-reported (girls): risk by age ", key_age_daughter)
by_round_all <- dplyr::bind_rows(
  km_by_age(fgm_events |> dplyr::filter(record_type == "self"), c("survey_id", "birth_cohort"), key_age_self) |>
    dplyr::mutate(record_type = self_label),
  km_by_age(fgm_events |> dplyr::filter(record_type == "daughter"), c("survey_id", "birth_cohort"), key_age_daughter) |>
    dplyr::mutate(record_type = daughter_label)) |>
  dplyr::left_join(fgm_events |> dplyr::distinct(survey_id, survey_type), by = "survey_id") |>
  dplyr::mutate(record_type = factor(record_type, levels = c(self_label, daughter_label)),
                risk_plot = risk, provisional = FALSE,
                survey_id = factor(survey_id, levels = c("DHS 1998", "DHS 2008", "DHS 2014", "DHS 2022")))
readr::write_csv(by_round_all |> dplyr::select(record_type, survey_id, survey_type, birth_cohort, risk, lower_ci, upper_ci, n),
                 file.path(dirs$trends, paste0(country_iso3, "_risk_by_round_and_cohort.csv")))
by_round <- by_round_all

round_trend_plot <- ggplot2::ggplot(by_round, ggplot2::aes(birth_cohort, risk_plot, color = survey_id, group = survey_id)) +
  ggplot2::geom_line(linewidth = 1.1) +
  ggplot2::geom_point(size = 2.8) +
  ggplot2::facet_wrap(~record_type, ncol = 1, scales = "free") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0, 0.05))) +
  ggplot2::labs(title = paste("Cumulative Risk of FGM by Birth Cohort, Each Survey Round,", country_name),
                x = "Birth cohort", y = "Cumulative risk (Kaplan-Meier)", color = "Survey") +
  ggplot2::theme_minimal(base_size = 15) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 21, hjust = 0.5),
                 strip.text = ggplot2::element_text(face = "bold", size = 14),
                 strip.background = ggplot2::element_rect(fill = "grey93", color = NA),
                 panel.grid.major = ggplot2::element_line(color = "grey85", linetype = "dashed", linewidth = 0.35),
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(color = "grey60"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.title = ggplot2::element_text(face = "bold"),
                 legend.box.background = ggplot2::element_rect(color = "grey80", fill = "white"))
print(round_trend_plot)
ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_risk_by_round_and_cohort.png")),
                round_trend_plot, width = 16, height = 10, dpi = 300, bg = "white")
"""),
md("""
## 14. Change point analysis

Mean self-reported annual hazard over ages 0–14 by birth cohort, pooling all rounds, with
PELT change-point detection (V1 method).
"""),
code("""
if (run_change_point) {
  suppressPackageStartupMessages(library(changepoint))
  cohort_series <- lexis_grid_self |>
    dplyr::filter(age_band_mid < 15) |>
    dplyr::group_by(cohort_mid) |>
    dplyr::summarise(mean_hazard = mean(hazard, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(cohort_mid)
  cpt_result <- changepoint::cpt.mean(cohort_series$mean_hazard, method = "PELT")
  n_segments <- length(cpt_result@cpts)
  change_point_estimate <- tibble::tibble(
    country = country_name, basis = "self-reported cohorts, all rounds, mean annual hazard ages 0-14",
    changepoint_detected = n_segments > 1,
    estimated_change_cohort = if (n_segments > 1) cohort_series$cohort_mid[cpt_result@cpts[1]] else NA_real_)
  readr::write_csv(change_point_estimate, file.path(dirs$trends, paste0(country_iso3, "_change_point.csv")))
  print(change_point_estimate)
  cohort_series_plot <- cohort_series |>
    dplyr::mutate(segment = cut(dplyr::row_number(), breaks = c(0, cpt_result@cpts), labels = FALSE)) |>
    dplyr::group_by(segment) |> dplyr::mutate(segment_mean = mean(mean_hazard)) |> dplyr::ungroup()
  cpt_plot <- ggplot2::ggplot(cohort_series_plot, ggplot2::aes(cohort_mid, mean_hazard)) +
    ggplot2::geom_line(color = "#4C6EF5", linewidth = 1) + ggplot2::geom_point(size = 2.5, color = "#4C6EF5") +
    ggplot2::geom_line(ggplot2::aes(y = segment_mean, group = segment), color = "firebrick", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(title = paste("Change-point analysis, self-reported cohorts,", country_name),
                  subtitle = if (n_segments > 1) paste("PELT: shift after the", cohort_series_plot$cohort_mid[cpt_result@cpts[1]], "cohort")
                             else "PELT: no distinct mean shift detected",
                  x = "Birth cohort (midpoint year)", y = "Average annual FGM hazard, ages 0-14") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"))
  print(cpt_plot)
  ggplot2::ggsave(file.path(dirs$trends, paste0(country_iso3, "_change_point.png")), cpt_plot, width = 9, height = 5.5, dpi = 300, bg = "white")
}
"""),
md("""
## 15. Descriptive model comparison

Conditional risk within each age band from the pooled national Kaplan–Meier, the direct
regional estimates (weighted average of complete cells) and the spatial model. Aggregation
and eligible cohorts differ, so agreement is descriptive.
"""),
code("""
km_overall <- function(record_label) {
  py <- py_cohort[[record_label]][, .(w_risk = sum(w_risk), w_event = sum(w_event)), by = .(cluster, strata, age_year)][
    , national := country_name]
  followup <- tibble::tibble(national = country_name,
                             max_observed = max(fgm_events$time[fgm_events$record_type == record_label]))
  tibble::as_tibble(km_band_risk(py, "national", age_band_cutpoints, followup, design_psus[[record_label]])) |>
    dplyr::transmute(age_band, fgm_risk = risk, record_type = record_label, model = "Kaplan Meier")
}
km_national <- dplyr::bind_rows(km_overall("self"), km_overall("daughter"))
direct_national <- dplyr::bind_rows(full_risk_self, full_risk_daughter) |>
  dplyr::filter(is_complete) |>
  dplyr::group_by(age_band, record_type) |>
  dplyr::summarise(fgm_risk = weighted.mean(event, w = sum_weight, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(model = "Design based direct")
spatial_national <- if (run_spatial_model) {
  dplyr::bind_rows(smoothed_by_band_self |> dplyr::mutate(record_type = "self"),
                   smoothed_by_band_daughter |> dplyr::mutate(record_type = "daughter")) |>
    dplyr::group_by(age_band, record_type) |>
    dplyr::summarise(fgm_risk = weighted.mean(smoothed_risk, w = sum_weight, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(model = "Spatial small area")
} else tibble::tibble()
validation_comparison <- dplyr::bind_rows(km_national, direct_national, spatial_national) |>
  dplyr::mutate(age_band = factor(age_band, levels = age_band_levels),
                model = factor(model, levels = c("Kaplan Meier", "Design based direct", "Spatial small area")),
                record_type = factor(record_type, levels = c("self", "daughter"), labels = c("Self-reported", "Daughter-reported")))
validation_plot <- ggplot2::ggplot(validation_comparison, ggplot2::aes(age_band, fgm_risk, color = record_type, group = record_type)) +
  ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) + ggplot2::geom_point(size = 2, na.rm = TRUE) +
  ggplot2::facet_wrap(~model, ncol = 1) +
  ggplot2::scale_color_manual(values = c("Self-reported" = "#1D4E89", "Daughter-reported" = "#D9534F")) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ggplot2::labs(title = paste("Model comparison,", country_name),
                subtitle = "Conditional risk within each age band among those still uncut at its start, all rounds pooled",
                y = "Conditional FGM risk", x = "Age band (years)", color = "Reporting source") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                 strip.text = ggplot2::element_text(face = "bold"), legend.position = "bottom")
print(validation_plot)
ggplot2::ggsave(file.path(dirs$validation, paste0(country_iso3, "_model_comparison.png")), validation_plot, width = 9, height = 11, dpi = 300, bg = "white")
readr::write_csv(validation_comparison, file.path(dirs$validation, paste0(country_iso3, "_model_comparison.csv")))
"""),
md("""
## 15A. Check against direct estimates

(1) Pooled national Kaplan–Meier trend vs. the weighted average of complete regional direct
estimates, by cohort and band; (2) the 12A covariate model vs. regional direct estimates,
cell by cell.
"""),
code("""
direct_cohort_national <- dplyr::bind_rows(full_risk_self, full_risk_daughter) |>
  dplyr::filter(is_complete, !is.na(event)) |>
  dplyr::group_by(record_type, birth_cohort = as.character(birth_cohort), age_band) |>
  dplyr::summarise(direct_risk = weighted.mean(event, w = sum_weight), n_regions = dplyr::n(), .groups = "drop")

trend_vs_direct <- national_km_trend |>
  dplyr::filter(supported) |>
  dplyr::inner_join(direct_cohort_national, by = c("record_type", "birth_cohort", "age_band")) |>
  dplyr::mutate(difference = mean_risk - direct_risk,
                direct_in_km_ci = direct_risk >= lower_ci & direct_risk <= upper_ci)
readr::write_csv(trend_vs_direct, file.path(dirs$validation, paste0(country_iso3, "_check_trend_vs_direct.csv")))
cat("National pooled KM vs. direct regional average (complete cells)\\n")
print(trend_vs_direct |> dplyr::group_by(record_type, age_band) |>
  dplyr::summarise(cells = dplyr::n(), mean_diff_pp = 100 * mean(difference),
                   max_abs_diff_pp = 100 * max(abs(difference)), direct_within_ci = mean(direct_in_km_ci), .groups = "drop"))

if (run_spatial_model) {
  covariate_vs_direct <- dplyr::bind_rows(geo_cov_results) |>
    dplyr::filter(!is.na(adjusted_risk)) |>
    dplyr::inner_join(dplyr::bind_rows(full_risk_self, full_risk_daughter) |>
                        dplyr::mutate(birth_cohort = as.character(birth_cohort)) |>
                        dplyr::select(record_type, region, birth_cohort, age_band, event, provisional_event),
                      by = c("record_type", "region", "birth_cohort", "age_band")) |>
    dplyr::mutate(direct_risk = dplyr::if_else(is_provisional, provisional_event, event),
                  difference = adjusted_risk - direct_risk,
                  direct_in_ci = direct_risk >= lower_ci & direct_risk <= upper_ci) |>
    dplyr::filter(!is.na(direct_risk))
  readr::write_csv(covariate_vs_direct, file.path(dirs$validation, paste0(country_iso3, "_check_covariate_model_vs_direct.csv")))
  cat("\\n12A covariate model vs. regional direct estimates\\n")
  print(covariate_vs_direct |> dplyr::group_by(record_type, age_band, is_provisional) |>
    dplyr::summarise(cells = dplyr::n(), mean_diff_pp = 100 * mean(difference), mean_abs_diff_pp = 100 * mean(abs(difference)),
                     correlation = if (dplyr::n() > 2) stats::cor(adjusted_risk, direct_risk) else NA_real_,
                     direct_within_ci = mean(direct_in_ci), .groups = "drop"))
}
"""),
md("""
## 15B. Sensitivity analysis: reporting, age at interview and survey programme

Risk incidence rests on women recalling, and being willing to report, whether and when they
were cut, often decades earlier. Because each birth cohort is observed in several rounds, at
different ages and survey years, the pooled data can test how sensitive the estimates are to
age at interview and reporting source. Checks on women's own reports use the risk of being cut
by age 15 (Kaplan–Meier, survey-weighted); the comparison with mothers' reports uses risk by
age 10, because daughters are observed only to age 14.

1. **Same cohort across rounds** (section 13B, summarised here).
2. **Fixed age at interview:** each cohort estimated only from women interviewed at ages
   20–29, so every cohort is compared at the same distance from its childhood.
3. **Age at interview within cohort:** the same cohort reported at 15–24, 25–34 and 35–49.
4. **Women's own reports vs mothers' reports** (risk by age 10) for cohorts observed both ways.
5. **DHS vs MICS:** not possible, because Kenya has no national MICS; the subnational MICS
   are compared in section 15C.
6. **Age-at-cutting estimation:** risk with "during infancy" and unknown ages estimated (main
   analysis) versus restricted to exact reported ages.
"""),
code("""
km_by5 <- function(d, domain) km_by_age(d, domain, key_age_self)
self_events <- fgm_events |> dplyr::filter(record_type == "self") |>
  dplyr::mutate(age_at_interview = cut(age, c(14, 24, 34, 49), labels = c("15-24", "25-34", "35-49")))
pct <- function(x) round(100 * x, 1)

# 1. Same cohort across rounds (women's reports).
same_cohort <- by_round_all |> dplyr::filter(record_type == self_label) |>
  dplyr::transmute(birth_cohort, survey_id = as.character(survey_id), risk = pct(risk_plot)) |>
  tidyr::pivot_wider(names_from = survey_id, values_from = risk) |> dplyr::arrange(birth_cohort)

# 2-3. Fixed age at interview and age at interview within cohort.
all_ages <- km_by5(self_events, "birth_cohort") |> dplyr::select(birth_cohort, all_ages = risk)
age_20_29 <- km_by5(self_events |> dplyr::filter(age >= 20, age <= 29), "birth_cohort") |>
  dplyr::select(birth_cohort, age_20_29 = risk, age_20_29_lower = lower_ci, age_20_29_upper = upper_ci, age_20_29_n = n)
by_interview_age <- km_by5(self_events, c("birth_cohort", "age_at_interview")) |>
  dplyr::mutate(risk = pct(risk)) |> dplyr::select(birth_cohort, age_at_interview, risk) |>
  tidyr::pivot_wider(names_from = age_at_interview, values_from = risk, names_prefix = "interviewed_")

# 4. Women's own reports vs mothers' reports for the same birth cohorts.
own_by10 <- km_by_age(self_events, "birth_cohort", key_age_daughter) |>
  dplyr::select(birth_cohort, own_by10 = risk, own_by10_lower = lower_ci, own_by10_upper = upper_ci)
mothers <- km_by_age(fgm_events |> dplyr::filter(record_type == "daughter"), "birth_cohort", key_age_daughter) |>
  dplyr::select(birth_cohort, mothers_reports = risk, mothers_lower = lower_ci, mothers_upper = upper_ci)

# 5. DHS vs MICS, same cohorts.
programme <- if (all(c("DHS", "MICS") %in% self_events$survey_type)) {
  km_by5(self_events, c("survey_type", "birth_cohort")) |> dplyr::mutate(risk = pct(risk)) |>
    dplyr::select(survey_type, birth_cohort, risk) |> tidyr::pivot_wider(names_from = survey_type, values_from = risk) |>
    dplyr::filter(!is.na(DHS), !is.na(MICS)) |> dplyr::mutate(DHS_minus_MICS = DHS - MICS)
} else tibble::tibble(note = "Kenya has no national MICS; its subnational MICS are compared in section 15C. No DHS-MICS comparison of national cohorts is possible.")

# 6. Exact reported ages only (drops estimated ages; biased downward, shown for comparison).
by_type <- function(d, a_self, a_daughter) dplyr::bind_rows(
  km_by_age(d |> dplyr::filter(record_type == "self"), "birth_cohort", a_self) |> dplyr::mutate(record_type = "self", key_age = a_self),
  km_by_age(d |> dplyr::filter(record_type == "daughter"), "birth_cohort", a_daughter) |> dplyr::mutate(record_type = "daughter", key_age = a_daughter))
exact_only <- by_type(fgm_events |> dplyr::filter(age_source %in% c("not cut", "exact age reported")), key_age_self, key_age_daughter) |>
  dplyr::select(record_type, key_age, birth_cohort, exact_ages_only = risk)
main_estimate <- by_type(fgm_events, key_age_self, key_age_daughter) |> dplyr::select(record_type, key_age, birth_cohort, main = risk)
age_method <- main_estimate |> dplyr::inner_join(exact_only, by = c("record_type", "key_age", "birth_cohort")) |>
  dplyr::mutate(difference_pp = pct(main - exact_ages_only), main = pct(main), exact_ages_only = pct(exact_ages_only))

sensitivity_table <- all_ages |> dplyr::full_join(age_20_29, by = "birth_cohort") |>
  dplyr::full_join(own_by10, by = "birth_cohort") |>
  dplyr::full_join(mothers, by = "birth_cohort") |> dplyr::arrange(birth_cohort) |>
  dplyr::mutate(dplyr::across(where(is.numeric) & !dplyr::ends_with("_n"), pct))
readr::write_csv(same_cohort, file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_same_cohort_by_round.csv")))
readr::write_csv(sensitivity_table, file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_fixed_age_and_mothers.csv")))
readr::write_csv(by_interview_age, file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_by_age_at_interview.csv")))
readr::write_csv(programme, file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_dhs_vs_mics.csv")))
readr::write_csv(age_method, file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_age_at_cutting_method.csv")))

cat("1. Same cohort, risk by age 15 (%) in each round\\n"); IRdisplay::display(as.data.frame(same_cohort))
cat("2/4. Risk by age 15: all ages vs interviewed at 20-29; risk by age 10: own vs mothers' reports (%)\\n"); IRdisplay::display(as.data.frame(sensitivity_table))
cat("3. By age at interview (%)\\n"); IRdisplay::display(as.data.frame(by_interview_age))
cat("5. DHS vs MICS (%)\\n"); IRdisplay::display(as.data.frame(programme))
cat("6. Age-at-cutting estimation vs exact ages only (%)\\n"); IRdisplay::display(as.data.frame(age_method))
"""),
code("""
panel_15 <- "Women's own reports: risk by age 15"
panel_10 <- "Own vs mothers' reports: risk by age 10"
sensitivity_plot_data <- dplyr::bind_rows(
  sensitivity_table |> dplyr::transmute(birth_cohort, panel = panel_15, series = "Women, all ages at interview", risk = all_ages / 100),
  sensitivity_table |> dplyr::transmute(birth_cohort, panel = panel_15, series = "Women interviewed at ages 20-29", risk = age_20_29 / 100,
                                        lower = age_20_29_lower / 100, upper = age_20_29_upper / 100),
  sensitivity_table |> dplyr::transmute(birth_cohort, panel = panel_10, series = "Women's own reports", risk = own_by10 / 100,
                                        lower = own_by10_lower / 100, upper = own_by10_upper / 100),
  sensitivity_table |> dplyr::transmute(birth_cohort, panel = panel_10, series = "Mothers' reports for daughters", risk = mothers_reports / 100,
                                        lower = mothers_lower / 100, upper = mothers_upper / 100)
) |> dplyr::filter(!is.na(risk)) |>
  dplyr::mutate(panel = factor(panel, levels = c(panel_15, panel_10)),
                series = factor(series, levels = c("Women, all ages at interview", "Women interviewed at ages 20-29",
                                                   "Women's own reports", "Mothers' reports for daughters")))
series_colours <- c("Women, all ages at interview" = "grey45", "Women interviewed at ages 20-29" = "#1D4E89",
                    "Women's own reports" = "#E5A11C", "Mothers' reports for daughters" = "#C0392B")
sensitivity_plot <- ggplot2::ggplot(sensitivity_plot_data, ggplot2::aes(birth_cohort, risk, color = series, group = series)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = series), alpha = 0.12, color = NA, show.legend = FALSE) +
  ggplot2::geom_line(linewidth = 1.2) + ggplot2::geom_point(size = 2.6) +
  ggplot2::geom_text(ggplot2::aes(label = scales::percent(risk, accuracy = 0.1)), vjust = -1, size = 2.9, fontface = "bold",
                     show.legend = FALSE) +
  ggplot2::facet_wrap(~panel, ncol = 1, scales = "free") +
  ggplot2::scale_color_manual(values = series_colours, name = NULL) +
  ggplot2::scale_fill_manual(values = series_colours, guide = "none") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(title = paste("Sensitivity of FGM Risk to Reporting,", country_name),
                subtitle = "All survey rounds pooled; Kaplan-Meier, survey-weighted, 95% CI",
                x = "Birth cohort", y = "Cumulative risk of FGM") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 17, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35"),
                 strip.text = ggplot2::element_text(face = "bold", size = 12.5),
                 axis.title = ggplot2::element_text(face = "bold"),
                 axis.text = ggplot2::element_text(face = "bold", color = "grey25"),
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                 legend.position = "bottom", legend.text = ggplot2::element_text(face = "bold", size = 11))
print(sensitivity_plot)
ggplot2::ggsave(file.path(dirs$sensitivity, paste0(country_iso3, "_sensitivity_risk.png")), sensitivity_plot,
                width = 13, height = 10, dpi = 300, bg = "white")
"""),
md("""
## 16. Optional machine learning cross check

Random survival forest on a stratified random sample (up to 4,000 records per round and
reporting source) of histories with an exact age at cutting or no cutting, as a
covariate-screening cross check only.
"""),
code("""
if (run_ml_crosscheck) {
  suppressPackageStartupMessages(library(randomForestSRC))
  rsf_data <- fgm_events |>
    dplyr::filter(age_share == 1) |>
    dplyr::select(time, fgm_event, urban_rural, region, survey_type, ethnicity, record_type, birth_cohort, survey_id) |>
    tidyr::drop_na() |>
    dplyr::group_by(survey_id, record_type) |> dplyr::slice_sample(n = 4000) |> dplyr::ungroup() |>
    dplyr::select(-survey_id) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor), birth_cohort = droplevels(birth_cohort)) |>
    # Drop predictors with a single value (survey_type is DHS only).
    dplyr::select(dplyr::where(~ dplyr::n_distinct(.x) > 1))
  rsf_model <- randomForestSRC::rfsrc(Surv(time, fgm_event) ~ ., data = as.data.frame(rsf_data), ntree = 300, importance = TRUE)
  print(rsf_model)
  rsf_importance <- sort(rsf_model$importance, decreasing = TRUE)
  readr::write_csv(tibble::enframe(rsf_importance, "variable", "importance"),
                   file.path(dirs$validation, paste0(country_iso3, "_rsf_variable_importance.csv")))
  rsf_importance
}
"""),
md("## 17. Final outputs"),
code("""
cat("All risk outputs written to:", normalizePath(output_dir), "\\n")
tibble::tibble(file = list.files(output_dir, recursive = TRUE))
"""),
]

nb = {"cells": cells, "metadata": v1["metadata"], "nbformat": 4, "nbformat_minor": 5}
json.dump(nb, open(f"{OUT}/FGM_Risk_Incidence.ipynb", "w"), indent=1, ensure_ascii=False)
print("cells:", len(cells))
