suppressPackageStartupMessages({library(tidyverse); library(data.table)}); options(width = 200)
source(file.path(Sys.getenv("S"), "km_core.R"))
e <- readRDS("../02_Data/processed/KEN/analysis/KEN_fgm_events.rds")
km05 <- function(d, dom) {
  psus <- as.data.table(d)[, .(cluster = unique(cluster)), by = strata][, .(n_h = .N), by = strata]
  py <- aggregate_person_years(d, dom, max_age = 5L)
  fu <- d |> group_by(across(all_of(dom))) |> summarise(max_observed = max(time), n = sum(age_share), .groups = "drop")
  as_tibble(km_band_risk(py, dom, c(0, 5), fu |> select(-n), psus)) |> left_join(fu |> select(-max_observed), by = dom) |>
    filter(supported) |> transmute(across(all_of(dom)), risk = 100 * risk, lower = 100 * lower_ci, upper = 100 * upper_ci, n)
}
self <- e |> filter(record_type == "self") |> mutate(age_at_interview = cut(age, c(14, 24, 34, 49), labels = c("15-24", "25-34", "35-49")))
cat("\n## A. Risk before 5 by cohort, women interviewed at ages 20-29 vs all ages\n")
a1 <- km05(self, "birth_cohort") |> select(birth_cohort, all_ages = risk)
a2 <- km05(self |> filter(age >= 20, age <= 29), "birth_cohort") |> select(birth_cohort, age_20_29 = risk, lo = lower, hi = upper, n)
print(full_join(a1, a2, by = "birth_cohort") |> mutate(across(where(is.numeric), ~ round(.x, 1))))
cat("\n## B. Same cohort by age at interview\n")
b <- km05(self, c("birth_cohort", "age_at_interview")) |> select(birth_cohort, age_at_interview, risk) |>
  pivot_wider(names_from = age_at_interview, values_from = risk)
print(b |> mutate(across(where(is.numeric), ~ round(.x, 1))))
cat("\n## C. Overlapping cohorts: women's own vs mothers' reports\n")
c1 <- km05(e |> filter(birth_cohort %in% c("1995 to 1999", "2000 to 2004", "2005 to 2009")), c("record_type", "birth_cohort")) |>
  select(record_type, birth_cohort, risk, lower, upper)
print(c1 |> mutate(across(where(is.numeric), ~ round(.x, 1))))
cat("\n## D. DHS vs MICS, same cohort\n")
d <- km05(self |> filter(birth_cohort >= "1965 to 1969"), c("survey_type", "birth_cohort")) |> select(survey_type, birth_cohort, risk) |>
  pivot_wider(names_from = survey_type, values_from = risk)
print(d |> mutate(across(where(is.numeric), ~ round(.x, 1))))
