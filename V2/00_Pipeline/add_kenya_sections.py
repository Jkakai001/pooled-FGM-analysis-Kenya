"""Insert the Kenya-specific sections into the V2 notebooks: official-report check and MICS
harmonisation (EDA), subnational MICS comparisons (prevalence 8A, risk 15C, attitudes Part D) and
county estimates from DHS 2014 and 2022 (prevalence 8B, attitudes Part D).

Usage: python3 add_kenya_sections.py <V2/01_Notebooks>
Run after the notebooks are generated (and after eda_summary.py / add_results.py); re-running
replaces the sections. Each section reads the harmonised files, so it runs on its own.
"""
import json, sys, uuid, os

NB = sys.argv[1]
TAG = "kenya-extra"


def md(text):
    return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {"tags": [TAG]},
            "source": text.strip("\n").splitlines(keepends=True)}


def code(text):
    return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "metadata": {"tags": [TAG]},
            "execution_count": None, "outputs": [], "source": text.strip("\n").splitlines(keepends=True)}


COMMON = r'''
source(file.path(project_root, "00_Pipeline", "mics_core.R"))
source(file.path(project_root, "00_Pipeline", "county_core.R"))
options(survey.lonely.psu = "adjust")
pct1 <- function(x) round(100 * x, 1)
harmonized_path <- file.path(project_root, "02_Data", "processed", country_iso3, "harmonized")
h_women <- readRDS(file.path(harmonized_path, paste0(country_iso3, "_women_all_rounds.rds"))) |>
  dplyr::mutate(fgm_heard = as.numeric(fgm_heard))
mics_women <- readRDS(file.path(harmonized_path, paste0(country_iso3, "_mics_subnational_women.rds")))
'''

# ---------------------------------------------------------------- EDA
EDA = [
    md(r'''
## 7A. Check against the official DHS reports

Before modelling, the harmonised data must reproduce the official DHS figures (DHS Program API,
national values). Women's prevalence and attitudes match the official tables because no Kenya
round recorded a "don't know" own status. The official attitude figure keeps "depends" and
"don't know" answers in the denominator, so it is compared with the share of all answers.
'''),
    code(r'''
options(survey.lonely.psu = "adjust")
official <- tibble::tribble(
  ~survey_id, ~indicator, ~official,
  "DHS 1998", "Women 15-49 circumcised", 37.6, "DHS 2003", "Women 15-49 circumcised", 32.2,
  "DHS 2008", "Women 15-49 circumcised", 27.1, "DHS 2014", "Women 15-49 circumcised", 21.0,
  "DHS 2022", "Women 15-49 circumcised", 14.8,
  "DHS 2008", "Heard of FGM", 96.1, "DHS 2014", "Heard of FGM", 96.8, "DHS 2022", "Heard of FGM", 96.9,
  "DHS 2014", "Girls 0-14 circumcised", 2.8, "DHS 2022", "Girls 0-14 circumcised", 1.6,
  "DHS 1998", "Women: support continuation (all answers)", 19.8, "DHS 2008", "Women: support continuation (all answers)", 9.8,
  "DHS 2014", "Women: support continuation (all answers)", 6.2, "DHS 2022", "Women: support continuation (all answers)", 4.3,
  "DHS 2014", "Men: support continuation (all answers)", 9.3, "DHS 2022", "Men: support continuation (all answers)", 4.8)
ours_one <- function(d, var) {
  d <- d |> dplyr::filter(!is.na(.data[[var]]))
  if (!nrow(d)) return(NA_real_)
  des <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
  round(100 * as.numeric(stats::coef(survey::svymean(stats::as.formula(paste0("~", var)), des))), 1)
}
support_all <- function(d) d |> dplyr::mutate(support_all = dplyr::if_else(!is.na(attitude_answer), as.numeric(attitude_answer == "continued"), NA_real_))
ours <- dplyr::bind_rows(lapply(c("DHS 1998", "DHS 2003", "DHS 2008", "DHS 2014", "DHS 2022"), function(s) {
  w <- women |> dplyr::filter(survey_id == s) |> support_all() |>
    dplyr::mutate(heard_any = dplyr::if_else(module_asked & !is.na(fgm_heard), as.numeric(fgm_heard %in% TRUE), NA_real_))
  d <- daughters |> dplyr::filter(survey_id == s)
  m <- men |> dplyr::filter(survey_id == s, fgm_heard) |> support_all()
  tibble::tibble(survey_id = s,
                 indicator = c("Women 15-49 circumcised", "Heard of FGM", "Girls 0-14 circumcised",
                               "Women: support continuation (all answers)", "Men: support continuation (all answers)"),
                 ours = c(ours_one(w, "fgm_status"), ours_one(w, "heard_any"),
                          if (nrow(d)) ours_one(d, "daughter_status") else NA_real_, ours_one(w, "support_all"),
                          if (nrow(m)) ours_one(m, "support_all") else NA_real_))
}))
official_check <- official |> dplyr::left_join(ours, by = c("survey_id", "indicator")) |>
  dplyr::mutate(difference = round(ours - official, 1))
readr::write_csv(official_check, file.path(output_dir, paste0(country_iso3, "_official_report_check.csv")))
IRdisplay::display(as.data.frame(official_check))
stopifnot(all(abs(official_check$difference) <= 0.2))
'''),
    md(r'''
## 7B. Subnational MICS surveys

Kenya has no national MICS. Five MICS surveys cover single areas: Mombasa informal settlements
(2009, MICS4), Nyanza province (2011, MICS4) and Bungoma, Kakamega and Turkana counties (2013–14,
MICS5). They are harmonised with the DHS definitions (never heard of FGM → not cut; support =
"continued" among "continued" or "discontinued") and compared with DHS for the same area in the
prevalence (8A), risk (15C) and attitude (Part D) notebooks. MICS4 did not ask women their own
age at cutting. The MICS releases have no stratum variable, so the cluster is the PSU and
urban/rural the stratum.
'''),
    code(r'''
source(file.path(project_root, "00_Pipeline", "mics_core.R"))
mics_women <- harmonize_mics(file.path(project_root, "02_Data", "raw", "kenya_FGM_Data", "extracted", "MICS"))
saveRDS(mics_women, file.path(harmonized_dir, paste0(country_iso3, "_mics_subnational_women.rds")))
mics_records <- mics_women |> dplyr::group_by(survey = source) |>
  dplyr::summarise(women = dplyr::n(), status_known = sum(!is.na(fgm_status)), women_cut = sum(fgm_status %in% 1),
                   cut_with_age = sum(!is.na(fgm_age)), attitude_continue_or_stop = sum(!is.na(supports_continuation)), .groups = "drop")
readr::write_csv(mics_records, file.path(output_dir, paste0(country_iso3, "_mics_subnational_records.csv")))
IRdisplay::display(as.data.frame(mics_records))
'''),
]

# ---------------------------------------------------------------- Prevalence
PREV = [
    md(r'''
## 8A. Comparison with Kenya's subnational MICS

Prevalence among women 15–49 in each MICS area against DHS for the same area: Bungoma, Kakamega
and Turkana MICS 2013–14 against DHS 2014 for the same county; Nyanza MICS 2011 against DHS
2008–09 and 2014 for Nyanza province; Mombasa informal settlements MICS 2009 against DHS 2014 for
the whole of Mombasa county (indicative only, since informal settlements are a subset).
'''),
    code(COMMON + r'''
mics_out <- file.path(output_dir, "07_MICS_Comparison")
dir.create(mics_out, recursive = TRUE, showWarnings = FALSE)
mics_prev <- mics_vs_dhs(mics_women, h_women, "fgm_status")
mics_prev_diff <- dplyr::bind_rows(lapply(unique(mics_prev$comparison), function(cmp) {
  x <- mics_prev |> dplyr::filter(comparison == cmp)
  m <- x |> dplyr::filter(source == cmp); d <- x |> dplyr::filter(source != cmp) |> dplyr::slice_tail(n = 1)
  diff_test(m, d) |> dplyr::mutate(comparison = cmp, versus = d$source, .before = 1)
})) |> dplyr::mutate(dplyr::across(c(difference, lower, upper), pct1))
mics_prev_table <- mics_prev |> dplyr::transmute(comparison, area, source, prevalence = pct1(estimate), lower_ci = pct1(lower_ci),
                                                 upper_ci = pct1(upper_ci), n)
readr::write_csv(mics_prev_table, file.path(mics_out, paste0(country_iso3, "_mics_prevalence_by_source.csv")))
readr::write_csv(mics_prev_diff, file.path(mics_out, paste0(country_iso3, "_mics_prevalence_difference_test.csv")))
IRdisplay::display(as.data.frame(mics_prev_table)); IRdisplay::display(as.data.frame(mics_prev_diff))

mics_prev_plot <- ggplot2::ggplot(mics_prev |> dplyr::mutate(programme = ifelse(grepl("^MICS", source), "MICS", source)),
                                  ggplot2::aes(programme, 100 * estimate, colour = programme)) +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = 100 * lower_ci, ymax = 100 * upper_ci), size = 0.7, linewidth = 1) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * estimate, 1), "%")), vjust = -1.2, size = 3.6, show.legend = FALSE) +
  ggplot2::facet_wrap(~comparison, nrow = 1, scales = "free_y") +
  ggplot2::scale_colour_manual(values = c(MICS = "#C0392B", "DHS 2008" = "#5B9BD5", "DHS 2014" = "#1F4E79"), guide = "none") +
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.2))) +
  ggplot2::labs(title = "FGM prevalence among women 15-49: subnational MICS compared with DHS for the same area",
                subtitle = "Design-based estimates with 95% CI", x = NULL, y = "Prevalence (ever cut)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), plot.subtitle = ggplot2::element_text(colour = "grey30"),
                 strip.text = ggplot2::element_text(face = "bold", size = 9), panel.grid.minor = ggplot2::element_blank())
print(mics_prev_plot)
ggplot2::ggsave(file.path(mics_out, paste0(country_iso3, "_mics_prevalence_by_source.png")), mics_prev_plot,
                width = 16, height = 6, dpi = 300, bg = "white")
'''),
    md(r'''
## 8B. County prevalence (47 counties), DHS 2014 and 2022

Counties are recorded only in DHS 2014 and 2022. Design-based county estimates are smoothed with
an area-level BYM2 model (direct logit estimate with its sampling variance as known precision),
separately for each round, for women 15–49 and girls 0–14.
'''),
    code(COMMON + r'''
county_out <- file.path(output_dir, "08_County_Level")
dir.create(county_out, recursive = TRUE, showWarnings = FALSE)
county_b <- county_boundaries(project_root)
h_daughters <- readRDS(file.path(harmonized_path, paste0(country_iso3, "_daughters_all_rounds.rds")))
county_women <- county_series(h_women, "fgm_status", c("DHS 2014", "DHS 2022"), county_b)
county_girls <- county_series(h_daughters, "daughter_status", c("DHS 2014", "DHS 2022"), county_b)
readr::write_csv(county_women, file.path(county_out, paste0(country_iso3, "_county_prevalence_women.csv")))
readr::write_csv(county_girls, file.path(county_out, paste0(country_iso3, "_county_prevalence_girls.csv")))
IRdisplay::display(as.data.frame(county_women |> dplyr::group_by(survey_id) |> dplyr::slice_max(smoothed, n = 8) |>
  dplyr::mutate(dplyr::across(c(direct, smoothed, lower_ci, upper_ci), pct1))))
for (spec in list(list(county_women, "women 15-49", "women"), list(county_girls, "girls 0-14", "girls"))) {
  p <- county_map(spec[[1]], county_b, paste("FGM prevalence among", spec[[2]], "by county, Kenya"),
                  "DHS 2014 and 2022; BYM2-smoothed design-based estimates", "Prevalence")
  print(p)
  ggplot2::ggsave(file.path(county_out, paste0(country_iso3, "_county_prevalence_", spec[[3]], ".png")), p,
                  width = 13, height = 7, dpi = 300, bg = "white")
}
'''),
]

# ---------------------------------------------------------------- Risk
RISK = [
    md(r'''
## 15C. Comparison with Kenya's subnational MICS: age at cutting

MICS 2013–14 asked women their age at cutting in Bungoma, Kakamega and Turkana (MICS4 did not).
FGM is rare in these counties (2–4% of women), so the comparison with DHS 2014 rests on few cut
women and is indicative only. Ages are shown as reported.
'''),
    code(COMMON + r'''
mics_out <- file.path(project_root, "03_Outputs", country_iso3, "Risk", "09_MICS_Comparison")
dir.create(mics_out, recursive = TRUE, showWarnings = FALSE)
age_cat <- function(age_report, a) dplyr::case_when(
  age_report %in% c("age 0", "during infancy") | (age_report == "specific age 1+" & a <= 4) ~ "0-4",
  age_report == "specific age 1+" & a <= 9 ~ "5-9", age_report == "specific age 1+" & a <= 14 ~ "10-14",
  age_report == "specific age 1+" ~ "15+", TRUE ~ "Don't know / missing")
mics5 <- mics_surveys |> dplyr::filter(round == "MICS5")
age_rows <- purrr::pmap_dfr(mics5, function(mics_id, folder, year, round, area, level, dhs_compare) dplyr::bind_rows(
  mics_women |> dplyr::filter(source == mics_id, fgm_status == 1) |> dplyr::transmute(comparison = mics_id, source = "MICS 2013-14", weight, age_report, fgm_age),
  dhs_area(h_women, "DHS 2014", area, level) |> dplyr::filter(fgm_status == 1) |>
    dplyr::transmute(comparison = mics_id, source = "DHS 2014", weight, age_report, fgm_age)))
age_table <- age_rows |> dplyr::mutate(age_group = factor(age_cat(age_report, fgm_age), levels = c("0-4", "5-9", "10-14", "15+", "Don't know / missing"))) |>
  dplyr::group_by(comparison, source, age_group) |> dplyr::summarise(w = sum(weight), n = dplyr::n(), .groups = "drop_last") |>
  dplyr::mutate(pct = round(100 * w / sum(w), 1), cut_women = sum(n)) |> dplyr::ungroup() |>
  dplyr::select(comparison, source, cut_women, age_group, pct) |>
  tidyr::pivot_wider(names_from = age_group, values_from = pct, values_fill = 0)
national_ages <- h_women |> dplyr::filter(fgm_status == 1, survey_id %in% c("DHS 2014", "DHS 2022"), !is.na(age_report)) |>
  dplyr::mutate(age_group = age_cat(age_report, fgm_age)) |> dplyr::group_by(survey_id, age_group) |>
  dplyr::summarise(w = sum(weight), .groups = "drop_last") |> dplyr::mutate(pct = round(100 * w / sum(w), 1)) |>
  dplyr::select(-w) |> tidyr::pivot_wider(names_from = age_group, values_from = pct)
readr::write_csv(age_table, file.path(mics_out, paste0(country_iso3, "_mics_age_at_cutting.csv")))
readr::write_csv(national_ages, file.path(mics_out, paste0(country_iso3, "_dhs_national_age_at_cutting.csv")))
IRdisplay::display(as.data.frame(age_table)); IRdisplay::display(as.data.frame(national_ages))
'''),
]

# ---------------------------------------------------------------- Attitudes
ATT = [
    md(r'''
# Part D — Subnational MICS comparison and county estimates

**D.1 MICS.** Women's support for continuing FGM in each MICS area against DHS for the same area
(as in the prevalence notebook, section 8A). **D.2 Counties.** BYM2-smoothed county support from
DHS 2014 and 2022 for women, and pooled 2014 and 2022 for men (smaller samples).
'''),
    code(COMMON + r'''
att_out <- file.path(project_root, "03_Outputs", country_iso3, "Attitude_Women_and_Men", "MICS_and_County")
dir.create(att_out, recursive = TRUE, showWarnings = FALSE)
mics_att <- mics_vs_dhs(mics_women, h_women, "supports_continuation")
mics_att_table <- mics_att |> dplyr::transmute(comparison, area, source, support = pct1(estimate), lower_ci = pct1(lower_ci),
                                               upper_ci = pct1(upper_ci), n)
readr::write_csv(mics_att_table, file.path(att_out, paste0(country_iso3, "_mics_support_by_source.csv")))
IRdisplay::display(as.data.frame(mics_att_table))
'''),
    code(r'''
county_b <- county_boundaries(project_root)
h_men <- readRDS(file.path(harmonized_path, paste0(country_iso3, "_men_all_rounds.rds")))
county_support_women <- county_series(h_women, "supports_continuation", c("DHS 2014", "DHS 2022"), county_b)
county_support_men <- county_smooth(county_direct(h_men |> dplyr::filter(survey_id %in% c("DHS 2014", "DHS 2022")),
                                                  "supports_continuation"), county_b) |>
  dplyr::mutate(survey_id = "DHS 2014 and 2022 pooled")
readr::write_csv(county_support_women, file.path(att_out, paste0(country_iso3, "_county_support_women.csv")))
readr::write_csv(county_support_men, file.path(att_out, paste0(country_iso3, "_county_support_men.csv")))
IRdisplay::display(as.data.frame(county_support_women |> dplyr::group_by(survey_id) |> dplyr::slice_max(smoothed, n = 6) |>
  dplyr::mutate(dplyr::across(c(direct, smoothed), pct1))))
IRdisplay::display(as.data.frame(county_support_men |> dplyr::slice_max(smoothed, n = 6) |> dplyr::mutate(dplyr::across(c(direct, smoothed), pct1))))
lims <- c(0, max(c(county_support_women$smoothed, county_support_men$smoothed), na.rm = TRUE))
p_w <- county_map(county_support_women, county_b, "Women's support for continuing FGM by county, Kenya",
                  "DHS 2014 and 2022; BYM2-smoothed design-based estimates", "Support", lims)
p_m <- county_map(county_support_men, county_b, "Men's support for continuing FGM by county, Kenya",
                  "DHS 2014 and 2022 pooled; BYM2-smoothed design-based estimates", "Support", lims)
print(p_w); print(p_m)
ggplot2::ggsave(file.path(att_out, paste0(country_iso3, "_county_support_women.png")), p_w, width = 13, height = 7, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(att_out, paste0(country_iso3, "_county_support_men.png")), p_m, width = 8, height = 7, dpi = 300, bg = "white")
'''),
]

TARGETS = {
    "00_EDA_All_Rounds.ipynb": (EDA, "## 8. Summary of results"),
    "FGM_Prevalence_Incidence.ipynb": (PREV, "## Results"),
    "FGM_Risk_Incidence.ipynb": (RISK, "## 16. Optional machine learning cross check"),
    "FGM_Attitudinal_Incidence.ipynb": (ATT, "## Results"),
}
for name, (cells, before) in TARGETS.items():
    path = os.path.join(NB, name)
    nb = json.load(open(path))
    nb["cells"] = [c for c in nb["cells"] if TAG not in c.get("metadata", {}).get("tags", [])]
    hits = [i for i, c in enumerate(nb["cells"]) if c["cell_type"] == "markdown" and "".join(c["source"]).lstrip().startswith(before)]
    idx = hits[0] if hits else len(nb["cells"])
    nb["cells"][idx:idx] = cells
    json.dump(nb, open(path, "w"), indent=1, ensure_ascii=False)
    open(path, "a").write("\n")
    print(f"{name}: inserted {len(cells)} cells before '{before}'" + ("" if hits else " (appended at end)"))
