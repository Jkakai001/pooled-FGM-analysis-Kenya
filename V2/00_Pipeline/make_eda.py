import json, uuid, re, sys

S = sys.argv[1]
OUT = sys.argv[2]
h = open(f"{S}/harmonize.R").read()

def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]
def md(t): return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(t)}
def code(t): return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "execution_count": None, "metadata": {}, "outputs": [], "source": lines(t)}

# Split the tested harmonization script into its sections.
sec = {}
for block in re.split(r"\n(?=# ---- )", h):
    title = block.split("\n")[0]
    sec[title] = block
def get(prefix):
    k = [t for t in sec if t.startswith(prefix)]
    assert len(k) == 1, (prefix, k)
    return sec[k[0]]

helpers = get("# ---- Harmonization helpers")
inventory = get("# ---- Survey inventory")
dhs = get("# ---- DHS women and daughters")
men = get("# ---- DHS men")
run_all = get("# ---- Run every round")
setup_block = get("# ---- Harmonize all")
setup_block = setup_block.split("\n", 1)[1]  # drop title line

cells = [
md("""
# FGM Data Preparation and EDA, Kenya, all survey rounds (V2)

This notebook harmonizes every Kenya DHS round with an FGM module into one consistent dataset,
then describes it. It replaces the single-round (DHS 2022) preparation used in V1. Kenya has no
national MICS; its MICS surveys cover single areas (Mombasa informal settlements 2009, Nyanza
2011, Bungoma, Kakamega and Turkana 2013–14) and are compared separately, not pooled.

| Survey | Women: own status | Women: age at cutting | Daughters | Women's attitude | Men's attitude |
|---|---|---|---|---|---|
| DHS 1998 | yes (asked of all; no "heard of FGM" question) | yes | eldest daughter only (not used) | yes (no "depends") | not asked |
| DHS 2003 | yes (asked of all) | not asked | eldest daughter only (not used) | not asked | men's file not available |
| DHS 2008–09 | yes | yes | most recently cut daughter only | yes | not asked |
| DHS 2014 | yes (long-questionnaire subsample) | yes | full roster (0–14) | yes | yes |
| DHS 2022 | yes (long-questionnaire subsample) | yes | full roster (0–14) | yes | yes |

Definitions corrected relative to V1:

1. **"During infancy" is spread over early childhood.** DHS code 95 ("during infancy",
   available in DHS 2008–09, 2014 and 2022) is spread over ages 0–4, in proportion to the ages
   reported by women of the same survey and birth cohort who gave an exact age. DHS 1998 has
   no infancy code. Few Kenyan women give this answer, because most were cut after infancy.
2. **Never-heard women are not cut.** Women who have never heard of FGM are not asked the
   status question. Following DHS convention, they and their daughters aged 0–14 are counted
   as not cut (daughters added from the birth history). DHS 1998 and 2003 asked every woman
   her status directly, without a "heard of FGM" question.
3. **Unknown ages are estimated; unknown status is excluded.** When a cut woman or daughter
   has a "don't know", missing or inconsistent age at cutting, the case is spread over ages 0
   to her current age, using exact-age reporters from the same survey and cohort (or the
   whole survey if the cohort has fewer than 30). No cut case is dropped. Women who don't
   know whether they were cut are excluded. Sections 4–6 report the counts, and section 7.1
   shows how ages are estimated.

Weights are rescaled to mean one within each survey, so pooled analyses weight each
round by its sample size; each survey keeps its own clusters and strata. In DHS 2014 and 2022
the FGM questions were asked only in the long-questionnaire subsample (about half of women);
women outside it have no FGM data and are not counted.
"""),
md("## 0. Configuration"),
code("""
country_name <- "Kenya"
country_iso3 <- "KEN"

project_root <- if (dir.exists("../02_Data")) ".." else "."
processed_dir <- file.path(project_root, "02_Data", "processed", country_iso3)
harmonized_dir <- file.path(processed_dir, "harmonized")
analysis_dir <- file.path(processed_dir, "analysis")
output_dir <- file.path(project_root, "03_Outputs", country_iso3, "EDA")
for (d in c(harmonized_dir, analysis_dir, output_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

options(repr.plot.width = 13, repr.plot.height = 7, width = 200)
"""),
md("## 1. Setup and survey inventory"),
code(setup_block + "\n\n" + inventory.split("\n", 1)[1]
     + "\n\nrounds |> dplyr::mutate(women_file = basename(ir_file), men_file = basename(mr_file)) |>\n  dplyr::select(survey_id, survey_type, round_year, women_file, men_file)"),
md("""
## 2. Harmonization rules

Shared coding applied to every round: the 8 former provinces (DHS 2022 records only counties,
which are mapped to their former province; DHS 1998 did not sample North Eastern), the 47
counties for DHS 2014 and 2022, ethnicity grouped into the ten large groups coded in every round
plus "other", religion, education and wealth collapsed to categories every survey can supply,
and age at cutting coded with infancy as age 0.
"""),
code(helpers.split("\n", 1)[1]),
md("""
### 2.1 DHS loader

Own status uses `s1002` (1998), `s821` (2003) and `g102` (2008–09, 2014, 2022). From 2008–09,
women who never heard of FGM (`g100` = no and `g101` = no) are not cut. Age at cutting uses
`s1003` (1998) and `g106`; DHS 2003 did not ask it. Attitude uses `s1012` (1998) and `g119`.
Daughters come from the full roster (2014, 2022); daughters 0–14 of never-heard mothers are
added from the birth history as not cut.
"""),
code(dhs.split("\n", 1)[1]),
md("### 2.2 DHS men loader (DHS 2014 and 2022)"),
code(men.split("\n", 1)[1]),
md("### 2.3 Harmonize all rounds"),
code(run_all.split("\n", 1)[1] + """

harmonized <- harmonize_all_rounds()
women <- harmonized$women
daughters <- harmonized$daughters
recent_daughters <- harmonized$recent
men <- harmonized$men

saveRDS(women, file.path(harmonized_dir, paste0(country_iso3, "_women_all_rounds.rds")))
saveRDS(daughters, file.path(harmonized_dir, paste0(country_iso3, "_daughters_all_rounds.rds")))
saveRDS(recent_daughters, file.path(harmonized_dir, paste0(country_iso3, "_most_recent_cut_daughter.rds")))
saveRDS(men, file.path(harmonized_dir, paste0(country_iso3, "_men_all_rounds.rds")))

cat("Women:", nrow(women), "| Daughters 0-14:", nrow(daughters),
    "| Most recently cut daughters:", nrow(recent_daughters), "| Men:", nrow(men), "\\n")
"""),
md("""
## 3. Total unweighted records

Counts are unweighted. DHS 2014 and 2022 asked the FGM module only in the long-questionnaire
subsample, so "FGM module asked" is about half of women interviewed in those rounds.
"Status known" includes never-heard women counted as not cut.
"""),
code("""
round_levels <- rounds |> dplyr::arrange(round_year) |> dplyr::pull(survey_id)

women_counts <- women |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(
    women_interviewed = dplyr::n(),
    fgm_module_asked = sum(module_asked),
    status_known = sum(!is.na(fgm_status)),
    never_heard_counted_uncut = sum(status_reported == "never heard (uncut)"),
    status_dont_know = sum(status_reported == "don't know"),
    women_cut = sum(fgm_status == 1, na.rm = TRUE),
    attitude_continue_or_stop = sum(!is.na(supports_continuation)),
    .groups = "drop"
  )
daughter_counts <- daughters |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(daughters_0_14 = dplyr::n(),
                   daughters_of_never_heard_mothers = sum(source != "roster"),
                   daughters_cut = sum(daughter_status == 1), .groups = "drop")
recent_counts <- recent_daughters |> dplyr::count(survey_id, name = "most_recently_cut_daughter")
men_counts <- men |> dplyr::group_by(survey_id) |>
  dplyr::summarise(men_15_49 = dplyr::n(), men_attitude_continue_or_stop = sum(!is.na(supports_continuation)),
                   .groups = "drop")

record_counts <- tibble::tibble(survey_id = round_levels) |>
  dplyr::left_join(women_counts, by = "survey_id") |>
  dplyr::left_join(daughter_counts, by = "survey_id") |>
  dplyr::left_join(recent_counts, by = "survey_id") |>
  dplyr::left_join(men_counts, by = "survey_id") |>
  dplyr::mutate(dplyr::across(-survey_id, ~ tidyr::replace_na(as.integer(.x), 0L)))
record_counts <- dplyr::bind_rows(
  record_counts,
  record_counts |> dplyr::summarise(survey_id = "Total", dplyr::across(-survey_id, sum))
)

readr::write_csv(record_counts, file.path(output_dir, paste0(country_iso3, "_records_by_round.csv")))
IRdisplay::display(as.data.frame(record_counts))
"""),
md("""
## 4. Direct prevalence by round

Survey-weighted prevalence with design-based 95% intervals, for checking each round
against its published report before any modelling.
"""),
code("""
svy_prevalence <- function(data, outcome) {
  data |>
    dplyr::filter(!is.na(.data[[outcome]])) |>
    dplyr::group_split(survey_id) |>
    purrr::map_dfr(function(d) {
      design <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
      est <- survey::svymean(stats::as.formula(paste0("~", outcome)), design)
      tibble::tibble(survey_id = d$survey_id[1], n = nrow(d),
                     prevalence = as.numeric(stats::coef(est)), se = as.numeric(survey::SE(est)))
    }) |>
    dplyr::mutate(lower_ci = pmax(prevalence - 1.96 * se, 0), upper_ci = pmin(prevalence + 1.96 * se, 1))
}

options(survey.lonely.psu = "adjust")
prevalence_by_round <- dplyr::bind_rows(
  svy_prevalence(women, "fgm_status") |> dplyr::mutate(population = "Women 15-49"),
  svy_prevalence(daughters |> dplyr::rename(fgm_daughter = daughter_status), "fgm_daughter") |>
    dplyr::mutate(population = "Daughters 0-14")
) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels))

readr::write_csv(prevalence_by_round, file.path(output_dir, paste0(country_iso3, "_direct_prevalence_by_round.csv")))
IRdisplay::display(as.data.frame(prevalence_by_round |> dplyr::arrange(population, survey_id) |>
  dplyr::mutate(dplyr::across(c(prevalence, lower_ci, upper_ci), ~ round(100 * .x, 1)))))
"""),
md("""
## 5. Reporting of cutting at infancy

Share of cut women (own report) and of cut daughters (mother's report) whose reported
age at cutting is during infancy, combining the explicit infancy code (DHS 2008–09, 2014 and
2022, code 95) with a reported age of 0. Denominators are all cut cases where the age question
was asked, so "don't know" answers stay in the denominator. DHS 2003 did not ask women's age
at cutting. DHS 2008–09 records the age only for the most recently cut daughter; it is shown
separately.
"""),
code("""
age_report_levels <- c("during infancy", "age 0", "specific age 1+", "don't know", "inconsistent", "missing")

summarise_infancy <- function(data, group_label) {
  data |>
    dplyr::filter(age_report %in% age_report_levels) |>
    dplyr::group_by(survey_id) |>
    dplyr::summarise(
      cut_cases = dplyr::n(),
      reported_during_infancy = sum(age_report == "during infancy"),
      reported_age_0 = sum(age_report == "age 0"),
      age_unknown = sum(age_report %in% c("don't know", "inconsistent", "missing")),
      pct_infancy_unweighted = 100 * mean(age_report %in% c("during infancy", "age 0")),
      pct_infancy_weighted = 100 * stats::weighted.mean(age_report %in% c("during infancy", "age 0"), weight),
      pct_infancy_among_known_age = 100 * mean(age_report[age_report %in% c("during infancy", "age 0", "specific age 1+")]
                                                %in% c("during infancy", "age 0")),
      .groups = "drop"
    ) |>
    dplyr::mutate(reporter = group_label)
}

infancy_table <- dplyr::bind_rows(
  summarise_infancy(women |> dplyr::filter(fgm_status == 1), "Women: own cutting"),
  summarise_infancy(daughters |> dplyr::filter(daughter_status == 1), "Mothers: daughters 0-14 (full roster)"),
  summarise_infancy(recent_daughters, "Mothers: most recently cut daughter")
) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels)) |>
  dplyr::arrange(reporter, survey_id) |>
  dplyr::relocate(reporter)

readr::write_csv(infancy_table, file.path(output_dir, paste0(country_iso3, "_infancy_reporting_by_round.csv")))
IRdisplay::display(as.data.frame(infancy_table |> dplyr::mutate(dplyr::across(dplyr::starts_with("pct"), ~ round(.x, 1)))))
"""),
code("""
infancy_plot_data <- infancy_table |>
  dplyr::left_join(rounds |> dplyr::select(survey_id, survey_type, round_year), by = "survey_id")

infancy_plot <- ggplot2::ggplot(infancy_plot_data,
    ggplot2::aes(round_year, pct_infancy_weighted / 100, color = reporter, shape = survey_type)) +
  ggplot2::geom_line(ggplot2::aes(group = reporter), linewidth = 0.8, alpha = 0.6) +
  ggplot2::geom_point(size = 3.4) +
  ggrepel::geom_text_repel(ggplot2::aes(label = scales::percent(pct_infancy_weighted / 100, accuracy = 1)),
                           size = 3.2, show.legend = FALSE, seed = 1) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  ggplot2::scale_x_continuous(breaks = sort(unique(rounds$round_year))) +
  ggplot2::scale_color_manual(values = c("Women: own cutting" = "#1D4E89",
                                         "Mothers: daughters 0-14 (full roster)" = "#C0392B",
                                         "Mothers: most recently cut daughter" = "#E5A11C")) +
  ggplot2::labs(title = paste("Cutting Reported at Infancy, by Survey Round,", country_name),
                subtitle = "Weighted share of cut cases reported during infancy or at age 0 (don't know kept in the denominator)",
                x = "Survey year", y = "Reported cut in infancy", color = NULL, shape = "Survey") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35"),
                 legend.position = "bottom", legend.box = "vertical")

print(infancy_plot)
ggplot2::ggsave(file.path(output_dir, paste0(country_iso3, "_infancy_reporting_by_round.png")),
                infancy_plot, width = 12, height = 7, dpi = 300, bg = "white")
"""),
md("""
### 5.1 Reported age at cutting, all rounds

Distribution of reported age at cutting among cut cases, by reporter and round.
"""),
code("""
age_group_of <- function(age_report, age) {
  dplyr::case_when(
    age_report %in% c("during infancy", "age 0") ~ "Infancy / age 0",
    age_report == "specific age 1+" & age <= 4 ~ "1-4",
    age_report == "specific age 1+" & age <= 9 ~ "5-9",
    age_report == "specific age 1+" & age <= 14 ~ "10-14",
    age_report == "specific age 1+" ~ "15+",
    TRUE ~ "Don't know / missing"
  )
}
age_distribution <- dplyr::bind_rows(
  women |> dplyr::filter(fgm_status == 1, age_report %in% age_report_levels) |>
    dplyr::transmute(survey_id, weight, reporter = "Women: own cutting", group = age_group_of(age_report, fgm_age)),
  daughters |> dplyr::filter(daughter_status == 1) |>
    dplyr::transmute(survey_id, weight, reporter = "Mothers: daughters 0-14", group = age_group_of(age_report, daughter_age_at_cut)),
  recent_daughters |> dplyr::transmute(survey_id, weight, reporter = "Mothers: daughters 0-14",
                                       group = age_group_of(age_report, daughter_age_at_cut))
) |>
  dplyr::group_by(reporter, survey_id, group) |>
  dplyr::summarise(w = sum(weight), n = dplyr::n(), .groups = "drop_last") |>
  dplyr::mutate(share = w / sum(w)) |> dplyr::ungroup() |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels),
                group = factor(group, levels = c("Infancy / age 0", "1-4", "5-9", "10-14", "15+", "Don't know / missing")))

age_distribution_plot <- ggplot2::ggplot(age_distribution, ggplot2::aes(survey_id, share, fill = group)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::facet_wrap(~reporter, ncol = 1) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_manual(values = c("Infancy / age 0" = "#7A0177", "1-4" = "#C51B8A", "5-9" = "#F768A1",
                                        "10-14" = "#FBB4B9", "15+" = "#FEEBE2", "Don't know / missing" = "grey70")) +
  ggplot2::labs(title = paste("Reported Age at Cutting by Survey Round,", country_name),
                x = NULL, y = "Share of cut cases (weighted)", fill = "Reported age") +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                 strip.text = ggplot2::element_text(face = "bold", size = 12),
                 axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
print(age_distribution_plot)
ggplot2::ggsave(file.path(output_dir, paste0(country_iso3, "_age_at_cutting_distribution_by_round.png")),
                age_distribution_plot, width = 12, height = 9, dpi = 300, bg = "white")
readr::write_csv(age_distribution, file.path(output_dir, paste0(country_iso3, "_age_at_cutting_distribution_by_round.csv")))
"""),
md("""
## 6. Missing age at FGM, by current age

Percentage of cut women and girls whose age at cutting is "don't know", inconsistent or
missing, by current age, pooling every round that asked the age question (DHS 2003 women
and the most-recent-daughter records, which have no current age, are excluded).
Point size is the number of cut cases at that age.
"""),
code("""
missing_age_data <- dplyr::bind_rows(
  women |> dplyr::filter(fgm_status == 1, age_report %in% age_report_levels) |>
    dplyr::transmute(survey_id, current_age = age, missing_age = age_report %in% c("don't know", "inconsistent", "missing"),
                     reporter = "Women (own report)"),
  daughters |> dplyr::filter(daughter_status == 1) |>
    dplyr::transmute(survey_id, current_age = daughter_age,
                     missing_age = age_report %in% c("don't know", "inconsistent", "missing"),
                     reporter = "Girls (mother's report)")
)

missing_by_age <- missing_age_data |>
  dplyr::group_by(current_age, reporter) |>
  dplyr::summarise(n = dplyr::n(), pct_missing = mean(missing_age), .groups = "drop")

# Y axis 0-10% in 2.5% steps, extended when a pooled share is higher.
y_top <- max(0.1, ceiling(max(missing_by_age$pct_missing) / 0.025) * 0.025)
missing_age_plot <- ggplot2::ggplot(missing_by_age, ggplot2::aes(current_age, pct_missing)) +
  ggplot2::geom_point(ggplot2::aes(size = n), color = "#B2182B") +
  ggplot2::scale_size_area(name = "# of observations", max_size = 7, breaks = scales::pretty_breaks(4)) +
  ggplot2::scale_x_continuous(breaks = 0:49, expand = ggplot2::expansion(add = 1)) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = seq(0, y_top, 0.025),
                              limits = c(0, y_top), expand = c(0, 0)) +
  ggplot2::labs(title = "Missing age-at-FGM",
                subtitle = paste0("All rounds with the age question, ", country_name, " (",
                                  format(nrow(missing_age_data), big.mark = ","), " cut women and girls)"),
                x = "Current age of women/girl", y = "Percentage of missing age-at-FGM") +
  ggplot2::theme_bw(base_size = 13) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 22, color = "#0B1F33"),
                 plot.subtitle = ggplot2::element_text(size = 14, color = "grey25", margin = ggplot2::margin(b = 12)),
                 axis.text.x = ggplot2::element_text(size = 8, color = "grey30"),
                 axis.text.y = ggplot2::element_text(size = 11, color = "grey30"),
                 axis.title = ggplot2::element_text(size = 13, color = "grey15"),
                 panel.grid.major = ggplot2::element_line(color = "grey92", linewidth = 0.4),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_line(color = "grey96", linewidth = 0.3),
                 panel.border = ggplot2::element_rect(color = "grey35", fill = NA, linewidth = 0.5),
                 legend.title = ggplot2::element_text(size = 12), legend.text = ggplot2::element_text(size = 11),
                 plot.margin = ggplot2::margin(15, 15, 10, 15))
print(missing_age_plot)
ggplot2::ggsave(file.path(output_dir, paste0(country_iso3, "_missing_age_at_fgm_by_current_age.png")),
                missing_age_plot, width = 12, height = 6, dpi = 300, bg = "white")

missing_by_round <- missing_age_data |>
  dplyr::group_by(survey_id, reporter) |>
  dplyr::summarise(cut_cases = dplyr::n(), pct_missing = round(100 * mean(missing_age), 2), .groups = "drop") |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels)) |> dplyr::arrange(reporter, survey_id)
readr::write_csv(missing_by_age, file.path(output_dir, paste0(country_iso3, "_missing_age_at_fgm_by_current_age.csv")))
readr::write_csv(missing_by_round, file.path(output_dir, paste0(country_iso3, "_missing_age_at_fgm_by_round.csv")))
IRdisplay::display(as.data.frame(missing_by_round))
"""),
code("""
missing_age_round_plot <- missing_age_data |>
  dplyr::group_by(survey_id, current_age) |>
  dplyr::summarise(n = dplyr::n(), pct_missing = mean(missing_age), .groups = "drop") |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels)) |>
  ggplot2::ggplot(ggplot2::aes(current_age, pct_missing)) +
  ggplot2::geom_point(ggplot2::aes(size = n), color = "#B2182B", alpha = 0.8) +
  ggplot2::facet_wrap(~survey_id, ncol = 3) +
  ggplot2::scale_size_area(name = "# of observations", max_size = 4) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(title = "Missing age-at-FGM by current age, each survey round",
                x = "Current age of women/girl", y = "Percentage of missing age-at-FGM") +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
print(missing_age_round_plot)
ggplot2::ggsave(file.path(output_dir, paste0(country_iso3, "_missing_age_at_fgm_by_current_age_each_round.png")),
                missing_age_round_plot, width = 13, height = 10, dpi = 300, bg = "white")
"""),
md("""
### 6.1 Women answering "don't know"

Two different "don't know" answers, by round. **Own status** ("don't know" whether she was
cut) was not recorded in any Kenya round; women with a missing status are excluded from
prevalence and risk. **Age at cutting** ("don't know" among
cut women) is kept in risk incidence with the age estimated as described in section 7.1.
Percentages are survey-weighted.
"""),
code("""
dont_know <- women |>
  dplyr::mutate(status_asked = module_asked & !status_reported %in% c("never heard (uncut)", "module not asked"),
                cut = fgm_status %in% 1) |>
  dplyr::group_by(survey_id) |>
  dplyr::summarise(
    women_asked_own_status = sum(status_asked),
    status_dont_know = sum(status_reported == "don't know"),
    status_dont_know_pct = 100 * sum(weight[status_reported == "don't know"]) / sum(weight[status_asked]),
    women_cut = sum(cut),
    age_dont_know = sum(cut & age_report %in% "don't know"),
    age_dont_know_pct = 100 * sum(weight[cut & age_report %in% "don't know"]) / sum(weight[cut]),
    age_missing_or_inconsistent = sum(cut & age_report %in% c("missing", "inconsistent")),
    .groups = "drop") |>
  dplyr::mutate(age_dont_know = dplyr::if_else(survey_id == "DHS 2003", NA_integer_, age_dont_know),
                age_dont_know_pct = dplyr::if_else(survey_id == "DHS 2003", NA_real_, age_dont_know_pct),
                survey_id = factor(survey_id, levels = round_levels)) |>
  dplyr::arrange(survey_id)
readr::write_csv(dont_know, file.path(output_dir, paste0(country_iso3, "_women_dont_know_by_round.csv")))
IRdisplay::display(as.data.frame(dont_know |> dplyr::mutate(dplyr::across(dplyr::ends_with("_pct"), ~ round(.x, 1)))))
"""),
md("""
## 7. Analysis files

The downstream notebooks read these files from `02_Data/processed/KEN/analysis/`.

- **Risk incidence** (`fgm_events`): one time-to-event record per woman or daughter; uncut
  cases are censored at current age (women capped at 49). Daughters come only from
  full-roster rounds (DHS 2014, 2022). DHS 2003 women are excluded because the survey did
  not ask age at cutting, so risk incidence uses DHS 1998, 2008–09, 2014 and 2022.
- **Prevalence**: every woman and daughter 0–14 with a known status.
- **Attitudes**: women and men who answered "continue" or "stop" ("depends" and "don't
  know" excluded, as in V1).

### 7.1 Age at cutting when it was not reported exactly

Cut cases without an exact age are neither excluded nor all placed at a single age.
Following the standard approach, their age at cutting is estimated by applying the
distribution observed among respondents in the same survey who reported an exact age.
Strict exclusion would bias risk downward, and more so in older cohorts, whose recall is
weaker.

- **"During infancy"** (DHS code 95, DHS 2008–09, 2014 and 2022): spread over ages 0–4, the "before
  age 5" group in which DHS reports place infancy. Shares come from exact reports at ages
  0–4.
- **"Don't know", missing or inconsistent ages**, including ages later than the current
  age: spread over ages 0 to the current age (women up to 49, daughters up to 14). Shares
  come from all exact reports.
- **Same survey and birth cohort.** Exact reporters come from the same survey and birth
  cohort, because recall differs by cohort; the whole survey is used when the cohort has
  fewer than 30 exact reporters in the relevant age range.
- **No randomness.** Each such case is split into fractional records whose shares sum to
  one; each keeps the respondent's survey weight multiplied by its share (`age_share`), so
  the estimates are deterministic and reproducible.

DHS 1998 has no separate infancy code; its answers are exact ages. Reported ages of 0 are
kept as age 0.
"""),
code("""
event_fields <- c("id", "person_id", "record_type", "survey_id", "survey_type", "round_year", "region", "county", "cluster",
                  "strata", "urban_rural", "weight", "age", "birth_year", "birth_cohort", "ethnicity", "education",
                  "religion", "wealth", "fgm_event", "time", "age_share", "age_source")
min_exact_reporters <- 30L

# One harmonized table of cut and uncut histories, before any age estimation.
histories <- dplyr::bind_rows(
  women |> dplyr::filter(!is.na(fgm_status), survey_id != "DHS 2003") |>
    dplyr::transmute(person_id = resp_id, record_type = "self", survey_id, survey_type, round_year, region, county, cluster,
                     strata, urban_rural, weight, age, birth_year, birth_cohort, ethnicity, education, religion,
                     wealth, fgm_event = fgm_status, age_cut = fgm_age, age_report, max_cut_age = max_age_years),
  daughters |>
    dplyr::transmute(person_id = daughter_id, record_type = "daughter", survey_id, survey_type, round_year, region,
                     county, cluster, strata, urban_rural, weight, age = daughter_age, birth_year, birth_cohort, ethnicity,
                     education, religion, wealth, fgm_event = daughter_status, age_cut = daughter_age_at_cut,
                     age_report, max_cut_age = max_daughter_age)
) |>
  dplyr::filter(!is.na(birth_cohort), !is.na(region)) |>
  dplyr::mutate(age_source = dplyr::case_when(
    fgm_event == 0 ~ "not cut",
    # DHS code 95 is a vague answer; MICS 2011 records infancy as "00" (the first year of
    # life), which is its only age-0 code, so it is kept as an exact age 0.
    age_report == "during infancy" & survey_type == "DHS" ~ "during infancy (spread over 0-4)",
    (age_report %in% c("age 0", "specific age 1+") | (age_report == "during infancy" & survey_type == "MICS")) &
      age_cut <= age ~ "exact age reported",
    TRUE ~ "unknown age (spread over 0 to current age)"))

exact_reports <- histories |> dplyr::filter(age_source == "exact age reported")

# Spread each case over its admissible ages using exact reporters of the same survey
# (and birth cohort when it has enough exact reporters in that age range).
spread_age_at_cutting <- function(cases, upper_age) {
  if (!nrow(cases)) return(tibble::tibble())
  cohort_counts <- exact_reports |> dplyr::group_by(record_type, survey_id, birth_cohort, age_cut) |>
    dplyr::summarise(w_cohort = sum(weight), n_cohort = dplyr::n(), .groups = "drop")
  survey_counts <- exact_reports |> dplyr::group_by(record_type, survey_id, age_cut) |>
    dplyr::summarise(w_survey = sum(weight), n_survey = dplyr::n(), .groups = "drop")
  cases |>
    dplyr::mutate(top_age = as.integer(pmin(upper_age, floor(age), max_cut_age))) |>
    tidyr::uncount(top_age + 1L, .id = "k") |>
    dplyr::mutate(age_cut = k - 1) |>
    dplyr::left_join(cohort_counts, by = c("record_type", "survey_id", "birth_cohort", "age_cut")) |>
    dplyr::left_join(survey_counts, by = c("record_type", "survey_id", "age_cut")) |>
    dplyr::mutate(dplyr::across(c(w_cohort, n_cohort, w_survey, n_survey), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::group_by(person_id) |>
    dplyr::mutate(source_level = dplyr::case_when(sum(n_cohort) >= min_exact_reporters ~ "cohort",
                                                  sum(n_survey) > 0 ~ "survey", TRUE ~ "uniform"),
                  age_share = dplyr::case_when(source_level == "cohort" ~ w_cohort / sum(w_cohort),
                                               source_level == "survey" ~ w_survey / sum(w_survey),
                                               TRUE ~ 1 / dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::filter(age_share > 0) |>
    dplyr::select(-k, -top_age, -w_cohort, -n_cohort, -w_survey, -n_survey)
}

spread_infancy <- spread_age_at_cutting(histories |> dplyr::filter(age_source == "during infancy (spread over 0-4)"), 4L)
spread_unknown <- spread_age_at_cutting(histories |> dplyr::filter(age_source == "unknown age (spread over 0 to current age)"), 49L)
stopifnot(all(abs(tapply(spread_infancy$age_share, spread_infancy$person_id, sum) - 1) < 1e-9),
          all(abs(tapply(spread_unknown$age_share, spread_unknown$person_id, sum) - 1) < 1e-9))

fgm_events <- dplyr::bind_rows(
  histories |> dplyr::filter(age_source %in% c("not cut", "exact age reported")) |>
    dplyr::mutate(age_share = 1, source_level = NA_character_),
  spread_infancy, spread_unknown
) |>
  dplyr::mutate(
    time = dplyr::if_else(fgm_event == 1, age_cut, pmin(age, dplyr::if_else(record_type == "self", max_age_years, age))),
    weight = weight * age_share,
    id = dplyr::if_else(age_share < 1, paste0(person_id, "-a", age_cut), person_id)
  ) |>
  dplyr::select(dplyr::all_of(event_fields), source_level)
stopifnot(!anyDuplicated(fgm_events$id), all(fgm_events$time >= 0), all(fgm_events$time <= fgm_events$age))

age_source_summary <- histories |>
  dplyr::filter(fgm_event == 1) |>
  dplyr::count(record_type, survey_id, age_source) |>
  tidyr::pivot_wider(names_from = age_source, values_from = n, values_fill = 0) |>
  dplyr::left_join(dplyr::bind_rows(spread_infancy, spread_unknown) |>
                     dplyr::distinct(person_id, record_type, survey_id, source_level) |>
                     dplyr::count(record_type, survey_id, source_level) |>
                     tidyr::pivot_wider(names_from = source_level, values_from = n, values_fill = 0,
                                        names_prefix = "distribution from: "),
                   by = c("record_type", "survey_id")) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels)) |>
  dplyr::arrange(dplyr::desc(record_type), survey_id)
readr::write_csv(age_source_summary, file.path(output_dir, paste0(country_iso3, "_risk_age_at_cutting_sources.csv")))
cat("Cut cases by how age at cutting enters risk incidence (DHS 2003 women excluded: age not asked)\n")
IRdisplay::display(as.data.frame(age_source_summary))


prevalence_self <- women |> dplyr::filter(!is.na(fgm_status)) |>
  dplyr::transmute(id = resp_id, survey_id, survey_type, round_year, region, county, cluster, strata, urban_rural, weight,
                   age, birth_year, birth_cohort, ethnicity, education, religion, wealth, fgm_self = fgm_status)
prevalence_daughter <- daughters |>
  dplyr::transmute(id = daughter_id, mother_id = resp_id, survey_id, survey_type, round_year, region, county, cluster, strata,
                   urban_rural, weight, age = daughter_age, birth_year, birth_cohort, ethnicity, education,
                   religion, wealth, fgm_daughter = daughter_status)
attitude_women <- women |> dplyr::filter(!is.na(supports_continuation)) |>
  dplyr::transmute(id = resp_id, survey_id, survey_type, round_year, region, county, cluster, strata, urban_rural, weight,
                   age, birth_year, birth_cohort, own_status = fgm_status, education, religion, wealth, ethnicity,
                   supports_continuation)
attitude_men <- men |> dplyr::filter(!is.na(supports_continuation)) |>
  dplyr::transmute(id = resp_id, survey_id, survey_type, round_year, region, county, cluster, strata, urban_rural, weight,
                   age, education, religion, wealth, ethnicity, supports_continuation)

analysis_files <- list(fgm_events = fgm_events, prevalence_self = prevalence_self,
                       prevalence_daughter = prevalence_daughter, attitude_women = attitude_women,
                       attitude_men = attitude_men)
for (nm in names(analysis_files)) {
  saveRDS(analysis_files[[nm]], file.path(analysis_dir, paste0(country_iso3, "_", nm, ".rds")))
}
IRdisplay::display(as.data.frame(tibble::tibble(
  file = paste0(country_iso3, "_", names(analysis_files), ".rds"),
  records = vapply(analysis_files, nrow, integer(1)),
  rounds = vapply(analysis_files, function(d) paste(sort(unique(d$survey_id)), collapse = ", "), character(1)))))
"""),
md("""
## 8. Notes for interpretation

- Cohort trends pool rounds by birth cohort. A cohort observed in several rounds is
  reported at different ages; time-to-event methods use each record only up to its
  observed age.
- Ethnicity is the respondent's own in every round. Wealth is unavailable in DHS 1998; models
  keep it as an "unknown" category for that round.
- DHS 1998 did not sample North Eastern province; DHS 2003 has no age at cutting or attitude
  question; men's attitudes are available only from DHS 2014 and 2022.
- Reported age at cutting is retrospective. In Kenya most cutting is reported at ages 5–14,
  so girls aged 0–14 are observed before much of their risk; daughters' estimates cover early
  ages only.
"""),
]

ref = json.load(open(f"{OUT}/../../V1/01_Notebooks/FGM_Risk_Incidence.ipynb"))
nb = {"cells": cells, "metadata": ref["metadata"], "nbformat": 4, "nbformat_minor": 5}
json.dump(nb, open(f"{OUT}/00_EDA_All_Rounds.ipynb", "w"), indent=1, ensure_ascii=False)
# Add the summary tables when this notebook's outputs already exist.
import os
sys.path.insert(0, S)
from eda_summary import insert_summary
eda_out = os.path.join(OUT, "..", "03_Outputs", "KEN", "EDA")
if os.path.exists(os.path.join(eda_out, "KEN_infancy_reporting_by_round.csv")):
    insert_summary(f"{OUT}/00_EDA_All_Rounds.ipynb", eda_out)
print("cells:", len(cells))
