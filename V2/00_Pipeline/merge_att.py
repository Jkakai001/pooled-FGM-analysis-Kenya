import json, os, sys, uuid

NB = sys.argv[1]
women = json.load(open(f"{NB}/FGM_Attitudinal_Incidence.ipynb"))
men = json.load(open(f"{NB}/FGM_Attitudinal_Incidence_Men.ipynb"))

def lines(t):
    p = t.strip("\n").split("\n")
    return [x + "\n" for x in p[:-1]] + [p[-1]]
def md(t): return {"cell_type": "markdown", "id": uuid.uuid4().hex[:8], "metadata": {}, "source": lines(t)}
def code(t): return {"cell_type": "code", "id": uuid.uuid4().hex[:8], "execution_count": None, "metadata": {}, "outputs": [], "source": lines(t)}
def src(c): return "".join(c["source"])

intro = md("""
# FGM Attitudinal Incidence, Women and Men, Kenya, all survey rounds (V2)

Support for continuing FGM, from every Kenya survey that asked about it. Run
`00_EDA_All_Rounds.ipynb` first.

- **Part A — Women** (aged 15–49): DHS 1998, 2008–09, 2014 and 2022 (DHS 2003 did not ask
  the attitude question).
- **Part B — Men** (aged 15–49; men were interviewed up to 54): the DHS men's recodes of 2014
  and 2022 (men were not asked about FGM in 1998 or 2008–09; the 2003 men's file is not
  available), including a design-based comparison with women in the same rounds.
- **Part C — Trends**: bar charts of support for continuation by survey round for women and
  men.

Both parts follow the same steps as V1: predictor screening, collinearity check, design-based
direct estimates, survey-weighted spatial-only and covariate-adjusted BYM2 models by round,
maps, residual patterns, validation and the national trend. Outputs go to
`03_Outputs/KEN/Attitude_Women`, `Attitude_Men` and `Attitude_Women_and_Men`.
""")

# Part A: women's notebook without its own title cell.
a_title = md("# Part A — Women's attitudes")
a_cells = women["cells"][1:]
# Part B: men's notebook; its title becomes the Part B header.
b_title = md("""
# Part B — Men's attitudes

Same steps as Part A, applied to men aged 15–49 in the DHS men's recodes (2014 and 2022). Own FGM status
does not apply to men, so age is the only predictor kept a priori. Section 15 compares men
with women in the rounds that asked both.
""")
b_cells = men["cells"][1:]

c_cells = [
md("""
# Part C — Trends in support for continuation, women and men

Design-based national estimates by survey round, with 95% confidence intervals, among
respondents who have heard of FGM and answered "continue" or "stop". Men were asked only in
DHS 2014 and 2022.
"""),
code("""
trend_dir <- file.path(project_root, "03_Outputs", country_iso3, "Attitude_Women_and_Men")
dir.create(trend_dir, recursive = TRUE, showWarnings = FALSE)
round_levels <- c("DHS 1998", "DHS 2008", "DHS 2014", "DHS 2022")
round_labels <- sub("DHS 2008", "DHS 2008-09", round_levels)
read_direct <- function(folder, sex) {
  readr::read_csv(file.path(project_root, "03_Outputs", country_iso3, folder, "02_Direct_Estimates",
                            paste0(country_iso3, "_attitude_direct_national_by_round.csv")), show_col_types = FALSE) |>
    dplyr::mutate(sex = sex)
}
attitude_trend <- dplyr::bind_rows(read_direct("Attitude_Women", "Women"), read_direct("Attitude_Men", "Men")) |>
  dplyr::mutate(survey_id = factor(survey_id, levels = round_levels, labels = round_labels),
                sex = factor(sex, levels = c("Women", "Men")))
readr::write_csv(attitude_trend, file.path(trend_dir, paste0(country_iso3, "_attitude_trend_women_men.csv")))

sex_colours <- c(Women = "#C0392B", Men = "#1D4E89")
bar_theme <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 17, hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey35"),
                 plot.caption = ggplot2::element_text(color = "grey40", size = 10, hjust = 0.5),
                 axis.title = ggplot2::element_text(face = "bold"),
                 axis.text = ggplot2::element_text(face = "bold", color = "grey25"),
                 panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                 legend.position = "top", legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(face = "bold", size = 12))

trend_bars <- ggplot2::ggplot(attitude_trend, ggplot2::aes(survey_id, estimate, fill = sex)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8, preserve = "single"), width = 0.75) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci),
                         position = ggplot2::position_dodge(width = 0.8, preserve = "single"), width = 0.25, color = "grey25") +
  ggplot2::geom_text(ggplot2::aes(y = upper_ci, label = scales::percent(estimate, accuracy = 0.1)),
                     position = ggplot2::position_dodge(width = 0.8, preserve = "single"), vjust = -0.6, size = 3.4, fontface = "bold") +
  ggplot2::scale_fill_manual(values = sex_colours) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::labs(title = paste("Support for Continuing FGM by Survey Round, Women and Men,", country_name),
                subtitle = "Among those who have heard of FGM and answered continue or stop; bars show 95% confidence intervals",
                caption = "Men were asked about FGM only in DHS 2014 and 2022.",
                x = "Survey round", y = "Support for continuation") +
  bar_theme
print(trend_bars)
ggplot2::ggsave(file.path(trend_dir, paste0(country_iso3, "_attitude_trend_bars_women_men.png")), trend_bars,
                width = 13, height = 7.5, dpi = 300, bg = "white")
"""),
code("""
men_vs_women <- readr::read_csv(file.path(project_root, "03_Outputs", country_iso3, "Attitude_Men", "07_Men_vs_Women",
                                          paste0(country_iso3, "_attitude_national_men_vs_women_by_round.csv")), show_col_types = FALSE)
# Use the same display labels as the trend chart ("DHS 2008" is shown as "DHS 2008-09").
men_vs_women <- men_vs_women |> dplyr::mutate(survey_id = round_labels[match(survey_id, round_levels)])
shared <- attitude_trend |> dplyr::filter(as.character(survey_id) %in% men_vs_women$survey_id) |>
  dplyr::mutate(survey_id = droplevels(survey_id))
significance <- men_vs_women |>
  dplyr::mutate(label = paste0(ifelse(difference > 0, "+", ""), round(100 * difference, 1), " pts",
                               dplyr::case_when(p_value < 0.001 ~ " ***", p_value < 0.01 ~ " **", p_value < 0.05 ~ " *", TRUE ~ " (n.s.)")),
                survey_id = factor(survey_id, levels = levels(shared$survey_id))) |>
  dplyr::left_join(shared |> dplyr::group_by(survey_id) |> dplyr::summarise(top = max(upper_ci), .groups = "drop"), by = "survey_id")

dhs_bars <- ggplot2::ggplot(shared, ggplot2::aes(survey_id, estimate, fill = sex)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), position = ggplot2::position_dodge(width = 0.8),
                         width = 0.25, color = "grey25") +
  ggplot2::geom_text(ggplot2::aes(y = upper_ci, label = scales::percent(estimate, accuracy = 0.1)),
                     position = ggplot2::position_dodge(width = 0.8), vjust = -0.6, size = 3.6, fontface = "bold") +
  ggplot2::geom_text(data = significance, ggplot2::aes(x = survey_id, y = top + 0.045, label = paste("Men - women:", label)),
                     inherit.aes = FALSE, size = 3.5, color = "grey25") +
  ggplot2::scale_fill_manual(values = sex_colours) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0, 0.18))) +
  ggplot2::labs(title = paste("Support for Continuing FGM, Women vs Men, DHS Rounds,", country_name),
                subtitle = "Same surveys, design-based difference test (*** p < 0.001, ** p < 0.01, * p < 0.05)",
                x = "Survey round", y = "Support for continuation") +
  bar_theme
print(dhs_bars)
ggplot2::ggsave(file.path(trend_dir, paste0(country_iso3, "_attitude_women_vs_men_dhs_bars.png")), dhs_bars,
                width = 11, height = 7, dpi = 300, bg = "white")
IRdisplay::display(as.data.frame(attitude_trend |> dplyr::select(sex, survey_id, estimate, lower_ci, upper_ci, n) |>
  dplyr::mutate(dplyr::across(c(estimate, lower_ci, upper_ci), ~ round(100 * .x, 1))) |> dplyr::arrange(sex, survey_id)))
"""),
]

nb = {"cells": [intro, a_title] + a_cells + [b_title] + b_cells + c_cells,
      "metadata": women["metadata"], "nbformat": 4, "nbformat_minor": 5}
json.dump(nb, open(f"{NB}/FGM_Attitudinal_Incidence.ipynb", "w"), indent=1, ensure_ascii=False)
os.remove(f"{NB}/FGM_Attitudinal_Incidence_Men.ipynb")
print("combined cells:", len(nb["cells"]))
