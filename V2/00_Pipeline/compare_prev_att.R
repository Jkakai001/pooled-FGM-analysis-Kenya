# Prevalence vs attitudes, Kenya: national (by survey round) and county level (47 counties,
# DHS 2014 and 2022 pooled). Kenya's 8 provinces are too few for a joint model, so the
# "whose attitudes matter more" analysis uses counties, the only level with enough areas.
# Usage: Rscript compare_prev_att.R <V2 folder>
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(INLA) })
options(survey.lonely.psu = "adjust", width = 200)
project_root <- commandArgs(TRUE)[1]; country <- "Kenya"; iso <- "KEN"
source(file.path(project_root, "00_Pipeline", "county_core.R"))
out <- file.path(project_root, "03_Outputs", iso)
dir <- file.path(out, "Prevalence_Attitude_Comparison", "03_National_and_County_Association")
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# ---- National, by survey round (design-based direct estimates) ----
prev <- readr::read_csv(file.path(out, "Prevalence/05_Trends", paste0(iso, "_prevalence_national_by_round.csv")), show_col_types = FALSE) |>
  filter(method == "Design-based direct") |>
  transmute(survey_id, value = estimate,
            series = if_else(population == "Women 15-49", "Prevalence, women 15-49", "Prevalence, girls 0-14"))
att <- readr::read_csv(file.path(out, "Attitude_Women_and_Men", paste0(iso, "_attitude_trend_women_men.csv")), show_col_types = FALSE) |>
  transmute(survey_id = sub("DHS 2008-09", "DHS 2008", survey_id), value = estimate, series = paste0("Support for continuation, ", tolower(sex)))
national <- bind_rows(prev, att) |> mutate(year = as.integer(sub(".* (\\d{4}).*", "\\1", survey_id))) |> arrange(year)
national_wide <- national |> mutate(value = round(100 * value, 1)) |>
  pivot_wider(id_cols = c(survey_id, year), names_from = series, values_from = value)
readr::write_csv(national_wide, file.path(dir, paste0(iso, "_national_prevalence_and_attitude_by_round.csv")))
nat_cor <- function(x, y) {
  d <- national_wide[, c(x, y)] |> drop_na()
  tibble(prevalence = x, attitude = y, rounds = nrow(d), r = if (nrow(d) >= 3) cor(d[[1]], d[[2]]) else NA_real_)
}
national_cor <- bind_rows(nat_cor("Prevalence, women 15-49", "Support for continuation, women"),
                          nat_cor("Prevalence, girls 0-14", "Support for continuation, women"),
                          nat_cor("Prevalence, women 15-49", "Support for continuation, men"),
                          nat_cor("Prevalence, girls 0-14", "Support for continuation, men"))
readr::write_csv(national_cor, file.path(dir, paste0(iso, "_national_correlation_by_round.csv")))
p_nat <- ggplot(national |> mutate(value = 100 * value), aes(year, value, colour = series)) +
  geom_line(linewidth = 1) + geom_point(size = 2.6) +
  scale_colour_manual(values = c("Prevalence, women 15-49" = "#7B2D8E", "Prevalence, girls 0-14" = "#D9822B",
                                 "Support for continuation, women" = "#1B7F79", "Support for continuation, men" = "#2C5AA0"), name = NULL) +
  scale_x_continuous(breaks = sort(unique(national$year))) +
  scale_y_continuous(limits = c(0, 40), labels = function(x) paste0(x, "%")) +
  labs(title = paste0("FGM prevalence and support for continuation, ", country),
       subtitle = "Design-based national estimates by DHS round", x = "Survey year", y = NULL) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey30"),
        legend.position = "bottom", panel.grid.minor = element_blank()) + guides(colour = guide_legend(nrow = 2))
ggsave(file.path(dir, paste0(iso, "_national_prevalence_and_attitude_trend.png")), p_nat, width = 10, height = 6, dpi = 300, bg = "white")

# ---- County level: DHS 2014 and 2022 pooled, BYM2-smoothed ----
h <- file.path(project_root, "02_Data", "processed", iso, "harmonized")
rounds <- c("DHS 2014", "DHS 2022")
women <- readRDS(file.path(h, paste0(iso, "_women_all_rounds.rds"))) |> filter(survey_id %in% rounds)
daughters <- readRDS(file.path(h, paste0(iso, "_daughters_all_rounds.rds"))) |> filter(survey_id %in% rounds)
men <- readRDS(file.path(h, paste0(iso, "_men_all_rounds.rds"))) |> filter(survey_id %in% rounds)
b <- county_boundaries(project_root)
sm <- function(d, var, name) county_smooth(county_direct(d, var), b) |> transmute(county, !!name := smoothed)
wide <- sm(women, "fgm_status", "prev_women") |> left_join(sm(daughters, "daughter_status", "prev_girls"), by = "county") |>
  left_join(sm(women, "supports_continuation", "women_support"), by = "county") |>
  left_join(sm(men, "supports_continuation", "men_support"), by = "county")
readr::write_csv(wide, file.path(dir, paste0(iso, "_county_prevalence_and_attitude_wide.csv")))

pair_cor <- function(y, x) {
  pt <- cor.test(wide[[y]], wide[[x]]); sp <- cor.test(wide[[y]], wide[[x]], method = "spearman", exact = FALSE)
  tibble(prevalence = y, attitude = x, pearson = unname(pt$estimate), lower = pt$conf.int[1], upper = pt$conf.int[2],
         p_value = pt$p.value, spearman = unname(sp$estimate))
}
county_cor <- bind_rows(lapply(c("prev_women", "prev_girls"), function(y) bind_rows(pair_cor(y, "women_support"), pair_cor(y, "men_support"))))
readr::write_csv(county_cor, file.path(dir, paste0(iso, "_county_correlation_with_ci.csv")))

joint <- bind_rows(lapply(c("prev_women", "prev_girls"), function(y) {
  z <- wide |> mutate(across(c(prev_women, prev_girls), ~ 100 * .x), across(c(women_support, men_support), ~ as.numeric(scale(.x))))
  m <- lm(reformulate(c("women_support", "men_support"), y), data = z); co <- summary(m)$coefficients; ci <- confint(m)
  tibble(prevalence = y, term = c("women_support", "men_support"),
         pp_per_sd = co[c("women_support", "men_support"), 1], lower = ci[c("women_support", "men_support"), 1],
         upper = ci[c("women_support", "men_support"), 2], p_value = co[c("women_support", "men_support"), 4],
         r2_both = summary(m)$r.squared,
         r2_women_only = summary(lm(reformulate("women_support", y), data = z))$r.squared,
         r2_men_only = summary(lm(reformulate("men_support", y), data = z))$r.squared)
}))
readr::write_csv(joint, file.path(dir, paste0(iso, "_county_joint_model_women_vs_men.csv")))

long <- wide |> pivot_longer(c(women_support, men_support), names_to = "who", values_to = "support") |>
  pivot_longer(c(prev_women, prev_girls), names_to = "pop", values_to = "prevalence") |>
  mutate(who = recode(who, women_support = "Women's support", men_support = "Men's support"),
         pop = recode(pop, prev_women = "Prevalence, women 15-49", prev_girls = "Prevalence, girls 0-14"))
p_cty <- ggplot(long, aes(support * 100, prevalence * 100)) + geom_point(colour = "#1F4E79", size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "#C0392B", linewidth = 0.8, formula = y ~ x) +
  facet_grid(pop ~ who, scales = "free") +
  labs(title = "County FGM prevalence against support for continuation, Kenya",
       subtitle = "47 counties, DHS 2014 and 2022 pooled; BYM2-smoothed estimates",
       x = "Support for continuation (%)", y = "Prevalence (%)") + theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"))
ggsave(file.path(dir, paste0(iso, "_county_scatter_prevalence_vs_attitude.png")), p_cty, width = 11, height = 8, dpi = 300, bg = "white")

print(national_wide, width = 200); print(national_cor); print(county_cor); print(joint, width = 200)
cat("support correlation women vs men:", round(cor(wide$women_support, wide$men_support), 3), "\n")
