# Kenya's subnational MICS versus DHS: shared helpers for the MICS comparison sections.
# Kenya has no national MICS. Five MICS cover single areas and are compared with DHS for the
# same area, closest in time:
#   Mombasa informal settlements 2009 (MICS4) -> DHS 2014 Mombasa county (whole county: indicative only)
#   Nyanza province 2011 (MICS4)              -> DHS 2008-09 and DHS 2014 Nyanza province
#   Bungoma, Kakamega, Turkana 2013-14 (MICS5) -> DHS 2014 county (same county, same year)
# Definitions follow the DHS harmonisation: never heard (FG1 = no and FG2 = no) -> not cut;
# missing status excluded; support = "continued" among "continued"/"discontinued".
# MICS4 did not ask women's own age at cutting. MICS weights: wmweight; PSU = cluster (HH1);
# stratum = survey x urban/rural (no stratum variable released).

mics_surveys <- tibble::tribble(
  ~mics_id,                                 ~folder,                                                   ~year, ~round,  ~area,          ~level,      ~dhs_compare,
  "MICS 2009 Mombasa informal settlements", "Kenya_Mombasa_Informal_Settlements_MICS4_2009_Datasets",  2009,  "MICS4", "MOMBASA",      "county",    "DHS 2014",
  "MICS 2011 Nyanza",                       "Kenya_Nyanza_MICS4_2011_Datasets",                        2011,  "MICS4", "NYANZA",       "province",  "DHS 2008|DHS 2014",
  "MICS 2013-14 Bungoma",                   "Kenya_Bungoma_MICS5_2013-14_Datasets",                    2014,  "MICS5", "BUNGOMA",      "county",    "DHS 2014",
  "MICS 2013-14 Kakamega",                  "Kenya_Kakamega_MICS5_2013-14_Datasets",                   2014,  "MICS5", "KAKAMEGA",     "county",    "DHS 2014",
  "MICS 2013-14 Turkana",                   "Kenya_Turkana_MICS5_2013-14_Datasets",                    2014,  "MICS5", "TURKANA",      "county",    "DHS 2014"
)

read_mics_women <- function(mics_dir, mics_id, folder, round, area, level) {
  f <- list.files(file.path(mics_dir, folder), "^wm\\.sav$", recursive = TRUE, full.names = TRUE)[1]
  wm <- haven::read_sav(f); names(wm) <- toupper(names(wm))
  n <- function(v) as.numeric(haven::zap_labels(v))
  pick <- function(...) { for (v in c(...)) if (v %in% names(wm)) return(n(wm[[v]])); rep(NA_real_, nrow(wm)) }
  heard <- pick("FG1"); probe <- pick("FG2"); status_raw <- pick("FG3")
  age_raw <- if (round == "MICS5") pick("FG7") else rep(NA_real_, nrow(wm))   # MICS4 FG7 is the circumciser
  att <- if (round == "MICS5") pick("FG22") else pick("FG16")
  module_asked <- heard %in% c(1, 2)
  never_heard <- module_asked & heard == 2 & !probe %in% 1
  fgm_status <- dplyr::case_when(status_raw == 1 ~ 1, status_raw == 2 ~ 0, never_heard ~ 0, TRUE ~ NA_real_)
  age <- pick("WB2", "WM9")
  tibble::tibble(
    source = mics_id, area, level, round,
    cluster = paste(mics_id, pick("HH1")), strata = paste(mics_id, pick("HH6")),
    weight = pick("WMWEIGHT"), result = pick("WM7"), age,
    fgm_heard = as.numeric(module_asked & !never_heard), fgm_status,
    fgm_age = dplyr::if_else(fgm_status == 1 & age_raw >= 0 & age_raw <= pmin(age, 49), age_raw, NA_real_),
    age_report = dplyr::case_when(is.na(fgm_status) | fgm_status != 1 ~ NA_character_,
                                  round == "MICS4" ~ "not asked in survey",
                                  age_raw == 0 ~ "age 0",
                                  age_raw >= 1 & age_raw <= age ~ "specific age 1+",
                                  age_raw == 98 ~ "don't know", TRUE ~ "missing"),
    supports_continuation = dplyr::case_when(att == 1 ~ 1, att == 2 ~ 0, TRUE ~ NA_real_),
    attitude_answer = dplyr::case_when(att == 1 ~ "continued", att == 2 ~ "stopped", att == 3 ~ "depends",
                                       att == 8 ~ "don't know", TRUE ~ NA_character_)
  ) |>
    dplyr::filter(result == 1, is.finite(weight), weight > 0, age >= 15, age <= 49) |>
    dplyr::select(-result)
}

harmonize_mics <- function(mics_dir) {
  dplyr::bind_rows(purrr::pmap(mics_surveys |> dplyr::select(mics_id, folder, round, area, level),
                               function(mics_id, folder, round, area, level) read_mics_women(mics_dir, mics_id, folder, round, area, level)))
}

# DHS women for the area a MICS survey covers (county or province), in the requested round.
dhs_area <- function(h_women, survey, area, level) {
  if (level == "county") h_women |> dplyr::filter(survey_id == survey, county == area)
  else h_women |> dplyr::filter(survey_id == survey, region == area)
}

svy_prop <- function(d, var) {
  d <- d |> dplyr::filter(!is.na(.data[[var]]), is.finite(weight), weight > 0)
  if (nrow(d) == 0) return(tibble::tibble(estimate = NA_real_, se = NA_real_, n = 0L, lower_ci = NA_real_, upper_ci = NA_real_))
  des <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
  est <- survey::svymean(stats::as.formula(paste0("~", var)), des)
  e <- as.numeric(stats::coef(est)); s <- as.numeric(survey::SE(est))
  tibble::tibble(estimate = e, se = s, n = nrow(d), lower_ci = pmax(e - 1.96 * s, 0), upper_ci = pmin(e + 1.96 * s, 1))
}

diff_test <- function(a, b) {
  d <- a$estimate - b$estimate; se <- sqrt(a$se^2 + b$se^2)
  tibble::tibble(difference = d, lower = d - 1.96 * se, upper = d + 1.96 * se, p_value = 2 * stats::pnorm(-abs(d / se)))
}

# One comparison table: each MICS survey against its DHS comparison round(s) for the same area.
mics_vs_dhs <- function(mics, h_women, var) {
  purrr::pmap_dfr(mics_surveys, function(mics_id, folder, year, round, area, level, dhs_compare) {
    m <- svy_prop(mics |> dplyr::filter(source == mics_id), var) |> dplyr::mutate(source = mics_id)
    dh <- purrr::map_dfr(strsplit(dhs_compare, "\\|")[[1]], function(s)
      svy_prop(dhs_area(h_women, s, area, level), var) |> dplyr::mutate(source = s))
    dplyr::bind_rows(m, dh) |> dplyr::mutate(comparison = mics_id, area, level, .before = 1)
  })
}
