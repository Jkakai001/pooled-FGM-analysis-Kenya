# ---- Harmonize all Kenya FGM rounds (DHS 1998, 2003, 2008-09, 2014, 2022) ----
# Kenya has no national MICS; its MICS surveys are subnational and are compared separately
# (mics_core.R), not pooled here.
suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

raw_dir <- file.path(project_root, "02_Data", "raw")
ext_dir <- file.path(raw_dir, "kenya_FGM_Data", "extracted", "DHS")

max_age_years    <- 49
max_daughter_age <- 14
cohort_breaks    <- seq(1945, 2025, by = 5)
cohort_labels    <- paste0(head(cohort_breaks, -1), " to ", head(cohort_breaks, -1) + 4)

# Pooled analyses use the 8 former provinces (available in every round). DHS 2022 records
# only the 47 counties, which are mapped to their former province; 2014 and 2022 also keep the
# county for county-level estimates. DHS 1998 did not sample North Eastern province.
kenya_provinces <- c("CENTRAL", "COAST", "EASTERN", "NAIROBI", "NORTH EASTERN", "NYANZA", "RIFT VALLEY", "WESTERN")
county_to_province <- c(
  "MOMBASA"="COAST","KWALE"="COAST","KILIFI"="COAST","TANA RIVER"="COAST","LAMU"="COAST","TAITA TAVETA"="COAST",
  "GARISSA"="NORTH EASTERN","WAJIR"="NORTH EASTERN","MANDERA"="NORTH EASTERN",
  "MARSABIT"="EASTERN","ISIOLO"="EASTERN","MERU"="EASTERN","THARAKA NITHI"="EASTERN","EMBU"="EASTERN","KITUI"="EASTERN",
  "MACHAKOS"="EASTERN","MAKUENI"="EASTERN",
  "NYANDARUA"="CENTRAL","NYERI"="CENTRAL","KIRINYAGA"="CENTRAL","MURANG A"="CENTRAL","KIAMBU"="CENTRAL",
  "TURKANA"="RIFT VALLEY","WEST POKOT"="RIFT VALLEY","SAMBURU"="RIFT VALLEY","TRANS NZOIA"="RIFT VALLEY",
  "UASIN GISHU"="RIFT VALLEY","ELGEYO MARAKWET"="RIFT VALLEY","NANDI"="RIFT VALLEY","BARINGO"="RIFT VALLEY",
  "LAIKIPIA"="RIFT VALLEY","NAKURU"="RIFT VALLEY","NAROK"="RIFT VALLEY","KAJIADO"="RIFT VALLEY","KERICHO"="RIFT VALLEY",
  "BOMET"="RIFT VALLEY",
  "KAKAMEGA"="WESTERN","VIHIGA"="WESTERN","BUNGOMA"="WESTERN","BUSIA"="WESTERN",
  "SIAYA"="NYANZA","KISUMU"="NYANZA","HOMA BAY"="NYANZA","MIGORI"="NYANZA","KISII"="NYANZA","NYAMIRA"="NYANZA",
  "NAIROBI"="NAIROBI")
kenya_counties <- sort(names(county_to_province))

# ---- Harmonization helpers ----
lbl <- function(x) tolower(trimws(as.character(haven::as_factor(x))))
num <- function(x) as.numeric(haven::zap_labels(x))
pick_col <- function(d, v) if (v %in% names(d)) d[[v]] else rep(NA, nrow(d))

clean_name <- function(x) toupper(as.character(x)) |> str_replace_all("[^A-Z ]", " ") |> str_squish()
standardize_region <- function(x) {          # former province
  s <- clean_name(x)
  s <- case_when(s == "NORTHEASTERN" ~ "NORTH EASTERN", TRUE ~ s)
  bad <- setdiff(unique(s[!is.na(s)]), kenya_provinces)
  if (length(bad)) stop("Unmatched province names: ", paste(bad, collapse = ", "))
  s
}
standardize_county <- function(x) {
  s <- clean_name(x)
  s <- case_when(s == "ELGEYO MARAKWET" | s == "KEIYO MARAKWET" ~ "ELGEYO MARAKWET", s == "THARAKA" ~ "THARAKA NITHI", TRUE ~ s)
  bad <- setdiff(unique(s[!is.na(s)]), kenya_counties)
  if (length(bad)) stop("Unmatched county names: ", paste(bad, collapse = ", "))
  s
}
province_from_county <- function(county) unname(county_to_province[county])

# Ten large groups coded in every round, everything else "other" (Turkana, Kuria, Samburu,
# Taita/Taveta and smaller groups are not coded separately in every round).
harmonize_ethnicity <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    is.na(x) | str_detect(x, "^(missing|dk|don.t know)$") ~ NA_character_,
    str_detect(x, "kikuy") ~ "kikuyu",
    str_detect(x, "luhya") ~ "luhya",
    str_detect(x, "kalenjin") ~ "kalenjin",
    str_detect(x, "^luo") ~ "luo",
    str_detect(x, "kamba") ~ "kamba",
    str_detect(x, "kisii") ~ "kisii",
    str_detect(x, "meru|embu") ~ "meru/embu",
    str_detect(x, "mijikenda|swahili") ~ "mijikenda/swahili",
    str_detect(x, "somali") ~ "somali",
    str_detect(x, "masai|maasai") ~ "maasai",
    TRUE ~ "other"
  )
}

harmonize_religion <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    is.na(x) | str_detect(x, "missing|manquant|^dk|nsp") ~ NA_character_,
    str_detect(x, "islam|muslim|moslem|musulman") ~ "islam",
    str_detect(x, "catholi|protestan|christian|evangel|instituted|orthodox|church") ~ "christian",
    str_detect(x, "tradition") ~ "traditional",
    TRUE ~ "other/none"
  )
}

harmonize_education <- function(x) {
  x <- tolower(as.character(x))
  case_when(
    is.na(x) | str_detect(x, "missing|manquant|dk|don.t know") ~ NA_character_,
    str_detect(x, "^no|none|non-formal|non formal|pre-?school") ~ "none",
    str_detect(x, "primary") ~ "primary",
    str_detect(x, "secondary|higher|tertiary") ~ "secondary or higher",
    TRUE ~ NA_character_
  )
}

harmonize_wealth <- function(x) {
  v <- num(x)
  factor(if_else(v %in% 1:5, v, NA_real_), levels = 1:5,
         labels = c("poorest", "poorer", "middle", "richer", "richest")) |> as.character()
}

# Age-at-cutting coding shared by all rounds: returns the usable age (infancy = 0)
# and a reporting category for the EDA.
code_age_at_cut <- function(raw, infancy_code = 95, max_valid) {
  cat_ <- case_when(
    is.na(raw) ~ "missing",
    raw == infancy_code ~ "during infancy",
    raw == 0 ~ "age 0",
    raw >= 0 & raw <= max_valid ~ "specific age 1+",
    raw == 98 ~ "don't know",
    raw == 97 ~ "inconsistent",
    TRUE ~ "missing"
  )
  age <- case_when(raw == infancy_code ~ 0, raw >= 0 & raw <= max_valid ~ raw, TRUE ~ NA_real_)
  list(age = age, category = cat_)
}

to_cohort <- function(birth_year) {
  as.character(cut(birth_year, breaks = cohort_breaks, right = FALSE, labels = cohort_labels))
}

attitude_from <- function(x) {
  v <- num(x)
  case_when(v == 1 ~ 1L, v == 2 ~ 0L, TRUE ~ NA_integer_)
}
attitude_label <- function(x) {
  v <- num(x)
  case_when(v == 1 ~ "continued", v == 2 ~ "stopped", v == 3 ~ "depends",
            v == 8 ~ "don't know", TRUE ~ NA_character_)
}

# ---- Survey inventory ----
# Men were asked about FGM only in DHS 2014 and 2022 (no FGM questions for men in 1998 or
# 2008-09; the 2003 men's file is not available).
rounds <- tribble(
  ~survey_id,  ~survey_type, ~round_year, ~ir_file,                  ~mr_file,
  "DHS 1998",  "DHS",        1998,        "KEIR3ADT/KEIR3AFL.DTA",   NA,
  "DHS 2003",  "DHS",        2003,        "KEIR42DT/KEIR42FL.DTA",   NA,
  "DHS 2008",  "DHS",        2008,        "KEIR52DT/KEIR52FL.DTA",   NA,
  "DHS 2014",  "DHS",        2014,        "KEIR72DT/KEIR72FL.DTA",   "KE_2014_DHS_10142026_516_258683/KEMR72DT/KEMR72FL.DTA",
  "DHS 2022",  "DHS",        2022,        "KEIR8CDT/KEIR8CFL.DTA",   "KE_2022_DHS_10142026_513_258683/KEMR8CDT/KEMR8CFL.DTA"
)

# ---- DHS women and daughters ----
load_dhs_round <- function(survey_id, round_year, ir_file) {
  path <- file.path(ext_dir, ir_file)
  # DHS 1998 and 2003 used country-specific questions (s10xx, s82x) and had no "heard of FGM"
  # question: every woman was asked her own status. DHS 2003 asked status only (no age at
  # cutting, no attitude). DHS 2008-09 recorded the most recently cut daughter only; DHS 2014
  # and 2022 list every daughter. In 2014 and 2022 the FGM questions were asked only in the
  # long-questionnaire subsample (women outside it have no FGM data).
  fg <- switch(as.character(round_year),
    "1998" = list(heard = NA, probe = NA, status = "s1002", age = "s1003", attitude = "s1012", recent_age = NA),
    "2003" = list(heard = NA, probe = NA, status = "s821", age = NA, attitude = NA, recent_age = NA),
    "2008" = list(heard = "g100", probe = "g101", status = "g102", age = "g106", attitude = "g119", recent_age = "g113"),
    list(heard = "g100", probe = "g101", status = "g102", age = "g106", attitude = "g119", recent_age = NA))
  keep <- c("v001", "v002", "v003", "v005", "v007", "v010", "v012", "v021", "v022", "v024", "v025",
            "v106", "v130", "v131", "v190", "scounty", unlist(fg))
  ir <- read_dta(path, col_select = c(any_of(keep[!is.na(keep)]),
                                      matches("^(gidx|g121|g122|bidx|b2|b4|b5|b8)_[0-9]{2}$")))
  col <- function(v) if (!is.na(v) && v %in% names(ir)) num(ir[[v]]) else rep(NA_real_, nrow(ir))

  heard <- col(fg$heard); probe <- col(fg$probe); status_raw <- col(fg$status)
  status_raw[!status_raw %in% c(0, 1, 8)] <- NA
  if (is.na(fg$heard)) {
    module_asked <- !is.na(status_raw)
    never_heard <- rep(FALSE, nrow(ir))
  } else {
    module_asked <- !is.na(heard) & heard %in% c(0, 1)
    never_heard <- module_asked & heard == 0 & (is.na(probe) | probe == 0)
  }
  fgm_status <- case_when(status_raw == 1 ~ 1L, status_raw == 0 ~ 0L, never_heard ~ 0L, TRUE ~ NA_integer_)
  age_raw <- col(fg$age)
  age_code <- code_age_at_cut(age_raw, infancy_code = 95, max_valid = max_age_years)
  # Province and county: 1998-2008 provinces only; 2014 province (v024) and county (scounty);
  # 2022 county only (v024), mapped to its former province.
  county <- if (round_year == 2014) standardize_county(lbl(ir$scounty)) else if (round_year == 2022) standardize_county(lbl(ir$v024)) else NA_character_
  region <- if (round_year == 2022) province_from_county(county) else standardize_region(lbl(ir$v024))
  century <- if (round_year == 1998) 1900 else 0       # DHS 1998 stores two-digit years
  attitude_raw <- if (is.na(fg$attitude)) rep(NA_real_, nrow(ir)) else ir[[fg$attitude]]

  women <- tibble(
    survey_id, survey_type = "DHS", round_year,
    resp_id = paste(survey_id, ir$v001, ir$v002, ir$v003, sep = "-"),
    cluster = paste(survey_id, ir$v001, sep = "-"),
    strata = paste(survey_id, num(ir$v022)),
    weight_raw = num(ir$v005) / 1e6,
    region, county,
    urban_rural = lbl(ir$v025),
    age = num(ir$v012),
    interview_year = num(ir$v007) + century,
    birth_year = num(ir$v010) + century,
    education = harmonize_education(lbl(ir$v106)),
    religion = harmonize_religion(lbl(ir$v130)),
    ethnicity = harmonize_ethnicity(lbl(ir$v131)),
    wealth = harmonize_wealth(pick_col(ir, "v190")),
    module_asked,
    fgm_heard = if (is.na(fg$heard)) NA else (!never_heard & module_asked),
    status_reported = case_when(status_raw %in% c(0, 1) ~ "asked",
                                never_heard ~ "never heard (uncut)",
                                status_raw == 8 ~ "don't know",
                                module_asked ~ "missing",
                                TRUE ~ "module not asked"),
    fgm_status,
    fgm_age_raw = age_raw,
    fgm_age = if_else(fgm_status == 1, age_code$age, NA_real_),
    age_report = case_when(is.na(fgm_status) | fgm_status != 1 ~ NA_character_,
                           is.na(fg$age) ~ "not asked in survey",
                           TRUE ~ age_code$category),
    attitude_answer = attitude_label(attitude_raw),
    supports_continuation = attitude_from(attitude_raw)
  )

  # Most recently cut daughter only (DHS 2008-09): descriptive use.
  recent <- tibble()
  if (!is.na(fg$recent_age) && fg$recent_age %in% names(ir)) {
    ra <- num(ir[[fg$recent_age]])
    rc <- code_age_at_cut(ra, infancy_code = 95, max_valid = max_daughter_age)
    recent <- tibble(survey_id, survey_type = "DHS", round_year, resp_id = women$resp_id,
                     weight_raw = women$weight_raw, region = women$region,
                     daughter_age_at_cut = rc$age, age_report = rc$category) |>
      filter(!is.na(ra))
  }

  # Full daughter roster (DHS 2014, 2022) plus daughters of never-heard mothers.
  daughters <- tibble()
  if (any(str_detect(names(ir), "^gidx_01$"))) {
    roster <- ir |>
      transmute(resp_id = women$resp_id, across(matches("^(gidx|g121|g122)_[0-9]{2}$"), num)) |>
      pivot_longer(-resp_id, names_to = c(".value", "slot"), names_pattern = "^(gidx|g121|g122)_([0-9]{2})$") |>
      filter(!is.na(gidx)) |>
      transmute(resp_id, birth_order = as.integer(gidx),
                daughter_status = case_when(g121 == 1 ~ 1L, g121 == 0 ~ 0L, TRUE ~ NA_integer_),
                age_at_cut_raw = g122)
    births <- ir |>
      transmute(resp_id = women$resp_id, across(matches("^(bidx|b2|b4|b5|b8)_[0-9]{2}$"), num)) |>
      pivot_longer(-resp_id, names_to = c(".value", "slot"), names_pattern = "^(bidx|b2|b4|b5|b8)_([0-9]{2})$") |>
      filter(!is.na(bidx), b4 == 2, b5 == 1, !is.na(b8), b8 <= max_daughter_age) |>
      transmute(resp_id, birth_order = as.integer(bidx), daughter_age = b8, birth_year = b2)
    reported <- births |> inner_join(roster, by = c("resp_id", "birth_order")) |>
      filter(!is.na(daughter_status)) |> mutate(source = "roster")
    imputed <- births |>
      semi_join(women |> filter(status_reported == "never heard (uncut)"), by = "resp_id") |>
      anti_join(reported, by = c("resp_id", "birth_order")) |>
      mutate(daughter_status = 0L, age_at_cut_raw = NA_real_, source = "mother never heard (uncut)")
    daughters <- bind_rows(reported, imputed) |>
      mutate(survey_id, survey_type = "DHS", round_year,
             daughter_id = paste(resp_id, birth_order, sep = "-d"))
  }
  list(women = women, daughters = daughters, recent = recent)
}

# ---- DHS men ----
load_dhs_men <- function(survey_id, round_year, mr_file) {
  mr <- read_dta(file.path(ext_dir, mr_file))
  county <- if (round_year == 2014) standardize_county(lbl(mr$smcounty)) else standardize_county(lbl(mr$mv024))
  region <- if (round_year == 2014) standardize_region(lbl(mr$mv024)) else province_from_county(county)
  heard <- num(mr$mg100); probe <- num(mr$mg101)
  tibble(
    survey_id, survey_type = "DHS", round_year,
    resp_id = paste(survey_id, mr$mv001, mr$mv002, mr$mv003, "m", sep = "-"),
    cluster = paste(survey_id, mr$mv001, sep = "-"),
    strata = paste(survey_id, num(mr$mv022)),
    weight_raw = num(mr$mv005) / 1e6,
    region, county,
    urban_rural = lbl(mr$mv025),
    age = num(mr$mv012),
    education = harmonize_education(lbl(mr$mv106)),
    religion = harmonize_religion(lbl(mr$mv130)),
    ethnicity = harmonize_ethnicity(lbl(mr$mv131)),
    wealth = harmonize_wealth(pick_col(mr, "mv190")),
    fgm_heard = heard %in% 1 | probe %in% 1,
    attitude_answer = attitude_label(mr$mg119),
    supports_continuation = attitude_from(mr$mg119)
  ) |>
    filter(age >= 15, age <= max_age_years, !is.na(heard))
}

# ---- Run every round ----
# Automatic coverage check, run for every round with a daughter list. DHS and MICS daughter
# lists cover only mothers who have heard of FGM, so the daughters of never-heard mothers must be
# added (as not cut) from the birth history or household roster. Stop if a round has never-heard
# women but no such daughters; warn if their share of daughters is far below their share of women.
check_daughter_coverage <- function(women, daughters) {
  chk <- women |>
    group_by(survey_id) |>
    summarise(women = n(), never_heard_women = sum(status_reported == "never heard (uncut)", na.rm = TRUE), .groups = "drop") |>
    inner_join(daughters |> group_by(survey_id) |>
                 summarise(daughters = n(), daughters_of_never_heard = sum(source == "mother never heard (uncut)"), .groups = "drop"),
               by = "survey_id") |>
    mutate(pct_never_heard_women = round(100 * never_heard_women / women, 1),
           pct_daughters_of_never_heard = round(100 * daughters_of_never_heard / daughters, 1))
  print(as.data.frame(chk))
  missing <- chk |> filter(never_heard_women > 0, daughters_of_never_heard == 0)
  if (nrow(missing)) stop("Daughters of never-heard mothers are missing in: ", paste(missing$survey_id, collapse = ", "),
                          ". Add them from the birth history or household roster.")
  low <- chk |> filter(pct_never_heard_women > 5, pct_daughters_of_never_heard < 0.5 * pct_never_heard_women)
  if (nrow(low)) warning("Daughters of never-heard mothers look under-counted in: ", paste(low$survey_id, collapse = ", "))
  invisible(chk)
}

harmonize_all_rounds <- function() {
  out <- pmap(rounds, function(survey_id, survey_type, round_year, ir_file, mr_file) {
    message("Harmonizing ", survey_id)
    load_dhs_round(survey_id, round_year, ir_file)
  })
  women <- map_dfr(out, "women")
  daughters <- map_dfr(out, "daughters")
  recent <- map_dfr(out, "recent")
  men <- rounds |> filter(!is.na(mr_file)) |>
    pmap_dfr(function(survey_id, survey_type, round_year, ir_file, mr_file) load_dhs_men(survey_id, round_year, mr_file))

  # Mean-one weights within each survey, so pooled analyses weight each round by its sample size.
  normalize <- function(d) d |> group_by(survey_id) |> mutate(weight = weight_raw / mean(weight_raw)) |> ungroup()
  women <- normalize(women) |> mutate(birth_cohort = to_cohort(birth_year))
  men <- normalize(men)

  mother_vars <- women |> select(resp_id, cluster, strata, weight, region, county, urban_rural, education,
                                 religion, ethnicity, wealth, interview_year, mother_age = age)
  daughters <- daughters |>
    left_join(mother_vars, by = "resp_id") |>
    mutate(daughter_age_at_cut = case_when(daughter_status == 1 & age_at_cut_raw == 95 ~ 0,
                                           daughter_status == 1 & age_at_cut_raw >= 0 & age_at_cut_raw <= max_daughter_age ~ age_at_cut_raw,
                                           TRUE ~ NA_real_),
           age_report = case_when(daughter_status != 1 ~ NA_character_,
                                  age_at_cut_raw == 95 ~ "during infancy",
                                  age_at_cut_raw == 0 ~ "age 0",
                                  age_at_cut_raw >= 1 & age_at_cut_raw <= max_daughter_age ~ "specific age 1+",
                                  age_at_cut_raw == 98 ~ "don't know",
                                  age_at_cut_raw == 97 ~ "inconsistent",
                                  TRUE ~ "missing"),
           birth_cohort = to_cohort(birth_year))
  recent <- recent |> group_by(survey_id) |> mutate(weight = weight_raw / mean(weight_raw)) |> ungroup()
  check_daughter_coverage(women, daughters)
  list(women = women, daughters = daughters, recent = recent, men = men)
}
