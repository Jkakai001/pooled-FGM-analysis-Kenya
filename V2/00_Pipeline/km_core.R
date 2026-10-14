# ---- Person-years and design-based Kaplan-Meier band risk ----
suppressPackageStartupMessages(library(data.table))

# One row per completed year of life up to the event or censoring age, aggregated
# within survey cluster and the requested grouping variables.
aggregate_person_years <- function(events, group_vars, max_age = 49L) {
  dt <- data.table::as.data.table(events)[, .row := .I]
  last_age <- pmin(floor(dt$time), max_age)
  idx <- rep.int(dt$.row, last_age + 1L)
  age_year <- sequence(last_age + 1L) - 1L
  py <- data.table::data.table(.row = idx, age_year = age_year)
  py[, event := as.integer(age_year == floor(dt$time[.row]) & dt$fgm_event[.row] == 1L)]
  keep <- unique(c("cluster", "strata", group_vars))
  # Records whose age at cutting was spread over several ages carry fractional shares;
  # unweighted counts sum those shares so each person counts once.
  share <- if ("age_share" %in% names(dt)) dt$age_share[py$.row] else 1
  py <- cbind(py, dt[py$.row, ..keep], weight = dt$weight[py$.row], share = share)
  py[, .(w_risk = sum(weight), w_event = sum(weight * event), n_risk = sum(share), n_event = sum(share * event)),
     by = c(keep, "age_year")]
}

# Kaplan-Meier conditional band risk 1 - S(hi)/S(lo) per domain, with a Taylor-
# linearized standard error of log S from PSU totals (stratified, with replacement;
# single-PSU strata centred on the overall PSU mean, as survey.lonely.psu = "adjust").
km_band_risk <- function(py, domain_vars, cutpoints, followup, design_psus) {
  py <- data.table::as.data.table(py)
  hz <- py[, .(E = sum(w_event), R = sum(w_risk)), by = c(domain_vars, "age_year")]
  hz[, h := E / R]
  bands <- data.table::data.table(lo = head(cutpoints, -1), hi = cutpoints[-1])
  bands[, age_band := paste0(lo, "-", hi)]
  out <- list()
  for (b in seq_len(nrow(bands))) {
    lo <- bands$lo[b]; hi <- bands$hi[b]
    ages <- if (lo == 0) 0:hi else (lo + 1):hi
    hb <- hz[age_year %in% ages]
    est <- hb[, .(log_surv = sum(log1p(-pmin(h, 1 - 1e-12))), n_ages = .N), by = domain_vars]
    # PSU-level influence values for log S.
    z <- merge(py[age_year %in% ages], hb[, c(domain_vars, "age_year", "h", "R"), with = FALSE],
               by = c(domain_vars, "age_year"))
    z[, u := -(w_event - h * w_risk) / (R * (1 - pmin(h, 1 - 1e-12)))]
    zc <- z[, .(z = sum(u)), by = c(domain_vars, "strata", "cluster")]
    var_dt <- domain_variance(zc, domain_vars, design_psus)
    res <- merge(est, var_dt, by = domain_vars, all.x = TRUE)
    res[, `:=`(age_band = bands$age_band[b], lo = lo, hi = hi, n_ages_needed = length(ages))]
    out[[b]] <- res
  }
  res <- data.table::rbindlist(out, use.names = TRUE)
  res <- merge(res, data.table::as.data.table(followup), by = domain_vars, all.x = TRUE)
  res[, supported := max_observed >= hi & n_ages == n_ages_needed]
  res[, se_log := sqrt(pmax(var_log, 0))]
  res[, `:=`(risk = data.table::fifelse(supported, 1 - exp(log_surv), NA_real_),
             lower_ci = data.table::fifelse(supported, 1 - pmin(1, exp(log_surv + 1.96 * se_log)), NA_real_),
             upper_ci = data.table::fifelse(supported, 1 - exp(log_surv - 1.96 * se_log), NA_real_))]
  # Risk to the last observed age for bands a cohort has not completed.
  res[]
}

# Stratified with-replacement variance of a domain total from PSU totals, counting
# PSUs with zero contribution within each stratum of the full design.
domain_variance <- function(zc, domain_vars, design_psus) {
  dp <- data.table::as.data.table(design_psus)            # strata, n_h
  s <- zc[, .(sum_z = sum(z), sum_z2 = sum(z^2)), by = c(domain_vars, "strata")]
  s <- merge(s, dp, by = "strata")
  grand <- zc[, .(total = sum(z)), by = domain_vars]
  n_psu_total <- sum(dp$n_h)
  s <- merge(s, grand, by = domain_vars)
  s[, contrib := data.table::fifelse(
    n_h > 1,
    n_h / (n_h - 1) * (sum_z2 - sum_z^2 / n_h),
    # Single-PSU stratum: deviation from the overall mean PSU total.
    (sum_z - total / n_psu_total)^2)]
  s[, .(var_log = sum(contrib)), by = domain_vars]
}
