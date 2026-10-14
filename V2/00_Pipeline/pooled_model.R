# Pooled survey-weighted BYM2 model on all survey rounds together. Weights are mean-one
# within each survey, so every round contributes in proportion to its sample size; a
# survey-round term absorbs level differences between rounds. Covariates a round did not
# collect are kept as an "unknown" category so no round is dropped. Respondents with
# identical predictors and outcome are grouped with summed weights, which leaves the
# weighted likelihood unchanged.
prepare_pooled <- function(data, outcome, covariates) {
  data |>
    dplyr::filter(!is.na(.data[[outcome]]), is.finite(weight), weight > 0, is.finite(age),
                  !is.na(birth_cohort), region %in% region_keys) |>
    dplyr::mutate(region_id = match(region, region_keys),
                  dplyr::across(dplyr::all_of(covariates), ~ factor(tidyr::replace_na(as.character(.x), "unknown"))),
                  survey_id = factor(as.character(survey_id)),
                  birth_cohort = as.character(birth_cohort),
                  age_id = as.integer(age - min(age) + 1L)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("region", "region_id", "age_id", "birth_cohort", "survey_id",
                                                  covariates, outcome)))) |>
    dplyr::summarise(survey_weight = sum(weight), n = dplyr::n(), .groups = "drop")
}

fit_pooled <- function(grouped, outcome, covariates, spatial_only = FALSE, n_draws = posterior_draws, seed = 1L) {
  fixed <- if (spatial_only) "1" else paste(c("survey_id", covariates), collapse = " + ")
  age_term <- if (spatial_only) "" else " + f(age_id, model = 'rw2', constr = TRUE, scale.model = TRUE)"
  model_formula <- stats::as.formula(paste0(outcome, " ~ ", fixed, age_term,
    " + f(region_id, model = 'bym2', graph = adjacency_matrix, scale.model = TRUE)"))
  fit <- INLA::inla(model_formula, family = "binomial", data = grouped, Ntrials = rep(1L, nrow(grouped)),
                    weights = grouped$survey_weight,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                    control.inla = list(control.vb = list(enable = FALSE)),
                    # Weakly informative N(0, sd 2) prior on the fixed effects (Kenya): groups with
                    # almost no cases (e.g. Kikuyu girls) otherwise give unbounded coefficients and
                    # inflated, unstable predictions in low-prevalence provinces.
                    control.fixed = list(prec = 0.25, prec.intercept = 0.001))
  set.seed(seed)
  samples <- INLA::inla.posterior.sample(n_draws, fit, use.improved.mean = FALSE, skew.corr = FALSE)
  rows <- grep("^Predictor:", rownames(samples[[1]]$latent))
  rows <- rows[order(as.integer(sub("^Predictor:", "", rownames(samples[[1]]$latent)[rows])))]
  stopifnot(length(rows) == nrow(grouped))
  w <- grouped$survey_weight
  weighted_probs <- lapply(samples, function(s) stats::plogis(as.numeric(s$latent[rows, 1L])) * w)
  rm(samples)
  groupings <- list(
    state = grouped$region,
    state_cohort = paste(grouped$region, grouped$birth_cohort, sep = "|"),
    cohort = grouped$birth_cohort,
    survey = as.character(grouped$survey_id),
    national = rep(country_name, nrow(grouped))
  )
  summaries <- lapply(groupings, function(g) {
    g <- factor(g)
    num <- vapply(weighted_probs, function(pw) as.numeric(rowsum(pw, g)), numeric(nlevels(g)))
    if (is.null(dim(num))) num <- matrix(num, nrow = nlevels(g))
    den <- as.numeric(rowsum(w, g))
    est <- num / den
    tibble::tibble(group = levels(g), estimate = rowMeans(est),
                   lower_ci = apply(est, 1, stats::quantile, 0.025, names = FALSE),
                   upper_ci = apply(est, 1, stats::quantile, 0.975, names = FALSE),
                   n_unweighted = as.numeric(rowsum(grouped$n, g)), sum_weight = den)
  })
  list(fit = fit, state = summaries$state |> dplyr::rename(region = group),
       state_cohort = summaries$state_cohort |> tidyr::separate(group, c("region", "birth_cohort"), sep = "\\|"),
       cohort = summaries$cohort |> dplyr::rename(birth_cohort = group),
       survey = summaries$survey |> dplyr::rename(survey_id = group),
       national = summaries$national |> dplyr::select(-group),
       fixed = tibble::as_tibble(fit$summary.fixed, rownames = "term"),
       residual = tibble::tibble(region = region_keys,
                                 residual_effect = fit$summary.random$region_id$mean[seq_along(region_keys)]),
       fit_stats = tibble::tibble(dic = fit$dic$dic, waic = fit$waic$waic, rows = nrow(grouped),
                                  respondents = sum(grouped$n)))
}

# Pooled design-based direct estimate by a grouping variable (each survey keeps its own
# clusters and strata inside the pooled design).
pooled_direct <- function(data, outcome, by) {
  d <- data |> dplyr::filter(!is.na(.data[[outcome]]))
  design <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
  est <- survey::svyby(stats::as.formula(paste0("~", outcome)), stats::as.formula(paste0("~", by)), design, survey::svymean)
  tibble::tibble(!!by := est[[by]], direct = as.numeric(est[[outcome]]), se = as.numeric(est$se)) |>
    dplyr::mutate(lower_ci = pmax(direct - 1.96 * se, 0), upper_ci = pmin(direct + 1.96 * se, 1),
                  n = as.integer(table(d[[by]])[as.character(.data[[by]])]))
}

# One map per estimate set (V1 style) and a faceted birth-cohort map.
state_map <- function(estimates, value_col, title_text, subtitle_text, legend_title, fill_limits) {
  map_data <- boundaries |> dplyr::mutate(region = region_keys) |> dplyr::left_join(estimates, by = "region")
  ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[value_col]]), color = "grey40", linewidth = 0.15) +
    ggplot2::scale_fill_viridis_c(name = legend_title, option = "magma", direction = -1,
                                  labels = scales::percent_format(accuracy = 1), limits = fill_limits,
                                  oob = scales::squish, na.value = "grey90") +
    ggplot2::coord_sf(datum = sf::st_crs(4326)) +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = "Longitude", y = "Latitude") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                   panel.border = ggplot2::element_rect(color = "grey40", fill = NA, linewidth = 0.4),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8))
}
cohort_map <- function(estimates, value_col, title_text, subtitle_text, legend_title, fill_limits) {
  map_data <- boundaries |> dplyr::mutate(region = region_keys) |>
    dplyr::left_join(estimates, by = "region", relationship = "one-to-many") |> dplyr::filter(!is.na(birth_cohort))
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = boundaries, fill = "grey90", color = "grey60", linewidth = 0.1) +
    ggplot2::geom_sf(data = map_data, ggplot2::aes(fill = .data[[value_col]]), color = "grey40", linewidth = 0.12) +
    ggplot2::facet_wrap(~birth_cohort, ncol = 4) +
    ggplot2::scale_fill_viridis_c(name = legend_title, option = "magma", direction = -1,
                                  labels = scales::percent_format(accuracy = 1), limits = fill_limits, oob = scales::squish) +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                   strip.text = ggplot2::element_text(face = "bold", size = 10),
                   strip.background = ggplot2::element_rect(fill = "grey90", color = "grey40"),
                   axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}
residual_map <- function(residuals, title_text, subtitle_text) {
  lim <- max(abs(residuals$residual_effect), na.rm = TRUE)
  ggplot2::ggplot(boundaries |> dplyr::mutate(region = region_keys) |> dplyr::left_join(residuals, by = "region")) +
    ggplot2::geom_sf(ggplot2::aes(fill = residual_effect), color = "grey40", linewidth = 0.15) +
    ggplot2::scale_fill_gradient2(name = "Residual\nlog-odds", low = "#1D4E89", mid = "white", high = "#D9534F",
                                  midpoint = 0, limits = c(-lim, lim)) +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = "Longitude", y = "Latitude") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8))
}
