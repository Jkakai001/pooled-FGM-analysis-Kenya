# County-level estimates (47 counties) from DHS 2014 and DHS 2022, the only rounds that record
# counties. Design-based direct estimates by county, then area-level BYM2 smoothing on the
# logit scale with the direct sampling variance as known precision (Fay-Herriot type), using
# PC priors and robust INLA settings. Small counties borrow strength from their neighbours.

county_boundaries <- function(project_root) {
  sf::st_read(file.path(project_root, "02_Data", "processed", "KEN", "boundaries", "KEN_counties.shp"), quiet = TRUE) |>
    sf::st_make_valid() |> dplyr::arrange(COUNTY)
}

county_direct <- function(d, var) {
  d <- d |> dplyr::filter(!is.na(.data[[var]]), !is.na(county), is.finite(weight), weight > 0)
  des <- survey::svydesign(ids = ~cluster, strata = ~strata, weights = ~weight, data = d, nest = TRUE)
  est <- survey::svyby(stats::as.formula(paste0("~", var)), ~county, des, survey::svymean, na.rm = TRUE)
  tibble::tibble(county = est$county, direct = as.numeric(est[[var]]), se = as.numeric(est$se),
                 n = as.integer(table(d$county)[est$county]))
}

county_smooth <- function(direct, boundaries) {
  nb <- spdep::poly2nb(boundaries, row.names = boundaries$COUNTY)
  graph <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)
  eps <- 1e-3
  d <- boundaries |> sf::st_drop_geometry() |> dplyr::select(county = COUNTY) |>
    dplyr::left_join(direct, by = "county") |>
    dplyr::mutate(id = dplyr::row_number(), p = pmin(pmax(direct, eps), 1 - eps), se_p = pmax(se, 0.005),
                  logit_est = dplyr::if_else(is.na(direct), NA_real_, qlogis(p)),
                  logit_prec = dplyr::if_else(is.na(direct), 1, pmin((p * (1 - p))^2 / se_p^2, 1e4)))
  fit <- NULL
  for (attempt in 1:4) {
    fit <- tryCatch(INLA::inla(
      logit_est ~ 1 + f(id, model = "bym2", graph = graph, scale.model = TRUE,
                        hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)),
                                     phi = list(prior = "pc", param = c(0.5, 2 / 3)))),
      family = "gaussian", data = d, scale = d$logit_prec,
      control.family = list(hyper = list(prec = list(fixed = TRUE, initial = 0))),
      control.predictor = list(compute = TRUE),
      control.mode = list(theta = c(0, 0), restart = TRUE), num.threads = "1:1"), error = function(e) NULL)
    if (!is.null(fit)) break
  }
  stopifnot(!is.null(fit))
  d |> dplyr::transmute(county, direct, se, n,
                        smoothed = plogis(fit$summary.linear.predictor$mean),
                        lower_ci = plogis(fit$summary.linear.predictor$`0.025quant`),
                        upper_ci = plogis(fit$summary.linear.predictor$`0.975quant`))
}

county_map <- function(est, boundaries, title_text, subtitle_text, legend_title, limits = NULL) {
  m <- boundaries |> dplyr::left_join(est, by = c("COUNTY" = "county"))
  if (is.null(limits)) limits <- c(0, max(est$smoothed, na.rm = TRUE))
  ggplot2::ggplot(m) +
    ggplot2::geom_sf(ggplot2::aes(fill = smoothed), color = "grey40", linewidth = 0.12) +
    ggplot2::facet_wrap(~survey_id) +
    ggplot2::scale_fill_viridis_c(name = legend_title, option = "magma", direction = -1,
                                  labels = scales::percent_format(accuracy = 1), limits = limits, oob = scales::squish) +
    ggplot2::labs(title = title_text, subtitle = subtitle_text, x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey30"),
                   strip.text = ggplot2::element_text(face = "bold", size = 12),
                   strip.background = ggplot2::element_rect(fill = "grey90", color = "grey40"),
                   axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

# Direct + smoothed county estimates for one outcome in each requested round, plus a two-panel map.
county_series <- function(data, var, rounds, boundaries) {
  dplyr::bind_rows(lapply(rounds, function(r) {
    d <- data |> dplyr::filter(survey_id == r)
    county_smooth(county_direct(d, var), boundaries) |> dplyr::mutate(survey_id = r)
  }))
}
