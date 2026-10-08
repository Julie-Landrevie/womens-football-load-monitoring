# =============================================================================
# 05_dashboard.R — Export des données du tableau de bord web
#
# Le tableau de bord est une page HTML statique (docs/index.html) publiée avec
# GitHub Pages. Le pipeline R écrit ses données dans docs/data.js ; la page les
# lit et gère l'interactivité (filtres, survol, zoom) dans le navigateur.
# Format compact : une série journalière continue par joueuse (la date se
# déduit de l'indice), valeurs arrondies.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(jsonlite) })

# Points d'attention codés en bits : 1 hausse rapide, 2 sous-charge,
# 4 monotonie, 8 wellness
flag_bits <- function(m) {
  as.integer(coalesce(m$flag_acwr_high, FALSE)) * 1L +
    as.integer(coalesce(m$flag_acwr_low, FALSE)) * 2L +
    as.integer(coalesce(m$flag_monotony, FALSE)) * 4L +
    as.integer(coalesce(m$flag_wellness, FALSE)) * 8L
}

rnd <- function(x, d) ifelse(is.na(x) | !is.finite(x), NA, round(x, d))

export_dashboard <- function(res, out_dir = "docs") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  m <- res$metrics |> arrange(label, date)
  m$flags <- flag_bits(m)

  players <- m |>
    group_by(id = label, team) |>
    summarise(first = format(min(date)), last = format(max(date)),
              days = n(),
              load_share = round(mean(load > 0), 3),
              wellness_share = round(mean(!is.na(wellness_z)), 3),
              .groups = "drop") |>
    arrange(id)

  series <- split(m, m$label) |>
    lapply(function(d) {
      out <- list(
        start   = format(min(d$date)),
        load    = I(rnd(d$load, 0)),
        acute   = I(rnd(d$acute_ewma, 0)),
        chronic = I(rnd(d$chronic_ewma, 0)),
        acwr    = I(rnd(ifelse(d$interpretable, d$acwr_ewma, NA), 2)),
        acwr_raw = I(rnd(d$acwr_ewma, 2)),
        mono    = I(rnd(d$monotony_calc, 2)),
        well    = I(rnd(d$wellness_z, 2)),
        cov     = I(rnd(d$coverage_28, 2)),
        doc     = I(as.integer(d$load_reported)),
        flags   = I(d$flags)
      )
      # niveau de la base : charge chronique / normale de la joueuse
      if ("base_rel" %in% names(d)) out$base <- I(rnd(d$base_rel, 2))
      if ("total_km" %in% names(d)) {
        dd <- d
        out$gps_km     <- I(rnd(dd$total_km, 2))
        out$gps_hsr    <- I(rnd(dd$hsr_m, 0))
        out$gps_sprint <- I(rnd(dd$sprint_m, 0))
        out$gps_acc    <- I(rnd(dd$n_acc, 0))
      }
      # ACWR GPS : valeurs absolues (m/jour) et ratio, masqués si couverture insuffisante
      # (sinon les jours de repos seuls font chuter les moyennes quand le GPS n'est pas porté)
      if ("hsr_acwr" %in% names(d)) {
        gi <- coalesce(d$gps_interpretable, FALSE)
        out$hsr_a    <- I(rnd(ifelse(gi, d$hsr_acute, NA), 0))
        out$hsr_c    <- I(rnd(ifelse(gi, d$hsr_chronic, NA), 0))
        out$hsr_acwr <- I(rnd(ifelse(gi, d$hsr_acwr, NA), 2))
        out$hsr_base <- I(rnd(ifelse(gi, d$hsr_base_rel, NA), 2))
        out$spr_a    <- I(rnd(ifelse(gi, d$sprint_acute, NA), 1))
        out$spr_c    <- I(rnd(ifelse(gi, d$sprint_chronic, NA), 1))
        out$spr_acwr <- I(rnd(ifelse(gi, d$sprint_acwr, NA), 2))
        out$spr_base <- I(rnd(ifelse(gi, d$sprint_base_rel, NA), 2))
        out$gps_cov  <- I(rnd(d$gps_coverage_28, 2))
      }
      out
    })

  inj <- res$injuries |>
    inner_join(distinct(m, player, label), by = "player") |>
    distinct(label, date) |>
    group_by(label) |> summarise(dates = list(format(sort(date))), .groups = "drop")
  injuries <- setNames(lapply(inj$dates, I), inj$label)

  comp <- res$completeness |>
    transmute(id = label, team, var = variable, v = round(completeness, 3))

  prof <- NULL
  if (!is.null(res$inj_win) && nrow(res$inj_win) > 0) {
    prof <- res$inj_win |>
      group_by(d = days_to_injury) |>
      summarise(acwr_med = median(acwr_ewma, na.rm = TRUE),
                acwr_q1 = quantile(acwr_ewma, .25, na.rm = TRUE),
                acwr_q3 = quantile(acwr_ewma, .75, na.rm = TRUE),
                well_med = median(wellness_z, na.rm = TRUE),
                well_q1 = quantile(wellness_z, .25, na.rm = TRUE),
                well_q3 = quantile(wellness_z, .75, na.rm = TRUE),
                .groups = "drop") |>
      mutate(across(-d, ~ round(.x, 3)))
    prof <- c(as.list(prof), n = n_distinct(res$inj_win$injury_id))
  }

  ie <- NULL
  if ("total_km" %in% names(m)) {
    pts <- m |> filter(load > 0, !is.na(total_km), total_km > 0)
    ie <- lapply(split(pts, pts$team), function(x) list(
      load = I(round(x$load)), km = I(round(x$total_km, 2)), id = I(x$label),
      r = round(cor(x$load, x$total_km), 2), n = nrow(x)))
  }

  cs <- res$consistency
  data <- list(
    meta = list(
      generated = format(Sys.Date()),
      source = "SoccerMon (Midoglu et al., Scientific Data, 2024)",
      thresholds = list(acwr_low = res$config$acwr_low, acwr_high = res$config$acwr_high,
                        monotony_high = res$config$monotony_high,
                        wellness_z = res$config$wellness_z_alert,
                        min_coverage = res$config$min_coverage_chronic,
                        base_low = res$config$base_low, base_high = res$config$base_high,
                        min_coverage_gps = res$config$min_coverage_gps),
      has_gps = "total_km" %in% names(m)
    ),
    teams = sort(unique(players$team)),
    players = players,
    series = series,
    injuries = injuries,
    qc = list(
      summary = res$summary_qc,
      completeness = comp,
      consistency = if (!is.null(cs) && nrow(cs)) as.list(cs) else NULL
    ),
    injury_profile = prof,
    gps_qc = res$gps_qc,
    internal_external = ie,
    exposure = res$exposure
  )

  js <- paste0("// Généré par run_pipeline.R — ne pas modifier à la main\nwindow.DASHBOARD_DATA = ",
               toJSON(data, auto_unbox = TRUE, na = "null", digits = NA, dataframe = "columns"),
               ";\n")
  writeLines(js, file.path(out_dir, "data.js"), useBytes = TRUE)
  message("   -> ", file.path(out_dir, "data.js"), " (",
          format(round(file.size(file.path(out_dir, "data.js")) / 1e6, 1)), " Mo)")
  invisible(data)
}
