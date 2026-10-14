# =============================================================================
# 07_return.R — Retour au jeu : comment une joueuse retrouve son niveau
#                après une absence
#
# Principe (inspiré du suivi « return to performance » en club) : on compare
# chaque semaine qui suit la reprise à la référence PROPRE de la joueuse, les
# 28 jours qui précèdent l'absence. On suit plusieurs qualités qui ne
# reviennent pas au même rythme :
#   - la charge perçue (sRPE hebdomadaire) ;
#   - l'exposition à haute intensité (GPS, seuils féminins : > 19 km/h) ;
#   - l'explosivité : meilleur temps pour atteindre 19 km/h (GPS) ;
#   - la vitesse maximale atteinte (GPS) ;
#   - le profil accélération-vitesse mesuré en séance (A0, S0), si les
#     fichiers GPS bruts ont été traités par run_gps.R (08_as_profile.R).
#
# Absence = au moins `return_min_absence` jours consécutifs sans charge
# déclarée ni fichier GPS. Elle est dite :
#   - « individuelle » si au moins la moitié de l'équipe s'entraîne pendant
#     ce temps (blessure, maladie, sélection, raison personnelle...) ;
#   - « collective » sinon (intersaison, coupure).
# SoccerMon ne donne pas la cause d'une absence : une absence peut aussi être
# une période sans déclaration. Une blessure déclarée autour du début de
# l'absence est signalée, sans diagnostic (pas d'information sur le LCA).
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

#' Repère les absences suivies d'une reprise
detect_absences <- function(metrics, injuries = NULL, cfg = CONFIG) {
  has_gps <- "total_km" %in% names(metrics)
  m <- metrics |>
    arrange(player, date) |>
    mutate(active = load > 0 | (if (has_gps) !is.na(total_km) else FALSE))
  team_act <- m |> group_by(team, date) |> summarise(team_active = mean(active), .groups = "drop")
  runs <- m |>
    group_by(team, player, label) |>
    group_modify(function(g, key) {
      r <- rle(!g$active)
      end <- cumsum(r$lengths); start <- end - r$lengths + 1
      keep <- r$values & r$lengths >= cfg$return_min_absence & end < nrow(g)
      if (!any(keep)) return(tibble())
      tibble(absence_start = g$date[start[keep]], absence_end = g$date[end[keep]],
             return_date = g$date[end[keep] + 1], absence_days = r$lengths[keep],
             days_before = start[keep] - 1)
    }) |>
    ungroup()
  if (nrow(runs) == 0) return(runs)
  runs <- runs |>
    rowwise() |>
    mutate(team_active = mean(team_act$team_active[team_act$team == team &
                                                     team_act$date >= absence_start &
                                                     team_act$date <= absence_end])) |>
    ungroup() |>
    mutate(type = if_else(team_active >= 0.5, "individuelle", "collective"))
  inj <- if (!is.null(injuries) && nrow(injuries)) distinct(injuries, player, date) else tibble(player = character(), date = as.Date(character()))
  runs |>
    rowwise() |>
    mutate(injury_declared = any(inj$player == player &
                                   inj$date >= absence_start - 14 & inj$date <= absence_end)) |>
    ungroup() |>
    arrange(label, absence_start) |>
    mutate(episode = paste0(label, " · ", format(return_date, "%d/%m/%Y")))
}

#' Ratios semaine par semaine après la reprise, par rapport aux 28 jours
#' précédant l'absence. as_sessions : sortie de run_gps.R (facultatif).
return_curves <- function(metrics, absences, as_sessions = NULL, cfg = CONFIG) {
  if (is.null(absences) || nrow(absences) == 0) return(NULL)
  has_gps <- all(c("total_km", "gps_hi_m", "gps_vmax") %in% names(metrics))
  base_n <- cfg$return_baseline_days
  out <- vector("list", nrow(absences))
  for (i in seq_len(nrow(absences))) {
    a <- absences[i, ]
    g <- metrics[metrics$player == a$player, ]
    win <- function(d0, d1) g[g$date >= d0 & g$date <= d1, ]
    b <- win(a$absence_start - base_n, a$absence_start - 1)
    if (nrow(b) < 0.75 * base_n || sum(b$load) <= 0) next
    b_week_load <- sum(b$load) / nrow(b) * 7
    b_gps <- if (has_gps) b[!is.na(b$total_km), ] else b[0, ]
    b_active <- sum(b$load > 0 | (if (has_gps) !is.na(b$total_km) else FALSE))
    gps_ok_b <- has_gps && b_active > 0 && nrow(b_gps) / b_active >= cfg$return_min_gps_coverage
    # Référence comparable à une semaine : on ne compare pas le maximum d'une
    # semaine au maximum de 4 semaines (plus de séances = maximum plus élevé).
    # Vitesse max et profil A-S de référence = médiane des valeurs hebdomadaires.
    b_weeks <- lapply(seq_len(base_n %/% 7), function(j) {
      d0 <- a$absence_start - base_n + 7 * (j - 1)
      list(gps = b_gps[b_gps$date >= d0 & b_gps$date <= d0 + 6, ],
           as = if (!is.null(as_sessions)) as_fit(as_sessions[as_sessions$player == a$player &
                                                                as_sessions$date >= d0 & as_sessions$date <= d0 + 6, ],
                                                  cfg, min_sessions = 2) else NULL)
    })
    wk_max <- function(x, col) { v <- x[[col]]; v <- v[!is.na(v)]; if (length(v)) max(v) else NA_real_ }
    wk_min <- function(x, col) { v <- x[[col]]; v <- v[!is.na(v)]; if (length(v)) min(v) else NA_real_ }
    b_vmax_w <- vapply(b_weeks, function(x) wk_max(x$gps, "gps_vmax"), numeric(1))
    b_t19_w  <- vapply(b_weeks, function(x) if ("gps_t19" %in% names(x$gps)) wk_min(x$gps, "gps_t19") else NA_real_, numeric(1))
    t19_b <- if (sum(!is.na(b_t19_w)) >= 2) stats::median(b_t19_w, na.rm = TRUE) else NA_real_
    vmax_b <- if (sum(!is.na(b_vmax_w)) >= 2) stats::median(b_vmax_w, na.rm = TRUE) else NA_real_
    b_fits <- Filter(Negate(is.null), lapply(b_weeks, `[[`, "as"))
    b_as <- if (length(b_fits) >= 2) list(a0 = stats::median(vapply(b_fits, `[[`, 0, "a0")),
                                          s0 = stats::median(vapply(b_fits, `[[`, 0, "s0"))) else NULL
    rows <- lapply(seq_len(cfg$return_weeks), function(k) {
      w <- win(a$return_date + 7 * (k - 1), a$return_date + 7 * k - 1)
      if (nrow(w) < 7) return(NULL)
      w_gps <- if (has_gps) w[!is.na(w$total_km), ] else w[0, ]
      w_active <- sum(w$load > 0 | (if (has_gps) !is.na(w$total_km) else FALSE))
      gps_ok_w <- gps_ok_b && w_active > 0 && nrow(w_gps) / w_active >= cfg$return_min_gps_coverage
      # exposition hebdomadaire estimée = moyenne par jour GPS x jours actifs
      hsr_week  <- if (gps_ok_w) mean(w_gps$gps_hi_m) * w_active else NA_real_
      hsr_base  <- if (gps_ok_b) mean(b_gps$gps_hi_m) * b_active / nrow(b) * 7 else NA_real_
      vmax_w <- if (has_gps && nrow(w_gps) >= 1) wk_max(w_gps, "gps_vmax") else NA_real_
      t19_w  <- if (has_gps && "gps_t19" %in% names(w_gps)) wk_min(w_gps, "gps_t19") else NA_real_
      w_as <- if (!is.null(b_as) && !is.null(as_sessions)) as_fit(as_sessions[as_sessions$player == a$player &
                                                                                as_sessions$date >= a$return_date + 7 * (k - 1) &
                                                                                as_sessions$date <= a$return_date + 7 * k - 1, ], cfg, min_sessions = 2) else NULL
      tibble(week = k,
             srpe_week = sum(w$load), srpe_base = b_week_load,
             hsr_week = hsr_week, hsr_base = hsr_base,
             vmax_week = vmax_w, vmax_base = vmax_b,
             t19_week = t19_w, t19_base = t19_b,
             a0_week = if (is.null(w_as)) NA_real_ else w_as$a0,
             a0_base = if (is.null(b_as)) NA_real_ else b_as$a0,
             s0_week = if (is.null(w_as)) NA_real_ else w_as$s0,
             s0_base = if (is.null(b_as)) NA_real_ else b_as$s0)
    })
    rows <- bind_rows(rows)
    if (nrow(rows) == 0) next
    out[[i]] <- bind_cols(a[rep(1, nrow(rows)), c("team", "player", "label", "episode", "type",
                                                  "absence_days", "return_date", "injury_declared")], rows)
  }
  res <- bind_rows(out)
  if (nrow(res) == 0) return(NULL)
  res |>
    mutate(srpe = safe_div(srpe_week, srpe_base),
           hsr  = safe_div(hsr_week, hsr_base),
           vmax = safe_div(vmax_week, vmax_base),
           # explosivité : temps de référence / temps de la semaine (100 % = aussi rapide, < 100 % = plus lente)
           explo = safe_div(t19_base, t19_week),
           a0   = safe_div(a0_week, a0_base),
           s0   = safe_div(s0_week, s0_base))
}

#' Courbe moyenne par type d'absence : médiane et intervalle interquartile
return_summary <- function(curves) {
  if (is.null(curves)) return(NULL)
  curves |>
    pivot_longer(c(srpe, hsr, vmax, explo, a0, s0), names_to = "metric", values_to = "ratio") |>
    filter(!is.na(ratio), is.finite(ratio)) |>
    group_by(type, metric, week) |>
    summarise(n = n(), med = median(ratio), q1 = quantile(ratio, .25), q3 = quantile(ratio, .75),
              .groups = "drop")
}

#' Semaines nécessaires pour retrouver `level` (90 %) de la référence
return_recovery <- function(curves, level = 0.9) {
  if (is.null(curves)) return(NULL)
  curves |>
    pivot_longer(c(srpe, hsr, vmax, explo, a0, s0), names_to = "metric", values_to = "ratio") |>
    filter(!is.na(ratio), is.finite(ratio)) |>
    group_by(type, episode, metric) |>
    summarise(weeks_followed = n(),
              week_reached = if (any(ratio >= level)) min(week[ratio >= level]) else NA_real_,
              .groups = "drop") |>
    group_by(type, metric) |>
    summarise(episodes = n(),
              reached = sum(!is.na(week_reached)),
              share_reached = mean(!is.na(week_reached)),
              median_week = if (any(!is.na(week_reached))) median(week_reached, na.rm = TRUE) else NA_real_,
              .groups = "drop")
}

#' L'ACWR pendant les 28 jours qui suivent une reprise : part des jours où il
#' est masqué (couverture insuffisante) et part des jours où le ratio brut
#' dépasse le seuil haut. Montre pourquoi un suivi dédié à la reprise est utile.
return_acwr_gap <- function(metrics, absences, cfg = CONFIG) {
  if (is.null(absences) || nrow(absences) == 0) return(NULL)
  absences |>
    select(type, episode, player, return_date) |>
    inner_join(metrics |> select(player, date, interpretable, acwr_ewma), by = "player",
               relationship = "many-to-many") |>
    filter(date >= return_date, date < return_date + 28) |>
    group_by(type, episode) |>
    summarise(masked = mean(!interpretable), raw_high = mean(coalesce(acwr_ewma, 0) > cfg$acwr_high),
              .groups = "drop") |>
    group_by(type) |>
    summarise(episodes = n(), masked_median = median(masked), raw_high_median = median(raw_high),
              .groups = "drop")
}

#' Tout le module en un appel (utilisé par run_pipeline.R)
compute_returns <- function(metrics, injuries, as_sessions = NULL, cfg = CONFIG) {
  ab <- detect_absences(metrics, injuries, cfg)
  if (nrow(ab) == 0) return(NULL)
  cv <- return_curves(metrics, ab, as_sessions, cfg)
  sm <- return_summary(cv)
  rc <- return_recovery(cv)
  gap <- return_acwr_gap(metrics, ab, cfg)
  ind <- function(df, mt, wk) {
    if (is.null(df)) return(NA_real_)
    x <- df$med[df$type == "individuelle" & df$metric == mt & df$week == wk]
    if (length(x)) round(x, 2) else NA_real_
  }
  stats <- list(
    absences_individual = sum(ab$type == "individuelle"),
    absences_collective = sum(ab$type == "collective"),
    absences_injury = sum(ab$injury_declared),
    absence_days_median_individual = median(ab$absence_days[ab$type == "individuelle"]),
    curves_individual = if (is.null(cv)) 0L else n_distinct(cv$episode[cv$type == "individuelle"]),
    curves_collective = if (is.null(cv)) 0L else n_distinct(cv$episode[cv$type == "collective"]),
    srpe_w1_individual = ind(sm, "srpe", 1),
    srpe_w2_individual = ind(sm, "srpe", 2),
    srpe_w3_individual = ind(sm, "srpe", 3),
    srpe_weeks_to_90_individual = if (is.null(rc)) NA else rc$median_week[rc$type == "individuelle" & rc$metric == "srpe"][1],
    srpe_w1_collective = if (is.null(sm)) NA else round(sm$med[sm$type == "collective" & sm$metric == "srpe" & sm$week == 1][1], 2),
    vmax_w1_collective = if (is.null(sm)) NA else round(sm$med[sm$type == "collective" & sm$metric == "vmax" & sm$week == 1][1], 2),
    gps_curves_individual = if (is.null(cv)) 0L else n_distinct(cv$episode[cv$type == "individuelle" & !is.na(cv$vmax)]),
    acwr_masked_after_return = if (is.null(gap)) NA else round(gap$masked_median[gap$type == "individuelle"][1], 2),
    acwr_raw_high_after_return = if (is.null(gap)) NA else round(gap$raw_high_median[gap$type == "individuelle"][1], 2)
  )
  list(absences = ab, curves = cv, summary = sm, recovery = rc, acwr_gap = gap, stats = stats)
}
