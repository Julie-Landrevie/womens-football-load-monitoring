# =============================================================================
# 03_load_metrics.R — Indicateurs de charge et de wellness, recalculés
#
# Charge interne : sRPE (Foster 2001) = RPE x durée (min), cumulée par jour
# (variable `daily_load` de SoccerMon).
#   - Aiguë / chronique en moyennes glissantes (7 j / 28 j)  -> acwr_ra
#   - Aiguë / chronique en moyennes exponentielles (EWMA,
#     Williams et al. 2017), moins sensible aux pics isolés  -> acwr_ewma
#   - Monotonie (moyenne / écart-type sur 7 j) et strain (Foster 1998)
# Wellness : z-score de chaque item par rapport à la référence glissante
# PROPRE à chaque joueuse (28 j précédents), puis score composite.
#
# Choix de traitement des données manquantes (documenté, réglable) :
#   - dans SoccerMon, un jour sans déclaration de charge vaut 0 : repos et
#     non-déclaration sont indiscernables dans daily_load ;
#   - on mesure donc une couverture "documentée" sur 28 j (charge > 0 ou
#     wellness rempli) et les alertes ne sont émises que si elle est suffisante
#     (CONFIG$min_coverage_chronic).
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

# --- utilitaires de fenêtres glissantes (alignées à droite) -----------------
roll_apply <- function(x, k, fun, min_obs = k) {
  n <- length(x)
  vapply(seq_len(n), function(i) {
    if (i < k) return(NA_real_)
    w <- x[(i - k + 1):i]
    if (sum(!is.na(w)) < min_obs) return(NA_real_)
    fun(w)
  }, numeric(1))
}
roll_mean <- function(x, k) roll_apply(x, k, mean)
roll_sum  <- function(x, k) roll_apply(x, k, sum)
roll_sd   <- function(x, k) roll_apply(x, k, sd)

ewma <- function(x, span) {
  lambda <- 2 / (span + 1)
  out <- numeric(length(x))
  out[1] <- x[1]
  for (i in seq_along(x)[-1]) out[i] <- lambda * x[i] + (1 - lambda) * out[i - 1]
  out
}

safe_div <- function(a, b) ifelse(!is.na(b) & b > 0, a / b, NA_real_)

# --- calendrier complet par joueuse, limité à sa période active --------------
# Période active = du premier au dernier jour avec une charge > 0 : on écarte
# les mois "remplis de zéros" avant l'arrivée ou après le départ d'une joueuse.
complete_calendar <- function(wide) {
  wide |>
    group_by(player, team, label) |>
    complete(date = seq(min(date), max(date), by = "day")) |>
    filter(any(coalesce(daily_load, 0) > 0)) |>
    filter(date >= min(date[coalesce(daily_load, 0) > 0]),
           date <= max(date[coalesce(daily_load, 0) > 0])) |>
    ungroup() |>
    arrange(player, date)
}

# --- indicateurs de charge ---------------------------------------------------
compute_load_metrics <- function(wide, cfg = CONFIG) {
  a <- cfg$acute_window; c <- cfg$chronic_window
  well <- intersect(WELLNESS_VARS, names(wide))
  complete_calendar(wide) |>
    mutate(any_wellness = rowSums(!is.na(pick(all_of(well)))) > 0) |>
    group_by(player) |>
    mutate(
      # Jour "documenté" : charge > 0 OU questionnaire wellness rempli.
      # (un 0 seul ne prouve pas que la joueuse a déclaré un repos)
      load_reported = coalesce(daily_load, 0) > 0 | any_wellness,
      load          = coalesce(daily_load, 0),
      day_index     = row_number(),
      coverage_28   = roll_mean(as.numeric(load_reported), c),

      acute_ra      = roll_mean(load, a),
      chronic_ra    = roll_mean(load, c),
      acwr_ra       = safe_div(acute_ra, chronic_ra),

      acute_ewma    = ewma(load, a),
      chronic_ewma  = ewma(load, c),
      acwr_ewma     = if_else(day_index >= c, safe_div(acute_ewma, chronic_ewma), NA_real_),

      weekly_load_calc = roll_sum(load, a),
      monotony_calc    = safe_div(roll_mean(load, a), roll_sd(load, a)),
      strain_calc      = weekly_load_calc * monotony_calc,

      interpretable = !is.na(coverage_28) & coverage_28 >= cfg$min_coverage_chronic
    ) |>
    ungroup()
}

# --- wellness : z-score individuel -------------------------------------------
compute_wellness_z <- function(metrics, cfg = CONFIG) {
  items <- intersect(WELLNESS_VARS, names(metrics))
  k <- cfg$wellness_baseline_window; m <- cfg$wellness_baseline_min

  z_item <- function(x, higher_is_better) {
    # référence = jours PRÉCÉDENTS uniquement (pas de fuite du jour courant)
    prev <- dplyr::lag(x)
    mu <- roll_apply(prev, k, function(w) mean(w, na.rm = TRUE), min_obs = m)
    s  <- roll_apply(prev, k, function(w) sd(w, na.rm = TRUE),   min_obs = m)
    z  <- ifelse(!is.na(s) & s > 0, (x - mu) / s, NA_real_)
    if (higher_is_better) z else -z   # négatif = moins bien que d'habitude
  }

  out <- metrics |> group_by(player)
  for (it in items) {
    hib <- cfg$wellness_higher_is_better[[it]]
    out <- out |> mutate("z_{it}" := z_item(.data[[it]], hib))
  }
  zcols <- paste0("z_", items)
  out |>
    ungroup() |>
    mutate(wellness_z = {
      zm <- as.matrix(pick(all_of(zcols)))
      n_ok <- rowSums(!is.na(zm))
      ifelse(n_ok >= 3, rowMeans(zm, na.rm = TRUE), NA_real_)
    })
}

# --- points d'attention ------------------------------------------------------
# Contexte équipe : une coupure ou une reprise PLANIFIÉE fait varier l'ACWR de
# toute l'équipe en même temps. On ne signale une joueuse que si elle s'écarte
# de son équipe : ACWR individuel hors zone ALORS QUE la médiane de l'équipe
# ce jour-là est dans la zone.
compute_flags <- function(metrics, cfg = CONFIG) {
  k <- cfg$alert_persistence; mg <- cfg$team_margin
  metrics |>
    group_by(team, date) |>
    mutate(team_acwr_median = median(acwr_ewma[interpretable], na.rm = TRUE),
           team_acwr_median = ifelse(is.nan(team_acwr_median), NA_real_, team_acwr_median)) |>
    ungroup() |>
    mutate(
      team_in_zone = !is.na(team_acwr_median) &
                     team_acwr_median >= cfg$acwr_low & team_acwr_median <= cfg$acwr_high,
      # conditions brutes du jour
      raw_high = interpretable & team_in_zone & !is.na(acwr_ewma) & acwr_ewma > cfg$acwr_high &
                 acwr_ewma - team_acwr_median >= mg,
      raw_low  = interpretable & team_in_zone & !is.na(acwr_ewma) & acwr_ewma < cfg$acwr_low &
                 team_acwr_median - acwr_ewma >= mg & !is.na(chronic_ewma) & chronic_ewma > 0,
      raw_mono = interpretable & !is.na(monotony_calc) & monotony_calc > cfg$monotony_high
    ) |>
    arrange(player, date) |>
    group_by(player) |>
    mutate(
      # persistance : alerte à partir du k-ième jour consécutif
      flag_acwr_high = persistent(raw_high, k),
      flag_acwr_low  = persistent(raw_low, k),
      flag_monotony  = persistent(raw_mono, k),
      # le wellness est un signal du jour même : pas de persistance exigée
      flag_wellness  = !is.na(wellness_z) & wellness_z < cfg$wellness_z_alert,
      n_flags = flag_acwr_high + flag_acwr_low + flag_monotony + flag_wellness,
      # début d'un épisode d'alerte (jours consécutifs avec alerte = 1 épisode)
      episode_start = n_flags > 0 & dplyr::lag(n_flags, default = 0) == 0
    ) |>
    ungroup() |>
    select(-raw_high, -raw_low, -raw_mono)
}

#' Vrai à partir du k-ième jour consécutif où x est vrai
persistent <- function(x, k) {
  x <- coalesce(x, FALSE)
  if (k <= 1) return(x)
  r <- rle(x)
  pos <- sequence(r$lengths)            # rang du jour dans sa séquence
  x & pos >= k
}

#' Résumé hebdomadaire par joueuse (pour la vue équipe)
weekly_summary <- function(metrics) {
  metrics |>
    mutate(week = as.Date(cut(date, "week", start.on.monday = TRUE))) |>
    group_by(team, player, label, week) |>
    summarise(
      weekly_load   = sum(load),
      days_reported = sum(load_reported),
      acwr_ewma_end = last(acwr_ewma),
      wellness_z    = mean(wellness_z, na.rm = TRUE),
      flags         = sum(n_flags, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(wellness_z = ifelse(is.nan(wellness_z), NA_real_, wellness_z))
}

#' Charge et wellness autour des blessures déclarées (analyse exploratoire)
injury_windows <- function(metrics, injuries, before = 21) {
  if (nrow(injuries) == 0) return(NULL)
  injuries |>
    distinct(player, date) |>
    mutate(injury_id = row_number()) |>
    inner_join(metrics |> select(player, date_m = date, load, acwr_ewma, wellness_z),
               by = "player", relationship = "many-to-many") |>
    mutate(days_to_injury = as.integer(date_m - date)) |>
    filter(days_to_injury >= -before, days_to_injury <= 0)
}
