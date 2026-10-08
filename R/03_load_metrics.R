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

# =============================================================================
# Valeurs absolues et niveau de base
#
# Un ACWR de 0,78 dit seulement que l'aigu est 22 % sous le chronique. Il ne
# dit pas si la joueuse s'entraîne beaucoup ou peu. On ajoute donc :
#   - les valeurs absolues (aiguë et chronique, en UA/jour ou en m/jour) ;
#   - le niveau de la base : charge chronique / normale de la joueuse ;
#   - l'ACWR sur la charge externe (course > 16 km/h, sprint > 20 km/h).
# =============================================================================

#' EWMA tolérante aux jours manquants : un NA ne met pas à jour la moyenne
#' (la dernière valeur connue est reportée), contrairement à un 0.
ewma_na <- function(x, span) {
  lambda <- 2 / (span + 1)
  out <- rep(NA_real_, length(x)); prev <- NA_real_
  for (i in seq_along(x)) {
    if (!is.na(x[i])) prev <- if (is.na(prev)) x[i] else lambda * x[i] + (1 - lambda) * prev
    out[i] <- prev
  }
  out
}

#' Normale de la joueuse : médiane de x sur les `window` jours PRÉCÉDENTS
#' (jours où ok est vrai), si au moins `min_n` jours sont disponibles.
usual_level <- function(x, ok, window, min_n) {
  ok <- coalesce(ok, FALSE) & !is.na(x)
  vapply(seq_along(x), function(i) {
    if (i == 1) return(NA_real_)
    j <- max(1, i - window):(i - 1)
    v <- x[j][ok[j]]
    if (length(v) < min_n) NA_real_ else stats::median(v)
  }, numeric(1))
}

base_category <- function(rel, cfg = CONFIG) {
  dplyr::case_when(is.na(rel) ~ NA_character_,
                   rel < cfg$base_low ~ "basse",
                   rel > cfg$base_high ~ "haute",
                   TRUE ~ "habituelle")
}

#' Niveau de la base sRPE : charge chronique / normale de la joueuse
compute_base_level <- function(metrics, cfg = CONFIG) {
  metrics |>
    arrange(player, date) |>
    group_by(player) |>
    mutate(chronic_usual = usual_level(chronic_ewma, interpretable, cfg$base_window, cfg$base_min_days),
           base_rel      = safe_div(chronic_ewma, chronic_usual),
           base_level    = base_category(base_rel, cfg)) |>
    ungroup()
}

#' ACWR de la charge externe (GPS), en mètres par jour.
#' Traitement des jours sans fichier GPS, sur la période GPS de la joueuse :
#'   - jour sans GPS ET sans charge déclarée -> 0 m (repos supposé) ;
#'   - jour sans GPS MAIS avec une charge > 0 -> manquant (séance non captée),
#'     la moyenne n'est pas mise à jour ce jour-là.
#' L'ACWR GPS n'est interprétable que si au moins `min_coverage_gps` des jours
#' actifs (charge > 0 ou GPS) des 28 derniers jours ont un fichier GPS.
compute_external_acwr <- function(metrics, cfg = CONFIG) {
  if (!all(c("total_km", "hsr_m", "sprint_m") %in% names(metrics))) return(metrics)
  a <- cfg$acute_window; c <- cfg$chronic_window
  one <- function(v, has_gps, active, in_gps) {
    x <- ifelse(has_gps, v, ifelse(active, NA_real_, 0))
    x[!in_gps] <- NA_real_
    list(acute = ifelse(in_gps, ewma_na(x, a), NA_real_),
         chronic = ifelse(in_gps, ewma_na(x, c), NA_real_))
  }
  metrics |>
    arrange(player, date) |>
    group_by(player) |>
    mutate(
      has_gps = !is.na(total_km),
      in_gps  = if (any(has_gps)) date >= min(date[has_gps]) & date <= max(date[has_gps]) else FALSE,
      active  = load > 0 | has_gps,
      gps_day = cumsum(in_gps),
      gps_coverage_28 = safe_div(roll_sum(as.numeric(has_gps & in_gps), c),
                                 roll_sum(as.numeric(active & in_gps), c)),
      gps_interpretable = in_gps & gps_day >= c & !is.na(gps_coverage_28) &
                          gps_coverage_28 >= cfg$min_coverage_gps,
      hsr_acute     = one(hsr_m, has_gps, active, in_gps)$acute,
      hsr_chronic   = one(hsr_m, has_gps, active, in_gps)$chronic,
      hsr_acwr      = safe_div(hsr_acute, hsr_chronic),
      sprint_acute  = one(sprint_m, has_gps, active, in_gps)$acute,
      sprint_chronic = one(sprint_m, has_gps, active, in_gps)$chronic,
      sprint_acwr   = safe_div(sprint_acute, sprint_chronic),
      hsr_base_rel    = safe_div(hsr_chronic, usual_level(hsr_chronic, gps_interpretable, cfg$base_window, cfg$base_min_days)),
      sprint_base_rel = safe_div(sprint_chronic, usual_level(sprint_chronic, gps_interpretable, cfg$base_window, cfg$base_min_days))
    ) |>
    ungroup() |>
    select(-has_gps, -in_gps, -active, -gps_day)
}

#' Lecture d'une alerte de charge selon le niveau de la base
alert_context <- function(flag_high, flag_low, base_level, base_rel) {
  pc <- ifelse(is.na(base_rel), "", sprintf(" (%d %% de sa normale)", as.integer(floor(100 * base_rel))))
  dplyr::case_when(
    flag_high & base_level == "basse" ~ paste0("hausse rapide depuis une base basse", pc, " : reprise à encadrer"),
    flag_high ~ paste0("hausse rapide sur une base ", coalesce(base_level, "pas encore établie"), pc),
    flag_low & base_level == "basse" ~ paste0("sous-exposition durable", pc),
    flag_low ~ paste0("baisse récente depuis une base ", coalesce(base_level, "pas encore établie"), pc),
    TRUE ~ ""
  )
}

#' Chiffres clés « ratio et valeurs absolues » (README, rapport, tableau de bord)
exposure_summary <- function(metrics, cfg = CONFIG) {
  iv <- metrics |> filter(interpretable, !is.na(acwr_ewma))
  near <- iv |> filter(acwr_ewma >= 0.75, acwr_ewma <= 0.85)
  q <- stats::quantile(near$acute_ewma, c(.1, .5, .9), na.rm = TRUE)
  fh <- iv |> filter(flag_acwr_high); fl <- iv |> filter(flag_acwr_low)
  out <- list(
    n_near_08 = nrow(near),
    acute_near_08 = unname(round(q)),
    high_alert_days = nrow(fh),
    high_alert_low_base = round(mean(fh$base_level == "basse", na.rm = TRUE), 3),
    low_alert_days = nrow(fl),
    low_alert_low_base = round(mean(fl$base_level == "basse", na.rm = TRUE), 3)
  )
  if ("hsr_acwr" %in% names(metrics)) {
    g <- metrics |> filter(interpretable, gps_interpretable, !is.na(acwr_ewma), !is.na(hsr_acwr))
    if (nrow(g) > 30) {
      sp <- g |> filter(hsr_acwr > cfg$acwr_high)
      out$gps_days <- nrow(g)
      out$gps_players <- n_distinct(g$player)
      out$cor_srpe_hsr <- round(stats::cor(g$acwr_ewma, g$hsr_acwr), 2)
      out$hsr_spike_days <- nrow(sp)
      out$hsr_spike_srpe_in_zone <- round(mean(sp$acwr_ewma <= cfg$acwr_high), 3)
      out$hsr_low_days_share <- round(mean(g$hsr_acwr < cfg$acwr_low), 3)
    }
  }
  out
}
