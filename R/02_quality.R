# =============================================================================
# 02_quality.R — Contrôle qualité des données
# Objectif : savoir AVANT d'interpréter si la donnée est fiable.
#   1. Complétude par joueuse et par variable
#   2. Valeurs hors bornes plausibles / non numériques
#   3. Doublons de dates
#   4. Cohérence : indicateurs fournis vs recalculés (cf. 03_load_metrics.R)
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

#' Une donnée "réelle" : non manquante, et pour la charge, strictement > 0.
#' (Dans SoccerMon, daily_load vaut 0 les jours sans déclaration : un 0 ne
#' permet pas de distinguer repos et questionnaire non rempli.)
is_real_value <- function(variable, value) {
  !is.na(value) & !(variable == "daily_load" & value <= 0)
}

#' Période active d'une joueuse = du premier au dernier jour avec une donnée réelle
active_period <- function(daily_long) {
  daily_long |>
    filter(is_real_value(variable, value)) |>
    group_by(player) |>
    summarise(first_day = min(date), last_day = max(date), .groups = "drop")
}

#' 1. Complétude (sur la période active de chaque joueuse).
#' Pour daily_load : part des jours avec une charge > 0 (et non "renseignée").
qc_completeness <- function(daily_long, vars = c("daily_load", WELLNESS_VARS)) {
  per <- active_period(daily_long)
  daily_long |>
    filter(variable %in% vars) |>
    inner_join(per, by = "player") |>
    filter(date >= first_day, date <= last_day) |>
    group_by(team, player, label, variable) |>
    summarise(days = n_distinct(date),
              reported = n_distinct(date[is_real_value(variable, value)]),
              completeness = reported / days, .groups = "drop") |>
    mutate(variable = if_else(variable == "daily_load", "daily_load (> 0)", variable))
}

#' Plages de charge nulle consécutive (absence, blessure, coupure ou non-déclaration)
qc_zero_runs <- function(daily_long, min_run = 14) {
  per <- active_period(daily_long)
  daily_long |>
    filter(variable == "daily_load") |>
    inner_join(per, by = "player") |>
    filter(date >= first_day, date <= last_day) |>
    arrange(player, date) |>
    group_by(team, player, label) |>
    summarise(
      zero_share  = mean(is.na(value) | value <= 0),
      longest_run = { r <- rle(is.na(value) | value <= 0); max(c(0, r$lengths[r$values])) },
      runs_ge_min = { r <- rle(is.na(value) | value <= 0); sum(r$values & r$lengths >= min_run) },
      .groups = "drop")
}

#' 2. Valeurs hors bornes et non numériques
qc_out_of_range <- function(daily_long, ranges = CONFIG$valid_ranges) {
  bounds <- tibble(variable = names(ranges),
                   min_ok = map_dbl(ranges, 1), max_ok = map_dbl(ranges, 2))
  daily_long |>
    inner_join(bounds, by = "variable") |>
    filter(non_numeric | (!is.na(value) & (value < min_ok | value > max_ok))) |>
    mutate(issue = if_else(non_numeric, "valeur non numérique", "hors bornes")) |>
    select(team, label, player, date, variable, value, min_ok, max_ok, issue)
}

#' Nettoyage : les valeurs suspectes sont mises à NA (jamais corrigées ni
#' supprimées en silence : elles restent listées dans le rapport qualité)
clean_values <- function(daily_long, out_of_range) {
  daily_long |>
    left_join(out_of_range |> distinct(player, date, variable) |> mutate(.bad = TRUE),
              by = c("player", "date", "variable")) |>
    mutate(value = if_else(coalesce(.bad, FALSE), NA_real_, value)) |>
    select(-.bad)
}

#' 3. Doublons (même joueuse, même date, même variable)
qc_duplicates <- function(daily_long) {
  daily_long |>
    count(player, date, variable, name = "n") |>
    filter(n > 1)
}

#' 4. Cohérence entre ACWR fourni dans SoccerMon et ACWR recalculé
qc_consistency <- function(metrics) {
  if (!"acwr" %in% names(metrics)) return(NULL)
  metrics |>
    filter(!is.na(acwr), !is.na(acwr_ra), is.finite(acwr), is.finite(acwr_ra)) |>
    summarise(n = n(),
              correlation = cor(acwr, acwr_ra),
              mean_abs_diff = mean(abs(acwr - acwr_ra)),
              share_within_0.1 = mean(abs(acwr - acwr_ra) <= 0.1))
}

#' Synthèse en une table lisible
qc_summary <- function(daily_long, completeness, out_of_range, duplicates, zero_runs) {
  per <- active_period(daily_long)
  tibble(
    indicateur = c("Joueuses", "Équipes", "Période des fichiers",
                   "Période réellement active (toutes joueuses)",
                   "Part médiane de jours avec charge > 0",
                   "Complétude médiane wellness",
                   "Joueuses avec ≥ 1 plage de 14 j+ sans charge",
                   "Valeurs hors bornes / non numériques",
                   "Doublons de dates"),
    valeur = c(
      n_distinct(daily_long$player),
      n_distinct(daily_long$team),
      paste(format(min(daily_long$date, na.rm = TRUE), "%d/%m/%Y"), "→",
            format(max(daily_long$date, na.rm = TRUE), "%d/%m/%Y")),
      paste(format(min(per$first_day), "%d/%m/%Y"), "→", format(max(per$last_day), "%d/%m/%Y")),
      scales::percent(median(completeness$completeness[completeness$variable == "daily_load (> 0)"]), 1),
      scales::percent(median(completeness$completeness[completeness$variable %in% WELLNESS_VARS]), 1),
      sum(zero_runs$runs_ge_min > 0),
      nrow(out_of_range),
      nrow(duplicates)
    ) |> as.character()
  )
}
