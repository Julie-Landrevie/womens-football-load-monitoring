# =============================================================================
# 11_opportunity.R — Effort relatif à l'opportunité
#
# Normalisation par l'opportunité, à partir de Bozděch (2026, PLOS ONE, doi 10.1371/journal.pone.0359538) :
# une performance se lit selon les occasions de l'exprimer. En sport collectif,
# une partie de ces occasions vient de l'environnement d'entraînement (type de
# séance, densité du groupe) et une autre de la compétition (temps de jeu,
# adversaires). SoccerMon ne permet de traiter que la première : le GPS de
# toutes les joueuses présentes un même jour neutralise le contenu de la
# séance. Il n'y a ni temps de jeu, ni distinction match / entraînement, ni
# adversaire.
#
# Indice d'effort relatif = valeur de la joueuse / médiane des joueuses de
# champ de son équipe présentes ce jour-là (au moins `opp_min_players` avec
# GPS, sinon NA). Le percentile dans l'équipe ce jour-là est aussi calculé : il
# reste défini quand la médiane vaut 0 (fréquent pour le sprint à
# l'entraînement), là où le ratio ne l'est pas.
#
# Les gardiennes probables (vitesse max médiane < `opp_gk_vmax` km/h) sont
# écartées : leur séance n'est pas celle du groupe.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

OPP_METRICS <- c(hi = "hi_w_m", sprint = "sprint_w_m", acc = "n_acc_high_w", total = "total_km")
OPP_LABELS  <- c(hi = "Haute intensité > 19 km/h", sprint = "Sprint > 23 km/h",
                 acc = "Accélérations > 2,7 m/s²", total = "Distance totale")

#' Gardiennes probables : vitesse max médiane trop basse
opp_goalkeepers <- function(gps_daily, cfg = CONFIG) {
  vcol <- if ("vmax_1s_kmh" %in% names(gps_daily)) "vmax_1s_kmh" else "vmax_kmh"
  gps_daily |>
    group_by(player) |>
    summarise(days = n(), vmax_med = median(.data[[vcol]], na.rm = TRUE), .groups = "drop") |>
    filter(days >= 10, vmax_med < cfg$opp_gk_vmax) |>
    pull(player)
}

#' 1. Indice d'effort relatif, jour par jour
opportunity_index <- function(gps_daily, cfg = CONFIG) {
  mets <- OPP_METRICS[OPP_METRICS %in% names(gps_daily)]
  if (!length(mets)) return(NULL)
  gk <- opp_goalkeepers(gps_daily, cfg)
  d <- gps_daily |>
    filter(!player %in% gk) |>
    mutate(team = sub("-.*$", "", player)) |>
    select(team, player, date, all_of(unname(mets)))
  long <- d |>
    pivot_longer(all_of(unname(mets)), names_to = "col", values_to = "value") |>
    mutate(metric = names(mets)[match(col, mets)]) |>
    filter(!is.na(value)) |>
    group_by(team, date, metric) |>
    mutate(n_present = n(),
           team_median = median(value),
           pct = if (n() > 1) (rank(value, ties.method = "average") - 1) / (n() - 1) else NA_real_) |>
    ungroup() |>
    mutate(ok = n_present >= cfg$opp_min_players,
           rel = if_else(ok & team_median > 0, value / team_median, NA_real_),
           pct = if_else(ok, pct, NA_real_))
  # 4. Jours de match (estimation) : le jour le plus chargé de chaque semaine
  #    pour toute l'équipe (médiane de la distance totale). Une estimation,
  #    jamais une donnée : SoccerMon ne distingue pas match et entraînement.
  match_days <- long |>
    filter(metric == "total", ok) |>
    distinct(team, date, team_median) |>
    mutate(week = format(date, "%G-%V")) |>
    group_by(team, week) |>
    slice_max(team_median, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(team, date, match_probable = TRUE)
  long |>
    left_join(match_days, by = c("team", "date")) |>
    mutate(match_probable = coalesce(match_probable, FALSE)) |>
    select(team, player, date, metric, value, n_present, team_median, rel, pct, match_probable) |>
    arrange(team, player, date, metric)
}

#' Corrélation de rang entre deux échantillons de jours disjoints, pour une
#' table joueuse × jour et plusieurs colonnes (même test que 09_thresholds.R)
opp_split_rho <- function(d, cols, n_days, reps = 30, seed = 1) {
  set.seed(seed)
  by_player <- split(d, d$player)
  bind_rows(lapply(n_days, function(n) {
    elig <- Filter(function(g) nrow(g) >= 2 * n, by_player)
    if (length(elig) < 8) return(NULL)
    rho <- vapply(seq_len(reps), function(r) {
      ab <- lapply(elig, function(g) { i <- sample(nrow(g), 2 * n); list(a = g[i[1:n], ], b = g[i[(n + 1):(2 * n)], ]) })
      vapply(cols, function(col) {
        f <- function(part) vapply(ab, function(x) mean(x[[part]][[col]], na.rm = TRUE), numeric(1))
        suppressWarnings(stats::cor(f("a"), f("b"), method = "spearman", use = "complete.obs"))
      }, numeric(1))
    }, numeric(length(cols)))
    rho <- matrix(rho, nrow = length(cols), dimnames = list(cols, NULL))
    tibble(n_days = n, players = length(elig), version = cols, rho = rowMeans(rho, na.rm = TRUE))
  }))
}

#' 2. Stabilité : volume brut, ratio à l'équipe et percentile dans l'équipe.
#' Seuls les jours où l'équipe compte assez de joueuses avec GPS sont gardés,
#' pour comparer les trois versions sur les mêmes jours.
opportunity_stability <- function(idx, n_days = c(1, 2, 3, 5, 8, 12, 16, 20, 30, 40), min_days = 20) {
  if (is.null(idx)) return(NULL)
  bind_rows(lapply(unique(idx$metric), function(mt) {
    d <- idx |> filter(metric == mt, !is.na(pct)) |>
      group_by(player) |> filter(n() >= min_days) |> ungroup() |>
      transmute(player, brut = value, ratio = rel, percentile = pct)
    opp_split_rho(d, c("brut", "ratio", "percentile"), n_days) |> mutate(metric = mt, .before = 1)
  }))
}

#' Joueuses dont le rang change le plus entre volume brut et effort relatif
opportunity_ranks <- function(idx, min_days = 20) {
  if (is.null(idx)) return(NULL)
  idx |>
    filter(!is.na(pct)) |>
    group_by(metric, team, player) |>
    filter(n() >= min_days) |>
    summarise(days = n(), brut = mean(value), percentile = mean(pct),
              ratio = mean(rel, na.rm = TRUE), share_match = mean(match_probable), .groups = "drop") |>
    group_by(metric) |>
    mutate(pct_brut = 100 * (rank(brut) - 1) / (n() - 1),
           pct_relatif = 100 * (rank(percentile) - 1) / (n() - 1),
           shift = pct_relatif - pct_brut) |>
    ungroup() |>
    arrange(metric, desc(abs(shift)))
}

#' 3. Retour au jeu : jours pour « retrouver sa place dans le groupe ». Sur 7
#' jours glissants, indice relatif = somme de la joueuse / somme des médianes de
#' l'équipe les mêmes jours ; jalon atteint quand il dépasse `opp_group_level`
#' (90 %) de son indice relatif des 28 jours avant l'absence. Un seuil absolu
#' (0,9 x la médiane) ne marche pas : hors absence, 39 % des semaines sont sous
#' 0,9 pour la haute intensité et 20 joueuses sur 64 y sont habituellement., à comparer au jalon « revenue
#' à son propre niveau d'avant » de 10_return_criteria.R.
opportunity_return <- function(idx, absences, cfg = CONFIG) {
  if (is.null(idx) || is.null(absences) || nrow(absences) == 0) return(NULL)
  win_n <- cfg$return_milestone_window
  out <- lapply(seq_len(nrow(absences)), function(i) {
    a <- absences[i, ]
    nxt <- absences$absence_start[absences$player == a$player & absences$absence_start > a$return_date]
    end <- min(c(a$return_date + cfg$return_follow_days - 1, if (length(nxt)) min(nxt) - 1))
    x <- idx[idx$player == a$player & idx$date >= a$return_date & idx$date <= end & !is.na(idx$pct), ]
    if (!nrow(x)) return(NULL)
    # place habituelle de la joueuse dans son groupe : les 28 jours avant l'absence
    pre <- idx[idx$player == a$player & !is.na(idx$pct) &
                 idx$date >= a$absence_start - cfg$return_baseline_days & idx$date <= a$absence_start - 1, ]
    x |>
      group_by(metric) |>
      group_modify(function(g, key) {
        p <- pre[pre$metric == key$metric, ]
        base_rel <- if (nrow(p) >= cfg$opp_min_base_days) safe_div(sum(p$value), sum(p$team_median)) else NA_real_
        days <- seq_len(as.integer(end - a$return_date) + 1)
        days <- days[days >= win_n]
        r <- vapply(days, function(t) {
          w <- g[g$date >= a$return_date + t - win_n & g$date <= a$return_date + t - 1, ]
          # ratio des sommes (et non moyenne des ratios, qui explose quand la
          # médiane du jour est proche de 0) ; au moins 2 jours GPS dans la fenêtre
          if (nrow(w) >= 2) safe_div(sum(w$value), sum(w$team_median)) else NA_real_
        }, numeric(1))
        if (!is.na(base_rel) && base_rel <= 0) base_rel <- NA_real_   # pas de place mesurable avant l'absence
        ok <- !is.na(r) & !is.na(base_rel)
        hit <- ok & r >= cfg$opp_group_level * base_rel
        tibble(base_rel = base_rel, days_measured = sum(ok),
               day = if (any(hit)) days[which(hit)[1]] else NA_integer_,
               last_measured = if (any(ok)) max(days[ok]) else NA_integer_)
      }) |>
      ungroup() |>
      mutate(episode = a$episode, type = a$type, label = a$label, .before = 1)
  })
  res <- bind_rows(out)
  if (!nrow(res)) return(NULL)
  res |>
    mutate(status = case_when(days_measured == 0 ~ "non mesurable", !is.na(day) ~ "atteint", TRUE ~ "non atteint"),
           time = if_else(status == "atteint", as.integer(day), as.integer(last_measured)),
           event = status == "atteint")
}

#' Comparaison « niveau du groupe » / « son niveau d'avant » (90 %)
opportunity_return_summary <- function(opp_ret, milestones_table) {
  if (is.null(opp_ret)) return(NULL)
  grp <- opp_ret |> filter(status != "non mesurable") |>
    group_by(type, metric) |>
    summarise(n = n(), reached = sum(event), km_median_day = km_median(time, event), .groups = "drop") |>
    mutate(jalon = "sa place dans le groupe")
  own <- if (is.null(milestones_table)) NULL else milestones_table |>
    filter(level == 0.9, status != "non mesurable", as.character(metric) %in% c("hi", "sprint")) |>
    mutate(metric = as.character(metric)) |>
    group_by(type, metric) |>
    summarise(n = n(), reached = sum(event), km_median_day = km_median(time, event), .groups = "drop") |>
    mutate(jalon = "son niveau d'avant")
  bind_rows(grp, own) |> arrange(type, metric, jalon)
}

#' Tout le module en un appel (utilisé par run_pipeline.R)
compute_opportunity <- function(gps_daily, labels = NULL, absences = NULL, milestones_table = NULL, cfg = CONFIG) {
  if (is.null(gps_daily)) return(NULL)
  idx <- opportunity_index(gps_daily, cfg)
  if (is.null(idx)) return(NULL)
  if (!is.null(labels)) idx <- idx |> left_join(distinct(labels, player, label), by = "player")
  stab <- opportunity_stability(idx)
  rk <- opportunity_ranks(idx)
  if (!is.null(labels) && !is.null(rk)) rk <- rk |> left_join(distinct(labels, player, label), by = "player")
  ret <- opportunity_return(idx, absences, cfg)
  ret_sm <- opportunity_return_summary(ret, milestones_table)
  n_to <- function(mt, v) {
    s <- stab[stab$metric == mt & stab$version == v & !is.na(stab$rho) & stab$rho >= 0.8, ]
    if (nrow(s)) min(s$n_days) else NA_real_
  }
  stats <- list(
    goalkeepers_excluded = length(opp_goalkeepers(gps_daily, cfg)),
    team_days_ok = n_distinct(paste(idx$team, idx$date)[idx$metric == "total" & !is.na(idx$pct)]),
    match_days_estimated = n_distinct(paste(idx$team, idx$date)[idx$match_probable]),
    sprint_days_team_median_zero = round(mean(idx$team_median[idx$metric == "sprint" & !is.na(idx$pct)] == 0), 3)
  )
  for (mt in unique(idx$metric))
    stats[[paste0("days_rho08_", mt)]] <- sprintf("brut %s · ratio %s · percentile %s",
                                                  n_to(mt, "brut"), n_to(mt, "ratio"), n_to(mt, "percentile"))
  list(index = idx, stability = stab, ranks = rk, returns = ret, returns_summary = ret_sm, stats = stats)
}
