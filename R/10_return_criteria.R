# =============================================================================
# 10_return_criteria.R — Retour au jeu par critères : jalons au jour près
#
# Le consensus UEFA sur le LCA dans le football féminin (partie 2, Verhagen et
# al., Science and Medicine in Football, 2026) recommande une reprise fondée sur
# des critères et non sur un calendrier, avec une réexposition progressive aux
# exigences du jeu. 07_return.R lit la reprise par le temps (« semaine 1, 2,
# 3... ») ; ce module la lit par jalons : combien de jours faut-il à chaque
# joueuse pour retrouver 90 % puis 100 % de sa propre référence ?
#
# Pour chaque reprise et chaque indicateur, on calcule chaque jour la valeur
# des 7 derniers jours (fenêtre glissante) rapportée à la référence d'avant
# l'absence. Mêmes références et mêmes règles que 07_return.R :
#   - charge perçue (sRPE) : charge des 7 jours / charge moyenne sur 7 jours
#     des 28 jours avant l'absence ;
#   - haute intensité (> 19 km/h) et sprint (> 23 km/h), seuils féminins :
#     moyenne par jour GPS x jours actifs, si la couverture GPS suffit ;
#   - vitesse maximale (maintenue 1 s) : maximum des 7 jours / médiane des
#     maxima hebdomadaires de référence (jamais le maximum de 4 semaines),
#     seulement si la fenêtre compte au moins `return_vmax_min_sessions`
#     séances GPS ; option `return_vmax_ref = "top3"` (moyenne des 3 meilleures
#     séances, comme SkillCorner), moins adaptée à une fenêtre de 7 jours.
#
# Convention : J1 = jour de la reprise. Une fenêtre complète de 7 jours
# n'existe qu'à partir de J7 : c'est le jalon le plus précoce possible.
# Statuts : « atteint » (jour du jalon), « non atteint » (censuré au dernier
# jour suivi : fin de suivi, nouvelle absence ou fin des données) et
# « non mesurable » (référence absente ou nulle, GPS insuffisant).
#
# Ces jalons de charge ne remplacent pas les critères cliniques (force,
# tests de saut, préparation psychologique) : ils les complètent.
# SoccerMon ne donne ni cause ni diagnostic : aucune conclusion sur le LCA.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

MILESTONE_LABELS <- c(srpe = "sRPE", hi = "HI", sprint = "Sprint", vmax = "Vmax")

#' Ratios quotidiens (fenêtre glissante de 7 jours) après chaque reprise
milestone_daily <- function(metrics, absences, cfg = CONFIG) {
  if (is.null(absences) || nrow(absences) == 0) return(NULL)
  has_gps <- all(c("total_km", "gps_hi_m", "gps_sprint_m", "gps_vmax") %in% names(metrics))
  base_n <- cfg$return_baseline_days
  win_n  <- cfg$return_milestone_window
  out <- vector("list", nrow(absences))
  for (i in seq_len(nrow(absences))) {
    a <- absences[i, ]
    g <- metrics[metrics$player == a$player, ]
    g <- g[order(g$date), ]
    b <- g[g$date >= a$absence_start - base_n & g$date <= a$absence_start - 1, ]
    # même filtre que return_curves() : on retrouve les mêmes reprises
    if (nrow(b) < 0.75 * base_n || sum(b$load) <= 0) next
    srpe_b <- sum(b$load) / nrow(b) * win_n
    b_gps <- if (has_gps) b[!is.na(b$total_km), ] else b[0, ]
    b_active <- sum(b$load > 0 | (if (has_gps) !is.na(b$total_km) else FALSE))
    gps_ok_b <- has_gps && b_active > 0 && nrow(b_gps) / b_active >= cfg$return_min_gps_coverage
    hi_b <- if (gps_ok_b) mean(b_gps$gps_hi_m, na.rm = TRUE) * b_active / nrow(b) * win_n else NA_real_
    sp_b <- if (gps_ok_b) mean(b_gps$gps_sprint_m, na.rm = TRUE) * b_active / nrow(b) * win_n else NA_real_
    vmax_b <- NA_real_
    if (gps_ok_b && cfg$return_vmax_ref == "top3") {
      v <- sort(b_gps$gps_vmax[!is.na(b_gps$gps_vmax)], decreasing = TRUE)
      if (length(v) >= 3) vmax_b <- mean(v[1:3])
    } else if (gps_ok_b) {
      wk <- vapply(seq_len(base_n %/% 7), function(j) {
        d0 <- a$absence_start - base_n + 7 * (j - 1)
        v <- b_gps$gps_vmax[b_gps$date >= d0 & b_gps$date <= d0 + 6]
        v <- v[!is.na(v)]
        if (length(v)) max(v) else NA_real_
      }, numeric(1))
      if (sum(!is.na(wk)) >= 2) vmax_b <- stats::median(wk, na.rm = TRUE)
    }
    # fin du suivi : durée maximale, nouvelle absence de la joueuse ou fin des données
    nxt <- absences$absence_start[absences$player == a$player & absences$absence_start > a$return_date]
    end <- min(c(a$return_date + cfg$return_follow_days - 1,
                 if (length(nxt)) min(nxt) - 1, max(g$date)))
    followed <- as.integer(end - a$return_date) + 1
    if (followed < win_n) next
    rows <- lapply(win_n:followed, function(t) {
      w <- g[g$date >= a$return_date + t - win_n & g$date <= a$return_date + t - 1, ]
      if (nrow(w) < win_n) return(NULL)
      w_gps <- if (has_gps) w[!is.na(w$total_km), ] else w[0, ]
      w_active <- sum(w$load > 0 | (if (has_gps) !is.na(w$total_km) else FALSE))
      gps_ok_w <- gps_ok_b && w_active > 0 && nrow(w_gps) / w_active >= cfg$return_min_gps_coverage
      vm <- if (has_gps) w_gps$gps_vmax[!is.na(w_gps$gps_vmax)] else numeric(0)
      tibble(day = t,
             srpe   = safe_div(sum(w$load), srpe_b),
             hi     = if (gps_ok_w) safe_div(mean(w_gps$gps_hi_m, na.rm = TRUE) * w_active, hi_b) else NA_real_,
             sprint = if (gps_ok_w) safe_div(mean(w_gps$gps_sprint_m, na.rm = TRUE) * w_active, sp_b) else NA_real_,
             vmax   = if (length(vm) >= cfg$return_vmax_min_sessions) safe_div(max(vm), vmax_b) else NA_real_)
    })
    rows <- bind_rows(rows)
    if (nrow(rows) == 0) next
    out[[i]] <- bind_cols(
      a[rep(1, nrow(rows)), c("team", "player", "label", "episode", "type", "absence_days", "return_date")],
      tibble(days_followed = followed,
             base_srpe = srpe_b, base_hi = hi_b, base_sprint = sp_b, base_vmax = vmax_b),
      rows)
  }
  res <- bind_rows(out)
  if (nrow(res) == 0) NULL else res
}

#' Jalons par reprise, indicateur et niveau (90 %, 100 %)
milestone_table <- function(daily, cfg = CONFIG) {
  if (is.null(daily)) return(NULL)
  long <- daily |>
    pivot_longer(names(MILESTONE_LABELS), names_to = "metric", values_to = "ratio")
  lv <- tibble(level = cfg$return_milestone_levels)
  long |>
    group_by(team, player, label, episode, type, absence_days, return_date, days_followed, metric) |>
    group_modify(function(d, key) {
      ok <- !is.na(d$ratio) & is.finite(d$ratio)
      lv |> rowwise() |> mutate(
        days_measured = sum(ok),
        day = if (any(ok & d$ratio >= level)) min(d$day[ok & d$ratio >= level]) else NA_integer_,
        last_measured = if (any(ok)) max(d$day[ok]) else NA_integer_
      ) |> ungroup()
    }) |>
    ungroup() |>
    mutate(status = case_when(days_measured == 0 ~ "non mesurable",
                              !is.na(day) ~ "atteint",
                              TRUE ~ "non atteint"),
           # temps observé pour l'analyse de survie : jalon ou censure
           time = if_else(status == "atteint", as.integer(day), as.integer(last_measured)),
           event = status == "atteint",
           metric = factor(metric, levels = names(MILESTONE_LABELS))) |>
    arrange(label, return_date, metric, level)
}

#' Médiane de Kaplan-Meier : tient compte des jalons non atteints (censure),
#' sinon la médiane des seules reprises « atteintes » serait trop optimiste.
km_median <- function(time, event) {
  ok <- !is.na(time)
  time <- time[ok]; event <- event[ok]
  if (!length(time) || !any(event)) return(NA_real_)
  s <- 1
  for (t in sort(unique(time[event]))) {
    s <- s * (1 - sum(time == t & event) / sum(time >= t))
    if (s <= 0.5) return(t)
  }
  NA_real_
}

#' Résumé par type d'absence, indicateur et niveau
milestone_summary <- function(tab) {
  if (is.null(tab)) return(NULL)
  tab |>
    filter(status != "non mesurable") |>
    group_by(type, metric, level) |>
    summarise(n = n(),
              reached = sum(event),
              share_not_reached = mean(!event),
              km_median_day = km_median(time, event),
              median_day_reached = if (any(event)) median(day, na.rm = TRUE) else NA_real_,
              q1_day_reached = if (any(event)) quantile(day, .25, na.rm = TRUE, names = FALSE) else NA_real_,
              q3_day_reached = if (any(event)) quantile(day, .75, na.rm = TRUE, names = FALSE) else NA_real_,
              .groups = "drop") |>
    arrange(type, level, metric)
}

#' Ordre des jalons dans une même reprise : la charge perçue revient-elle avant
#' la haute intensité, puis le sprint et la vitesse max ?
milestone_order <- function(tab, level = 0.9) {
  if (is.null(tab)) return(NULL)
  pairs <- list(c("srpe", "hi"), c("hi", "sprint"), c("sprint", "vmax"), c("srpe", "sprint"), c("srpe", "vmax"))
  x <- tab |> filter(level == !!level, status != "non mesurable") |>
    mutate(metric = as.character(metric)) |>
    select(type, episode, metric, day, time, event)
  bind_rows(lapply(pairs, function(p) {
    inner_join(x |> filter(metric == p[1]), x |> filter(metric == p[2]),
               by = c("type", "episode"), suffix = c("_a", "_b")) |>
      mutate(order = case_when(
        event_a & event_b & day_a < day_b ~ "a_avant",
        event_a & event_b & day_a == day_b ~ "meme_jour",
        event_a & event_b ~ "b_avant",
        event_a & !event_b ~ "a_seul",
        !event_a & event_b ~ "b_seul",
        TRUE ~ "aucun")) |>
      group_by(type) |>
      summarise(a = p[1], b = p[2], n = n(),
                a_first = sum(order %in% c("a_avant", "a_seul")),
                same_day = sum(order == "meme_jour"),
                b_first = sum(order %in% c("b_avant", "b_seul")),
                neither = sum(order == "aucun"),
                median_gap_days = if (any(order %in% c("a_avant", "meme_jour", "b_avant")))
                  median((day_b - day_a)[order %in% c("a_avant", "meme_jour", "b_avant")]) else NA_real_,
                .groups = "drop")
  }))
}

#' Ligne de jalons lisible par le staff, une par reprise :
#' « sRPE 90 % : J12 · HI 90 % : J18 · Sprint 90 % : non atteint (J56) · Vmax 90 % : — »
milestone_lines <- function(tab, level = 0.9) {
  if (is.null(tab)) return(NULL)
  tab |>
    filter(level == !!level) |>
    mutate(txt = paste0(MILESTONE_LABELS[as.character(metric)], " ", round(100 * level), " % : ",
                        case_when(status == "atteint" ~ paste0("J", day),
                                  status == "non atteint" ~ paste0("non atteint (J", time, ")"),
                                  TRUE ~ "—"))) |>
    group_by(label, return_date, episode, type) |>
    summarise(jalons = paste(txt, collapse = " · "), .groups = "drop")
}

#' Tout le module en un appel (utilisé par run_pipeline.R)
compute_milestones <- function(metrics, absences, cfg = CONFIG) {
  daily <- milestone_daily(metrics, absences, cfg)
  if (is.null(daily)) return(NULL)
  tab <- milestone_table(daily, cfg)
  sm  <- milestone_summary(tab)
  ord <- milestone_order(tab, 0.9)
  ind <- sm |> filter(type == "individuelle", level == 0.9)
  stat <- function(mt, col) { v <- ind[[col]][ind$metric == mt]; if (length(v)) v[1] else NA }
  stats <- list(reprises_individual = n_distinct(tab$episode[tab$type == "individuelle"]),
                reprises_collective = n_distinct(tab$episode[tab$type == "collective"]))
  for (mt in names(MILESTONE_LABELS)) {
    stats[[paste0(mt, "_90_n_individual")]] <- paste0(stat(mt, "reached"), " atteints / ", stat(mt, "n"))
    stats[[paste0(mt, "_90_km_median_day_individual")]] <- stat(mt, "km_median_day")
    stats[[paste0(mt, "_90_median_day_reached_individual")]] <- stat(mt, "median_day_reached")
  }
  list(daily = daily, table = tab, summary = sm, order = ord,
       lines = milestone_lines(tab, 0.9), stats = stats)
}

# Figure : l'escalier des jalons -------------------------------------------------
# Jours (médiane de Kaplan-Meier) pour retrouver 90 % de la référence d'avant
# l'absence, après une absence individuelle. Carré 1 200 x 1 200 px.
fig_return_milestones <- function(summary, type = "individuelle", level = 0.9, min_n = 5) {
  if (is.null(summary)) return(NULL)
  d <- summary |> filter(type == !!type, level == !!level, n >= min_n)
  if (nrow(d) == 0) return(NULL)
  lab <- c(srpe = "Charge perçue\n(sRPE)", hi = "Haute intensité\n> 19 km/h",
           sprint = "Sprint\n> 23 km/h", vmax = "Vitesse max\n(1 s)")
  d <- d |> mutate(name = factor(lab[as.character(metric)], levels = lab),
                   y = coalesce(km_median_day, 0),
                   gps = metric != "srpe",
                   txt = if_else(is.na(km_median_day), "médiane\nnon atteinte",
                                 paste0("J", km_median_day)),
                   sub = paste0("n = ", n, "\n", reached, " atteint", if_else(reached > 1, "s", ""),
                                ", ", n - reached, " non"))
  ymax <- max(d$y, 14, na.rm = TRUE) * 1.25
  ggplot(d, aes(name, y)) +
    geom_col(aes(fill = gps), width = 0.98) +
    geom_text(aes(label = txt), vjust = -0.4, fontface = "bold", size = 7, colour = COL$ink) +
    geom_text(aes(y = 0, label = sub), vjust = 1.3, size = 3.6, colour = COL$ink2, lineheight = 0.9) +
    scale_fill_manual(values = c(`FALSE` = COL$orange, `TRUE` = COL$blue), guide = "none") +
    scale_y_continuous(limits = c(-ymax * 0.12, ymax), breaks = seq(0, 56, 7),
                       labels = function(x) paste0("J", x), expand = c(0, 0)) +
    labs(title = paste0("Jours pour retrouver ", round(100 * level), " % du niveau d'avant l'absence"),
         subtitle = paste0("Après une absence ", type, " d'au moins 14 jours\nSoccerMon, football féminin d'élite"),
         x = NULL, y = NULL, tag = "Julie Landrevie",
         caption = paste0("Médiane de Kaplan-Meier : les jalons non atteints sont pris en compte.\n",
                          "Fenêtre glissante de 7 jours : J7 au plus tôt.\n",
                          "Orange = charge perçue, bleu = GPS. Ni cause ni diagnostic.")) +
    theme_perf(14) +
    theme(panel.grid.major.x = element_blank(),
          plot.tag = element_text(size = 11, colour = COL$ink2, face = "bold"),
          plot.tag.position = c(0.88, 0.03), plot.margin = margin(16, 16, 16, 16),
          axis.text.x = element_text(size = 13, colour = COL$ink, face = "bold"))
}
