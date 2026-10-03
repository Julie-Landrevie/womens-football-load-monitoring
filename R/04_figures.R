# =============================================================================
# 04_figures.R — Visualisations (ggplot2)
# Une couleur = un rôle : bleu = charge aiguë / magnitude, orange = charge
# chronique, gris = zones de référence, rouge = point d'attention (toujours
# accompagné d'un libellé, jamais la couleur seule).
# =============================================================================

suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(tidyr) })

COL <- list(
  blue = "#2a78d6", blue_light = "#9ec5f4", orange = "#eb6834",
  ink = "#0b0b0b", ink2 = "#52514e", grid = "#e6e5e1", zone = "#f0efec",
  alert = "#d03b3b",
  seq = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
)

theme_perf <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title = element_text(face = "bold", colour = COL$ink, size = base + 2),
      plot.subtitle = element_text(colour = COL$ink2, margin = margin(b = 8)),
      plot.caption = element_text(colour = COL$ink2, size = base - 2, hjust = 0),
      axis.text = element_text(colour = COL$ink2),
      axis.title = element_text(colour = COL$ink2),
      panel.grid.major = element_line(colour = COL$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", colour = COL$ink, hjust = 0),
      legend.position = "top", legend.justification = "left",
      plot.title.position = "plot", plot.caption.position = "plot"
    )
}

short_id <- function(p) sub("^(Team[A-Za-z])-?(.{0,6}).*$", "\\1-\\2", p)

# 1. Qualité : complétude par joueuse et variable -----------------------------
fig_qc_completeness <- function(completeness) {
  completeness |>
    mutate(player = short_id(player),
           player = reorder(player, completeness)) |>
    ggplot(aes(variable, player, fill = completeness)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    scale_fill_gradientn(colours = COL$seq, limits = c(0, 1),
                         labels = scales::percent, name = "Jours renseignés") +
    facet_grid(team ~ ., scales = "free_y", space = "free_y") +
    labs(title = "Complétude des déclarations",
         subtitle = "Part des jours renseignés sur la période active de chaque joueuse",
         x = NULL, y = NULL) +
    theme_perf(9) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid = element_blank(), legend.key.width = unit(1.4, "cm"))
}

# 2. Vue équipe : charge hebdomadaire par joueuse ------------------------------
fig_team_weekly <- function(weekly, team_name) {
  d <- weekly |> filter(team == team_name) |>
    mutate(player = short_id(player),
           weekly_load = ifelse(days_reported == 0, NA, weekly_load))
  ggplot(d, aes(week, reorder(player, weekly_load, FUN = function(x) mean(x, na.rm = TRUE)),
                fill = weekly_load)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_gradientn(colours = COL$seq, na.value = "#f7f7f5",
                         labels = scales::label_number(big.mark = " "),
                         name = "Charge hebdo (UA)") +
    scale_x_date(date_labels = "%b %y", expand = c(0, 0)) +
    labs(title = paste("Charge hebdomadaire —", team_name),
         subtitle = "sRPE cumulée par semaine ; case claire = aucune déclaration",
         x = NULL, y = NULL) +
    theme_perf(9) +
    theme(panel.grid = element_blank(), legend.key.width = unit(1.4, "cm"))
}

# 3. Vue joueuse : charge et ACWR (deux panneaux, un axe chacun) ---------------
fig_player_load <- function(metrics, player_id, from = NULL, to = NULL, cfg = CONFIG) {
  d <- metrics |> filter(player == player_id)
  if (!is.null(from)) d <- d |> filter(date >= as.Date(from))
  if (!is.null(to))   d <- d |> filter(date <= as.Date(to))

  p_load <- "Charge (UA)"; p_acwr <- "ACWR (EWMA 7 j / 28 j)"
  lines <- d |> select(date, `Aiguë (EWMA 7 j)` = acute_ewma,
                       `Chronique (EWMA 28 j)` = chronic_ewma) |>
    pivot_longer(-date, names_to = "serie") |> mutate(panel = p_load)
  bars  <- d |> filter(load > 0) |> mutate(panel = p_load)
  acwr  <- d |> mutate(panel = p_acwr,
                       acwr_plot = ifelse(interpretable, acwr_ewma, NA))
  zone  <- tibble(panel = p_acwr, xmin = min(d$date), xmax = max(d$date),
                  ymin = cfg$acwr_low, ymax = cfg$acwr_high)
  flags <- acwr |> filter(flag_acwr_high | flag_acwr_low)

  ggplot() +
    geom_rect(data = zone, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = COL$zone) +
    geom_col(data = bars, aes(date, load), fill = COL$blue_light, width = 0.8) +
    geom_line(data = lines, aes(date, value, colour = serie), linewidth = 0.8) +
    geom_line(data = acwr, aes(date, acwr_plot), colour = COL$ink2, linewidth = 0.6,
              na.rm = TRUE) +
    geom_point(data = flags, aes(date, acwr_ewma), colour = COL$alert, size = 2.2) +
    facet_grid(factor(panel, levels = c(p_load, p_acwr)) ~ ., scales = "free_y",
               switch = "y") +
    scale_colour_manual(values = c(`Aiguë (EWMA 7 j)` = COL$blue,
                                   `Chronique (EWMA 28 j)` = COL$orange), name = NULL) +
    scale_x_date(date_labels = "%d %b %y") +
    labs(title = paste("Suivi de charge —", short_id(player_id)),
         subtitle = sprintf(paste0("Barres : charge du jour (sRPE). Zone grise : ACWR %.1f–%.1f.\n",
                                   "Points rouges : ACWR hors zone alors que l'équipe y reste (couverture ≥ %d %%)."),
                            cfg$acwr_low, cfg$acwr_high, round(100 * cfg$min_coverage_chronic)),
         x = NULL, y = NULL,
         caption = "L'ACWR est un repère de progressivité de charge, pas un prédicteur de blessure.") +
    theme_perf() +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90))
}

# 4. Vue joueuse : wellness individuel ---------------------------------------
fig_player_wellness <- function(metrics, player_id, injuries = NULL,
                                from = NULL, to = NULL, cfg = CONFIG) {
  d <- metrics |> filter(player == player_id)
  if (!is.null(from)) d <- d |> filter(date >= as.Date(from))
  if (!is.null(to))   d <- d |> filter(date <= as.Date(to))
  inj <- if (!is.null(injuries)) injuries |> filter(player == player_id,
                                                    date >= min(d$date), date <= max(d$date)) else NULL
  p <- ggplot(d, aes(date, wellness_z)) +
    geom_hline(yintercept = 0, colour = COL$ink2, linewidth = 0.3) +
    geom_hline(yintercept = cfg$wellness_z_alert, colour = COL$alert,
               linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = min(d$date), y = cfg$wellness_z_alert, vjust = -0.5, hjust = 0,
             label = "seuil d'attention", colour = COL$alert, size = 3) +
    geom_line(colour = COL$blue, linewidth = 0.7, na.rm = TRUE) +
    geom_point(data = filter(d, flag_wellness), colour = COL$alert, size = 2) +
    scale_x_date(date_labels = "%d %b %y") +
    labs(title = paste("Wellness individuel —", short_id(player_id)),
         subtitle = "Score composite (fatigue, humeur, disponibilité, sommeil, douleurs, stress)\nen écarts-types par rapport aux 28 jours précédents de la joueuse ; < 0 = moins bien que d'habitude",
         x = NULL, y = "z-score composite") +
    theme_perf()
  if (!is.null(inj) && nrow(inj) > 0) {
    p <- p + geom_vline(data = inj, aes(xintercept = date), colour = COL$ink2,
                        linetype = "dotted") +
      labs(caption = "Lignes pointillées verticales : blessures auto-déclarées.")
  }
  p
}

# 5. Exploratoire : profil moyen avant une blessure déclarée -----------------
fig_injury_profile <- function(inj_win) {
  if (is.null(inj_win) || nrow(inj_win) == 0) return(NULL)
  s <- inj_win |>
    pivot_longer(c(acwr_ewma, wellness_z), names_to = "indic") |>
    group_by(indic, days_to_injury) |>
    summarise(med = median(value, na.rm = TRUE),
              q1 = quantile(value, .25, na.rm = TRUE),
              q3 = quantile(value, .75, na.rm = TRUE),
              n = sum(!is.na(value)), .groups = "drop") |>
    mutate(indic = recode(indic, acwr_ewma = "ACWR (EWMA)",
                          wellness_z = "Wellness (z-score)"))
  ggplot(s, aes(days_to_injury, med)) +
    geom_ribbon(aes(ymin = q1, ymax = q3), fill = COL$blue_light, alpha = 0.6) +
    geom_line(colour = COL$blue, linewidth = 0.8) +
    geom_vline(xintercept = 0, colour = COL$ink2, linetype = "dotted") +
    facet_wrap(~ indic, scales = "free_y", ncol = 2) +
    labs(title = "Profil moyen dans les 21 jours précédant une blessure déclarée",
         subtitle = sprintf("Médiane et intervalle interquartile — %d épisodes",
                            n_distinct(inj_win$injury_id)),
         x = "Jours avant la déclaration", y = NULL,
         caption = "Analyse exploratoire : blessures auto-déclarées, sans groupe témoin. Ne permet aucune conclusion causale.") +
    theme_perf()
}
