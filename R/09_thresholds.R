# =============================================================================
# 09_thresholds.R — Seuils masculins ou féminins : ce que change le choix
#
# Source des seuils féminins : SkillCorner, webinaire public « Women's
# Football: Dedicated Data, Smarter Decisions » (12/10/2026), présentation
# transmise aux inscrits. Sprint > 23 km/h (contre > 25 km/h chez les hommes),
# accélération haute > 2,7 m/s² (contre > 3 m/s²).
#
# SkillCorner a mesuré l'effet de ces seuils en match (tracking vidéo). On
# refait les trois mêmes vérifications sur SoccerMon (GPS, entraînements et
# matchs de deux équipes de Toppserien, l'une des ligues couvertes) :
#   1. couverture : part des jours où la joueuse dépasse le seuil au moins une
#      fois (avec des seuils trop hauts, beaucoup de journées valent 0) ;
#   2. stabilité : combien de jours faut-il pour classer les joueuses de façon
#      reproductible (corrélation entre deux échantillons de jours disjoints) ;
#   3. profils cachés : joueuses mal classées avec les seuils masculins et bien
#      classées avec les seuils féminins.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

THRESHOLD_PAIRS <- list(
  sprint_dist = list(label = "Distance de sprint (m)",  men = "sprint_men_m",   women = "sprint_w_m",
                     men_lab = "> 25 km/h", women_lab = "> 23 km/h"),
  sprints     = list(label = "Sprints d'au moins 1 s", men = "n_sprint_men",   women = "n_sprint_w",
                     men_lab = "> 25 km/h", women_lab = "> 23 km/h"),
  acc_high    = list(label = "Accélérations hautes",   men = "n_acc_high_men", women = "n_acc_high_w",
                     men_lab = "> 3 m/s²",  women_lab = "> 2,7 m/s²"),
  dec_high    = list(label = "Décélérations hautes",   men = "n_dec_high_men", women = "n_dec_high_w",
                     men_lab = "< -3 m/s²", women_lab = "< -2,7 m/s²")
)

available_pairs <- function(d) Filter(function(p) all(c(p$men, p$women) %in% names(d)), THRESHOLD_PAIRS)

#' 1. Couverture : part des jours au-dessus de 0, médianes
threshold_coverage <- function(d) {
  pr <- available_pairs(d)
  if (!length(pr)) return(NULL)
  bind_rows(lapply(names(pr), function(k) {
    p <- pr[[k]]; m <- d[[p$men]]; w <- d[[p$women]]
    ok <- !is.na(m) & !is.na(w)
    tibble(key = k, metric = p$label, men_lab = p$men_lab, women_lab = p$women_lab, days = sum(ok),
           cover_men = mean(m[ok] > 0), cover_women = mean(w[ok] > 0),
           median_men = median(m[ok]), median_women = median(w[ok]))
  }))
}

#' Courbes de couverture : part des jours où la valeur dépasse x
threshold_curves <- function(d, n_points = 40) {
  pr <- available_pairs(d)
  if (!length(pr)) return(NULL)
  bind_rows(lapply(names(pr), function(k) {
    p <- pr[[k]]; m <- d[[p$men]]; w <- d[[p$women]]
    ok <- !is.na(m) & !is.na(w); m <- m[ok]; w <- w[ok]
    xs <- unique(round(seq(0, stats::quantile(w, 0.99), length.out = n_points), 1))
    bind_rows(tibble(key = k, metric = p$label, set = paste("Seuil masculin", p$men_lab), x = xs,
                     share = vapply(xs, function(x) mean(m > x), numeric(1))),
              tibble(key = k, metric = p$label, set = paste("Seuil féminin", p$women_lab), x = xs,
                     share = vapply(xs, function(x) mean(w > x), numeric(1))))
  }))
}

#' 2. Stabilité du classement des joueuses selon le nombre de jours
threshold_stability <- function(d, n_days = c(1, 2, 3, 5, 8, 12, 16, 20, 30, 40), reps = 30, seed = 1) {
  pr <- available_pairs(d)
  if (!length(pr)) return(NULL)
  set.seed(seed)
  by_player <- split(d, d$player)
  bind_rows(lapply(names(pr), function(k) {
    p <- pr[[k]]
    bind_rows(lapply(n_days, function(n) {
      elig <- Filter(function(g) nrow(g) >= 2 * n, by_player)
      if (length(elig) < 8) return(NULL)
      rho <- vapply(seq_len(reps), function(r) {
        ab <- lapply(elig, function(g) { i <- sample(nrow(g), 2 * n); list(a = g[i[1:n], ], b = g[i[(n + 1):(2 * n)], ]) })
        f <- function(col, part) vapply(ab, function(x) mean(x[[part]][[col]], na.rm = TRUE), numeric(1))
        c(men = suppressWarnings(stats::cor(f(p$men, "a"), f(p$men, "b"), method = "spearman")),
          women = suppressWarnings(stats::cor(f(p$women, "a"), f(p$women, "b"), method = "spearman")))
      }, numeric(2))
      tibble(key = k, metric = p$label, n_days = n, players = length(elig),
             rho_men = mean(rho["men", ], na.rm = TRUE), rho_women = mean(rho["women", ], na.rm = TRUE))
    }))
  }))
}

#' 3. Profils cachés : rang (percentile) de chaque joueuse avec les deux seuils
threshold_ranks <- function(d, labels, min_days = 20) {
  pr <- available_pairs(d)
  if (!length(pr)) return(NULL)
  bind_rows(lapply(names(pr), function(k) {
    p <- pr[[k]]
    d |>
      group_by(player) |>
      filter(n() >= min_days) |>
      summarise(men = mean(.data[[p$men]], na.rm = TRUE), women = mean(.data[[p$women]], na.rm = TRUE),
                days = n(), .groups = "drop") |>
      mutate(key = k, metric = p$label,
             pct_men = 100 * (rank(men, ties.method = "average") - 1) / (n() - 1),
             pct_women = 100 * (rank(women, ties.method = "average") - 1) / (n() - 1),
             shift = pct_women - pct_men)
  })) |>
    left_join(labels, by = "player")
}

#' Tout le module en un appel
compute_thresholds <- function(gps_daily, labels, cfg = CONFIG) {
  if (is.null(gps_daily) || !length(available_pairs(gps_daily))) return(NULL)
  d <- gps_daily |> semi_join(labels, by = "player")   # joueuses suivies dans la partie subjective
  cov <- threshold_coverage(d)
  rk <- threshold_ranks(d, labels)
  stab <- threshold_stability(d)
  hid <- rk |> group_by(key, metric) |>
    summarise(players = n(), spearman = suppressWarnings(stats::cor(men, women, method = "spearman")),
              moved_20 = sum(abs(shift) >= 20), hidden = sum(pct_men <= 25 & pct_women >= 50), .groups = "drop")
  n_to <- function(k, col) {
    s <- stab[stab$key == k & !is.na(stab[[col]]) & stab[[col]] >= 0.8, ]
    if (nrow(s)) min(s$n_days) else NA_real_
  }
  # corrélation hommes / femmes au nombre de jours le plus proche de n
  rho_at <- function(k, n) {
    s <- stab[stab$key == k, ]
    if (!nrow(s)) return(NA)
    s <- s[which.min(abs(s$n_days - n)), ]
    sprintf("%d j : %.2f (seuil masculin) / %.2f (féminin)", s$n_days, s$rho_men, s$rho_women)
  }
  g <- function(k, col) { x <- cov[[col]][cov$key == k]; if (length(x)) round(x, 3) else NA_real_ }
  stats <- list(
    days = if (is.null(cov)) 0L else max(cov$days),
    sprint_cover_men = g("sprint_dist", "cover_men"), sprint_cover_women = g("sprint_dist", "cover_women"),
    sprint_median_men = g("sprint_dist", "median_men"), sprint_median_women = g("sprint_dist", "median_women"),
    acc_cover_men = g("acc_high", "cover_men"), acc_cover_women = g("acc_high", "cover_women"),
    acc_median_men = g("acc_high", "median_men"), acc_median_women = g("acc_high", "median_women"),
    stab_days_sprint_men = n_to("sprint_dist", "rho_men"), stab_days_sprint_women = n_to("sprint_dist", "rho_women"),
    stab_days_acc_men = n_to("acc_high", "rho_men"), stab_days_acc_women = n_to("acc_high", "rho_women"),
    stab_rho10_sprint = rho_at("sprint_dist", 10), stab_rho20_sprint = rho_at("sprint_dist", 20),
    stab_rho10_acc = rho_at("acc_high", 10), stab_rho20_acc = rho_at("acc_high", 20),
    hidden_sprint = if (is.null(hid)) NA else hid$hidden[hid$key == "sprint_dist"][1],
    hidden_acc = if (is.null(hid)) NA else hid$hidden[hid$key == "acc_high"][1],
    players_ranked = if (is.null(hid)) NA else hid$players[hid$key == "sprint_dist"][1],
    spearman_acc = if (is.null(hid)) NA else round(hid$spearman[hid$key == "acc_high"][1], 2)
  )
  list(coverage = cov, curves = threshold_curves(d), stability = stab, ranks = rk, hidden = hid, stats = stats)
}
