# =============================================================================
# 12_srpe_gps.R — Charge perçue et GPS : seuils masculins ou féminins ?
#
# Question : à modèle identique, les seuils féminins (sprint > 23 km/h,
# accélération haute > 2,7 m/s², SkillCorner) expliquent-ils mieux la charge
# perçue (sRPE) que les seuils masculins (25 km/h, 3 m/s²) ?
#
# Point de départ : un mémoire de l'université de Tromsø (« Developing a Linear
# Mixed Model to Predict RPE and sRPE in Female Elite Football Players Using
# External Load Measures »), sur deux clubs norvégiens 2020-2021, très
# probablement les mêmes données : R² = 0,66 pour la sRPE avec un modèle mixte.
#
# Méthode : modèle linéaire mixte, intercept aléatoire par joueuse (lme4).
#   base       : durée + distance totale + vitesse max + jour de match estimé
#   masculins  : base + haute intensité + sprint > 25 km/h + accélérations et
#                décélérations hautes (seuils masculins, 3 m/s²)
#   féminins   : base + haute intensité + sprint > 23 km/h + accélérations et
#                décélérations hautes (seuils féminins, 2,7 m/s²)
#   (la haute intensité > 19 km/h est la même dans les deux versions : seuls le
#   sprint et les accélérations changent)
# Comparaison : R² marginal et conditionnel (Nakagawa), AIC, et surtout
# l'erreur de prédiction en validation croisée :
#   - par joueuse (une joueuse mise de côté, prédite sans la connaître) ;
#   - par jours (5 groupes de jours, joueuses connues).
# Indicateur par indicateur (distance totale, haute
# intensité, sprint, vitesse max, accélérations, décélérations) : à durée
# égale, combien chacun ajoute-t-il à l'explication de la sRPE ? Gain de R²
# marginal et d'AIC par rapport au modèle « durée seule », coefficient
# standardisé, et corrélation intra-joueuse (valeurs centrées sur la moyenne
# de chaque joueuse : une joueuse comparée à elle-même).
# Seuls les jours avec sRPE > 0 et GPS sont gardés ; gardiennes probables
# écartées. Corrélation n'est pas causalité, et un meilleur R² ne fait pas un
# meilleur seuil pour le staff : seulement une mesure plus proche du ressenti.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

SG_VARS <- list(
  base      = c("duration_min", "total_km", "vmax_1s_kmh", "match"),
  masculins = c("hi_w_m", "sprint_men_m", "n_acc_high_men", "n_dec_high_men"),
  feminins  = c("hi_w_m", "sprint_w_m", "n_acc_high_w", "n_dec_high_w"))

SG_SINGLE <- c(total_km = "Distance totale", hsr_m = "Course rapide > 16 km/h",
               hi_w_m = "Haute intensité > 19 km/h", sprint_w_m = "Sprint > 23 km/h",
               sprint_men_m = "Sprint > 25 km/h", vmax_1s_kmh = "Vitesse max (1 s)",
               n_acc_high_w = "Accélérations > 2,7 m/s²", n_acc_high_men = "Accélérations > 3 m/s²",
               n_dec_high_w = "Décélérations hautes (seuil féminin)", n_dec_high_men = "Décélérations hautes (seuil masculin)")

#' Table jour-joueuse : sRPE (charge déclarée) + GPS du même jour
srpe_gps_data <- function(metrics, gps_daily, cfg = CONFIG) {
  need <- c("duration_min", "total_km", "vmax_1s_kmh", "hsr_m", "hi_w_m", "sprint_w_m",
            "sprint_men_m", "n_acc_high_w", "n_acc_high_men", "n_dec_high_w", "n_dec_high_men")
  if (is.null(gps_daily) || !all(need %in% names(gps_daily))) return(NULL)
  gk <- opp_goalkeepers(gps_daily, cfg)
  g <- gps_daily |> filter(!player %in% gk) |> select(player, date, all_of(need))
  # jour de match estimé : le jour le plus chargé de la semaine pour l'équipe
  # (médiane de la distance totale, au moins `opp_min_players` joueuses)
  md <- g |> mutate(team = sub("-.*$", "", player), week = format(date, "%G-%V")) |>
    group_by(team, date, week) |> filter(n() >= cfg$opp_min_players) |>
    summarise(km = median(total_km), .groups = "drop") |>
    group_by(team, week) |> slice_max(km, n = 1, with_ties = FALSE) |> ungroup() |>
    transmute(team, date, match = 1)
  metrics |>
    select(player, label, date, srpe = load) |>
    inner_join(g, by = c("player", "date")) |>
    mutate(team = sub("-.*$", "", player)) |>
    left_join(md, by = c("team", "date")) |>
    mutate(match = coalesce(match, 0)) |>
    filter(srpe > 0, duration_min >= cfg$sg_min_duration, total_km > 0) |>
    drop_na(all_of(need)) |>
    # RPE implicite = sRPE du jour / durée GPS. Hors de 1-10, la sRPE ne
    # correspond pas à la séance GPS (séance sans GPS en plus, muscu, durée
    # déclarée différente) : ces jours sont signalés et écartés par défaut.
    mutate(rpe_implicite = srpe / duration_min,
           coherent = rpe_implicite >= cfg$sg_rpe_range[1] & rpe_implicite <= cfg$sg_rpe_range[2])
}

#' Ajuste un modèle mixte (variables centrées-réduites, maximum de vraisemblance)
sg_fit <- function(d, vars) {
  f <- stats::as.formula(paste("srpe ~", paste(vars, collapse = " + "), "+ (1 | player)"))
  suppressMessages(lme4::lmer(f, data = d, REML = FALSE))
}

#' R² marginal (effets fixes seuls) et conditionnel (avec l'effet joueuse)
sg_r2 <- function(m) {
  vf <- stats::var(as.vector(lme4::getME(m, "X") %*% lme4::fixef(m)))
  vu <- as.numeric(lme4::VarCorr(m)$player)
  ve <- stats::sigma(m)^2
  c(r2_marginal = vf / (vf + vu + ve), r2_conditional = (vf + vu) / (vf + vu + ve),
    icc = vu / (vu + ve))
}

#' Validation croisée : par joueuse (prédiction sans effet joueuse) et par jours
sg_cv <- function(d, vars, folds = 5, seed = 1) {
  err <- function(y, p) c(rmse = sqrt(mean((y - p)^2)), mae = mean(abs(y - p)))
  # par joueuse
  by_p <- bind_rows(lapply(unique(d$player), function(p) {
    tr <- d[d$player != p, ]; te <- d[d$player == p, ]
    m <- sg_fit(tr, vars)
    pr <- stats::predict(m, newdata = te, re.form = NA)
    tibble(player = p, n = nrow(te), sq = sum((te$srpe - pr)^2), ab = sum(abs(te$srpe - pr)))
  }))
  # par jours (joueuses connues)
  set.seed(seed)
  k <- sample(rep(seq_len(folds), length.out = nrow(d)))
  pr <- numeric(nrow(d))
  for (i in seq_len(folds)) {
    m <- sg_fit(d[k != i, ], vars)
    pr[k == i] <- stats::predict(m, newdata = d[k == i, ], allow.new.levels = TRUE)
  }
  list(player = by_p |> mutate(rmse = sqrt(sq / n)),
       summary = c(cv_player_rmse = sqrt(sum(by_p$sq) / sum(by_p$n)),
                   cv_player_mae = sum(by_p$ab) / sum(by_p$n),
                   cv_days_rmse = err(d$srpe, pr)[["rmse"]],
                   cv_days_mae = err(d$srpe, pr)[["mae"]]))
}

#' Indicateur par indicateur, à durée égale
sg_single <- function(d) {
  m0 <- sg_fit(d, "duration_min"); r0 <- sg_r2(m0)[["r2_marginal"]]; a0 <- stats::AIC(m0)
  ctr <- function(x, p) x - stats::ave(x, p)
  bind_rows(lapply(names(SG_SINGLE), function(v) {
    m <- sg_fit(d, c("duration_min", v))
    tibble(indicator = v, label = unname(SG_SINGLE[v]),
           gain_r2_marginal = sg_r2(m)[["r2_marginal"]] - r0,
           gain_aic = a0 - stats::AIC(m),
           coef_std = lme4::fixef(m)[[v]],
           r_brut = stats::cor(d$srpe, d[[v]]),
           r_intra_joueuse = stats::cor(ctr(d$srpe, d$player), ctr(d[[v]], d$player)))
  })) |> arrange(desc(gain_r2_marginal))
}

#' Tout le module en un appel (utilisé par run_pipeline.R)
compute_srpe_gps <- function(metrics, gps_daily, cfg = CONFIG) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    message("   sRPE et GPS : paquet lme4 absent, module ignoré (install.packages(\"lme4\"))")
    return(NULL)
  }
  d_all <- srpe_gps_data(metrics, gps_daily, cfg)
  if (is.null(d_all) || nrow(d_all) < 200) return(NULL)
  num <- union(setdiff(unique(unlist(SG_VARS)), "match"), names(SG_SINGLE))
  zs <- function(x) x |> mutate(across(all_of(num), ~ as.numeric(scale(.x))))
  # contrôle : mêmes modèles sur tous les jours, sans le filtre de cohérence
  r2_all <- sg_r2(sg_fit(zs(d_all), c(SG_VARS$base, SG_VARS$feminins)))
  d0 <- d_all |> filter(coherent)
  d <- zs(d0)
  specs <- list(base = SG_VARS$base,
                masculins = c(SG_VARS$base, SG_VARS$masculins),
                feminins = c(SG_VARS$base, SG_VARS$feminins))
  fits <- lapply(specs, function(v) sg_fit(d, v))
  cvs <- lapply(specs, function(v) sg_cv(d, v))
  single <- sg_single(d)
  models <- bind_rows(lapply(names(specs), function(k) {
    r <- sg_r2(fits[[k]])
    tibble(model = k, n_days = nrow(d), n_players = n_distinct(d$player),
           aic = stats::AIC(fits[[k]]), !!!as.list(r), !!!as.list(cvs[[k]]$summary))
  }))
  coefs <- bind_rows(lapply(names(fits), function(k) {
    s <- summary(fits[[k]])$coefficients
    tibble(model = k, term = rownames(s), estimate = s[, 1], se = s[, 2], t = s[, 3])
  })) |> filter(term != "(Intercept)")
  # joueuse par joueuse : quel jeu de seuils prédit le mieux une joueuse inconnue ?
  pp <- inner_join(cvs$masculins$player |> select(player, n, rmse_m = rmse),
                   cvs$feminins$player |> select(player, rmse_f = rmse), by = "player")
  r <- function(k, col) round(models[[col]][models$model == k], 3)
  stats <- list(
    days_all = nrow(d_all),
    share_coherent = round(mean(d_all$coherent), 3),
    rpe_implicite_quartiles = paste(round(stats::quantile(d_all$rpe_implicite, c(.25, .5, .75)), 2), collapse = " / "),
    r2_all_days_feminins = sprintf("marginal %.3f · conditionnel %.3f", r2_all[["r2_marginal"]], r2_all[["r2_conditional"]]),
    match_days_share = round(mean(d0$match), 3),
    days = nrow(d), players = n_distinct(d$player),
    srpe_median = stats::median(d0$srpe),
    r2_marginal = sprintf("base %s · masculins %s · féminins %s", r("base", "r2_marginal"), r("masculins", "r2_marginal"), r("feminins", "r2_marginal")),
    r2_conditional = sprintf("base %s · masculins %s · féminins %s", r("base", "r2_conditional"), r("masculins", "r2_conditional"), r("feminins", "r2_conditional")),
    icc_base = r("base", "icc"),
    aic_fem_minus_men = round(models$aic[models$model == "feminins"] - models$aic[models$model == "masculins"], 1),
    cv_player_rmse = sprintf("base %s · masculins %s · féminins %s", round(r("base", "cv_player_rmse")), round(r("masculins", "cv_player_rmse")), round(r("feminins", "cv_player_rmse"))),
    cv_days_rmse = sprintf("base %s · masculins %s · féminins %s", round(r("base", "cv_days_rmse")), round(r("masculins", "cv_days_rmse")), round(r("feminins", "cv_days_rmse"))),
    players_better_women = sprintf("%d sur %d", sum(pp$rmse_f < pp$rmse_m), nrow(pp)),
    r2_duration_only = round(sg_r2(sg_fit(d, "duration_min"))[["r2_marginal"]], 3),
    single_gain_r2 = paste(sprintf("%s +%.3f", single$indicator, single$gain_r2_marginal), collapse = " · "),
    single_r_intra = paste(sprintf("%s %.2f", single$indicator, single$r_intra_joueuse), collapse = " · "))
  list(data = d0, models = models, coefficients = coefs, per_player = pp, single = single, stats = stats)
}
