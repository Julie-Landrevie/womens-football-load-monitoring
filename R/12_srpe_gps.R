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

#
# Tests complémentaires (échange du 11/11) :
#   - contribution propre : modèle complet avec tous les indicateurs, puis
#     retrait de chacun ; ce qu'il apporte une fois les autres pris en compte
#     (les indicateurs sont très corrélés : l'ordre « un par un » ne suffit pas) ;
#   - seuils masculins usuels 19,8 / 25,2 km/h (colonnes calculées par
#     run_gps.R ; ignorés s'ils sont absents de gps_daily.csv) ;
#   - jours de match estimés et entraînements séparés ;
#   - jours à une seule séance GPS contre jours à plusieurs séances.
#
# Fichiers SoccerMon supplémentaires (s'ils sont présents dans data/raw) :
#   - training-load/session.json : charge séance par séance (sRPE, RPE, durée
#     déclarée, date ; pas d'heure de saisie). Permet d'apparier comme le
#     mémoire UiT : jours à UNE séance déclarée et UNE séance GPS ;
#   - game-performance/game-performance.csv : note de performance donnée par
#     les joueuses après les matchs. Une date notée par au moins
#     `sg_match_min_reports` joueuses d'une équipe = vrai jour de match pour
#     l'équipe (remplace l'estimation « jour le plus chargé de la semaine »).

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

SG_VARS <- list(
  base      = c("duration_min", "total_km", "vmax_1s_kmh", "match"),
  masculins = c("hi_w_m", "sprint_men_m", "n_acc_high_men", "n_dec_high_men"),
  feminins  = c("hi_w_m", "sprint_w_m", "n_acc_high_w", "n_dec_high_w"),
  masculins_198 = c("hi_men198_m", "sprint_men252_m", "n_acc_high_men", "n_dec_high_men"))

# indicateurs du modèle complet (contribution propre), par jeu de seuils
SG_FULL <- list(
  feminins  = c("total_km", "hsr_m", "hi_w_m", "sprint_w_m", "vmax_1s_kmh", "n_acc_high_w", "n_dec_high_w"),
  masculins = c("total_km", "hsr_m", "hi_w_m", "sprint_men_m", "vmax_1s_kmh", "n_acc_high_men", "n_dec_high_men"),
  masculins_198 = c("total_km", "hsr_m", "hi_men198_m", "sprint_men252_m", "vmax_1s_kmh", "n_acc_high_men", "n_dec_high_men"))

SG_SINGLE <- c(total_km = "Distance totale", hsr_m = "Course rapide > 16 km/h",
               hi_w_m = "Haute intensité > 19 km/h", sprint_w_m = "Sprint > 23 km/h",
               sprint_men_m = "Sprint > 25 km/h", vmax_1s_kmh = "Vitesse max (1 s)",
               n_acc_high_w = "Accélérations > 2,7 m/s²", n_acc_high_men = "Accélérations > 3 m/s²",
               n_dec_high_w = "Décélérations hautes (seuil féminin)", n_dec_high_men = "Décélérations hautes (seuil masculin)",
               hi_men198_m = "Haute vitesse > 19,8 km/h", sprint_men252_m = "Sprint > 25,2 km/h")

#' Séances déclarées (session.json) -> une ligne par joueuse et par jour
sg_sessions <- function(dir = CONFIG$data_dir) {
  f <- list.files(dir, pattern = "^session\\.json$", recursive = TRUE, full.names = TRUE)
  if (!length(f) || !requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  j <- jsonlite::fromJSON(f[1], simplifyVector = TRUE)
  s <- bind_rows(lapply(names(j), function(p) {
    x <- j[[p]]
    if (!is.data.frame(x) || !nrow(x)) return(NULL)
    tibble(player = p, date = as.Date(x$date, "%d.%m.%Y"),
           srpe = as.numeric(x$srpe), rpe = as.numeric(x$rpe), duration = as.numeric(x$duration))
  }))
  s |> filter(!is.na(date)) |> group_by(player, date) |>
    summarise(n_decl = n(), srpe_decl = sum(srpe, na.rm = TRUE), duration_decl = sum(duration, na.rm = TRUE),
              rpe_decl = if (sum(duration, na.rm = TRUE) > 0) stats::weighted.mean(rpe, duration, na.rm = TRUE) else mean(rpe, na.rm = TRUE),
              .groups = "drop")
}

#' Vrais jours de match : dates notées dans game-performance.csv
sg_games <- function(dir = CONFIG$data_dir, cfg = CONFIG) {
  f <- list.files(dir, pattern = "^game-performance\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(f)) return(NULL)
  g <- utils::read.csv(f[1], stringsAsFactors = FALSE)
  if (!all(c("player_name", "timestamp") %in% names(g))) return(NULL)
  tibble(player = g$player_name, date = as.Date(substr(g$timestamp, 1, 10), "%d.%m.%Y")) |>
    filter(!is.na(date)) |> mutate(team = sub("-.*$", "", player)) |>
    count(team, date, name = "n_reports") |>
    filter(n_reports >= cfg$sg_match_min_reports) |>
    transmute(team, date, match_true = 1)
}

#' Table jour-joueuse : sRPE (charge déclarée) + GPS du même jour
srpe_gps_data <- function(metrics, gps_daily, cfg = CONFIG) {
  need <- c("duration_min", "total_km", "vmax_1s_kmh", "hsr_m", "hi_w_m", "sprint_w_m",
            "sprint_men_m", "n_acc_high_w", "n_acc_high_men", "n_dec_high_w", "n_dec_high_men")
  if (is.null(gps_daily) || !all(need %in% names(gps_daily))) return(NULL)
  gk <- opp_goalkeepers(gps_daily, cfg)
  g <- gps_daily |> filter(!player %in% gk) |>
    select(player, date, all_of(need), any_of(c("n_files", "hi_men198_m", "sprint_men252_m")))
  # jour de match estimé : médiane d'équipe de la distance totale > `match_team_km`
  # (au moins `opp_min_players` joueuses avec GPS ; voir 00_config.R)
  md <- g |> mutate(team = sub("-.*$", "", player)) |>
    group_by(team, date) |> filter(n() >= cfg$opp_min_players) |>
    summarise(km = median(total_km), .groups = "drop") |>
    filter(km > cfg$match_team_km) |>
    transmute(team, date, match = 1)
  metrics |>
    select(player, label, date, srpe = load) |>
    inner_join(g, by = c("player", "date")) |>
    mutate(team = sub("-.*$", "", player)) |>
    left_join(md, by = c("team", "date")) |>
    mutate(match = coalesce(match, 0), match_est = match) -> x
  # matchs notés par les joueuses, s'ils sont disponibles : les notes sont
  # incomplètes (26 matchs), on garde donc aussi l'estimation (match = l'un ou l'autre)
  games <- sg_games(cfg$data_dir, cfg)
  if (!is.null(games) && nrow(games)) {
    x <- x |> left_join(games, by = c("team", "date")) |>
      mutate(match_true = coalesce(match_true, 0), match = pmax(match_true, match_est))
  }
  # séances déclarées, si disponibles
  sess <- sg_sessions(cfg$data_dir)
  if (!is.null(sess)) x <- x |> left_join(sess, by = c("player", "date"))
  x |>
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
  bind_rows(lapply(intersect(names(SG_SINGLE), names(d)), function(v) {
    m <- sg_fit(d, c("duration_min", v))
    tibble(indicator = v, label = unname(SG_SINGLE[v]),
           gain_r2_marginal = sg_r2(m)[["r2_marginal"]] - r0,
           gain_aic = a0 - stats::AIC(m),
           coef_std = lme4::fixef(m)[[v]],
           r_brut = stats::cor(d$srpe, d[[v]]),
           r_intra_joueuse = stats::cor(ctr(d$srpe, d$player), ctr(d[[v]], d$player)))
  })) |> arrange(desc(gain_r2_marginal))
}

#' Contribution propre : retrait de chaque indicateur du modèle complet
sg_unique <- function(d, set) {
  vars <- SG_FULL[[set]]
  if (!all(vars %in% names(d))) return(NULL)
  full <- sg_fit(d, c("duration_min", "match", vars))
  r_full <- sg_r2(full)[["r2_marginal"]]; a_full <- stats::AIC(full)
  bind_rows(lapply(vars, function(v) {
    m <- sg_fit(d, c("duration_min", "match", setdiff(vars, v)))
    tibble(seuils = set, indicator = v, label = unname(SG_SINGLE[v]),
           perte_r2_si_retire = r_full - sg_r2(m)[["r2_marginal"]],
           perte_aic_si_retire = stats::AIC(m) - a_full,
           coef_std = lme4::fixef(full)[[v]])
  })) |> mutate(r2_modele_complet = r_full) |> arrange(desc(perte_r2_si_retire))
}

#' Match estimé contre entraînement : mêmes modèles sur chaque sous-ensemble
sg_by_context <- function(d, specs) {
  bind_rows(lapply(c(match = 1, entrainement = 0), function(k) {
    s <- d[d$match == k, ]
    if (nrow(s) < 200) return(NULL)
    ctx <- if (k == 1) "match" else "entraînement"
    r <- bind_rows(lapply(names(specs), function(nm) {
      m <- sg_fit(s, setdiff(specs[[nm]], "match"))
      tibble(contexte = ctx, model = nm, n_days = nrow(s), n_players = n_distinct(s$player),
             r2_marginal = sg_r2(m)[["r2_marginal"]], aic = stats::AIC(m))
    }))
    top <- sg_single(s)
    r |> mutate(top_indicateurs = paste(sprintf("%s +%.3f", head(top$indicator, 4), head(top$gain_r2_marginal, 4)), collapse = " · "))
  }))
}

#' Tout le module en un appel (utilisé par run_pipeline.R)
compute_srpe_gps <- function(metrics, gps_daily, cfg = CONFIG) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    message("   sRPE et GPS : paquet lme4 absent, module ignoré (install.packages(\"lme4\"))")
    return(NULL)
  }
  d_all <- srpe_gps_data(metrics, gps_daily, cfg)
  if (is.null(d_all) || nrow(d_all) < 200) return(NULL)
  num <- intersect(union(setdiff(unique(unlist(SG_VARS)), "match"), names(SG_SINGLE)), names(d_all))
  has198 <- all(c("hi_men198_m", "sprint_men252_m") %in% names(d_all)) &&
    !all(is.na(d_all$hi_men198_m))
  if (has198) d_all <- d_all |> drop_na(hi_men198_m, sprint_men252_m)
  zs <- function(x) x |> mutate(across(all_of(num), ~ as.numeric(scale(.x))))
  # contrôle : mêmes modèles sur tous les jours, sans le filtre de cohérence
  r2_all <- sg_r2(sg_fit(zs(d_all), c(SG_VARS$base, SG_VARS$feminins)))
  d0 <- d_all |> filter(coherent)
  d <- zs(d0)
  specs <- list(base = SG_VARS$base,
                masculins = c(SG_VARS$base, SG_VARS$masculins),
                feminins = c(SG_VARS$base, SG_VARS$feminins))
  if (has198) specs$masculins_198 <- c(SG_VARS$base, SG_VARS$masculins_198)
  fits <- lapply(specs, function(v) sg_fit(d, v))
  cvs <- lapply(specs, function(v) sg_cv(d, v))
  single <- sg_single(d)
  unique_c <- bind_rows(lapply(names(specs)[names(specs) != "base"], function(k) sg_unique(d, k)))
  cor_mat <- round(stats::cor(d[, intersect(names(SG_SINGLE), names(d))], method = "spearman"), 2)
  context <- sg_by_context(d, specs)
  # doubles séances : jours à plusieurs fichiers GPS
  multi <- if ("n_files" %in% names(d_all)) {
    d_all |> mutate(seances = if_else(n_files >= 2, "2 séances GPS ou plus", "1 séance GPS")) |>
      group_by(seances) |> summarise(n_days = n(), share_coherent = mean(coherent),
                                     rpe_implicite_mediane = stats::median(rpe_implicite), .groups = "drop")
  } else NULL
  # appariement séance par séance (comme le mémoire UiT) : une séance déclarée, une séance GPS
  one <- if (all(c("n_decl", "n_files") %in% names(d0))) which(d0$n_decl %in% 1 & d0$n_files == 1) else integer(0)
  seance_unique <- if (length(one) >= 200) {
    s1 <- d[one, ]
    fs <- lapply(specs, function(v) sg_fit(s1, v))
    list(n_days = length(one), n_players = n_distinct(s1$player),
         r2 = sapply(fs, function(m) sg_r2(m)[["r2_marginal"]]),
         r2_cond = sapply(fs, function(m) sg_r2(m)[["r2_conditional"]]),
         aic_fem_men = stats::AIC(fs$feminins) - stats::AIC(fs$masculins),
         single = sg_single(s1),
         unique = sg_unique(s1, "feminins"))
  } else NULL
  r2_single_session <- if ("n_files" %in% names(d0)) {
    s1 <- d[d0$n_files == 1, ]
    if (nrow(s1) >= 200) sg_r2(sg_fit(s1, specs$feminins))[["r2_marginal"]] else NA_real_
  } else NA_real_
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
    single_r_intra = paste(sprintf("%s %.2f", single$indicator, single$r_intra_joueuse), collapse = " · "),
    seuils_198_252 = if (has198) sprintf("R² marginal %s · AIC féminins − masculins 19,8/25,2 : %.1f · erreur par joueuse %s",
                                         r("masculins_198", "r2_marginal"),
                                         models$aic[models$model == "feminins"] - models$aic[models$model == "masculins_198"],
                                         round(r("masculins_198", "cv_player_rmse"))) else "colonnes absentes : relancer run_gps.R",
    unique_feminins = { u <- unique_c[unique_c$seuils == "feminins", ]
      paste(sprintf("%s %+.3f", u$indicator, u$perte_r2_si_retire), collapse = " · ") },
    contexte = paste(sprintf("%s (%d j) R² féminins %.3f", context$contexte[context$model == "feminins"],
                             context$n_days[context$model == "feminins"], context$r2_marginal[context$model == "feminins"]), collapse = " · "),
    doubles_seances = if (is.null(multi)) NA else paste(sprintf("%s : %d j, cohérents %.0f %%", multi$seances, multi$n_days, 100 * multi$share_coherent), collapse = " · "),
    r2_une_seance_feminins = round(r2_single_session, 3),
    jours_de_match = if ("match_true" %in% names(d_all))
      sprintf("matchs notés (game-performance) : %d jours-équipe, %.0f %% des jours-joueuses ; l'estimation en retrouve %.0f %% (jours-équipe) ; jours estimés : %.0f %% des jours-joueuses",
              n_distinct(paste(d_all$team[d_all$match_true == 1], d_all$date[d_all$match_true == 1])),
              100 * mean(d_all$match_true),
              100 * { td <- distinct(d_all[d_all$match_true == 1, c("team", "date", "match_est")]); mean(td$match_est == 1) },
              100 * mean(d_all$match_est)) else "estimés (game-performance absent)",
    seances_declarees = if ("n_decl" %in% names(d_all))
      sprintf("1 séance déclarée : %.0f %% des jours ; incohérents avec 2 séances déclarées ou plus : %.0f %% ; sRPE déclarée = charge du jour : %.0f %%",
              100 * mean(d_all$n_decl %in% 1), 100 * mean(d_all$n_decl[!d_all$coherent] >= 2, na.rm = TRUE),
              100 * mean(abs(d_all$srpe_decl - d_all$srpe) < 1, na.rm = TRUE)) else "session.json absent",
    seance_unique = if (is.null(seance_unique)) "trop peu de jours ou session.json absent" else
      sprintf("%d jours, %d joueuses : R² marginal base %.3f · masculins %.3f · féminins %.3f (conditionnel féminins %.3f) ; AIC féminins − masculins %.1f",
              seance_unique$n_days, seance_unique$n_players, seance_unique$r2[["base"]], seance_unique$r2[["masculins"]],
              seance_unique$r2[["feminins"]], seance_unique$r2_cond[["feminins"]], seance_unique$aic_fem_men),
    seance_unique_top = if (is.null(seance_unique)) NA else
      paste(sprintf("%s +%.3f", head(seance_unique$single$indicator, 5), head(seance_unique$single$gain_r2_marginal, 5)), collapse = " · "),
    seance_unique_propre = if (is.null(seance_unique)) NA else
      paste(sprintf("%s %+.3f", seance_unique$unique$indicator, seance_unique$unique$perte_r2_si_retire), collapse = " · "))
  list(data = d0, models = models, coefficients = coefs, per_player = pp, single = single,
       unique = unique_c, correlations = cor_mat, context = context, multi = multi,
       seance_unique = seance_unique, stats = stats)
}
