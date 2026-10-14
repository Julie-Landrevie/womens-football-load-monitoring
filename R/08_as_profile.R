# =============================================================================
# 08_as_profile.R — Profil accélération-vitesse mesuré en séance (« in situ »)
#
# Méthode (Morin et al., 2021) : en séance, la joueuse ne fait pas de test
# maximal, mais sur plusieurs séances elle atteint, à chaque vitesse, une
# accélération proche de son maximum. On garde donc, par tranche de vitesse,
# les deux accélérations les plus élevées observées, puis on ajuste une droite au-delà de
# 3 m/s :  a = A0 x (1 - v / S0)
#   A0 : accélération maximale théorique (m/s²), qualité « force »
#   S0 : vitesse maximale théorique (m/s), qualité « vitesse »
#
# Étape 1 (run_gps.R, une fois, sur les fichiers GPS bruts) : pour
# chaque séance, accélération maximale par tranche de 0,2 m/s.
# Étape 2 (ici) : ajustement du profil sur une fenêtre de plusieurs séances.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr) })

AS_BINS <- seq(0, 10, by = 0.2)   # bornes des tranches de vitesse (m/s)

#' Accélération maximale par tranche de vitesse pour une séance brute
as_session_df <- function(df, gps_cfg = GPS_CONFIG) {
  d <- df[!duplicated(df$time), c("time", "speed", "lat")]
  d$sec <- time_to_sec(d$time)
  d <- d[order(d$sec), ]
  if (nrow(d) < 100) return(NULL)
  fix <- !is.na(d$lat) & d$lat != 0
  v <- ifelse(fix & d$speed >= 0 & d$speed <= gps_cfg$max_speed, d$speed, NA)
  # lissage : médiane 0,5 s (pics isolés) puis moyenne 0,5 s (bruit)
  v <- roll_median(v, 5)
  v <- as.numeric(stats::filter(v, rep(1 / 5, 5), sides = 2))
  n <- length(v)
  # accélération centrée sur 0,2 s, uniquement sur des pas réguliers de 0,1 s
  dt2 <- c(NA, d$sec[3:n] - d$sec[1:(n - 2)], NA)
  acc <- c(NA, (v[3:n] - v[1:(n - 2)]), NA) / dt2
  acc[is.na(dt2) | dt2 > 0.25 | dt2 <= 0] <- NA
  acc[acc > 10 | acc < -10] <- NA                     # valeurs physiquement impossibles
  bin <- cut(v, AS_BINS, right = FALSE)
  amax <- tapply(acc, bin, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  amax <- as.numeric(amax)
  names(amax) <- sprintf("a_%.1f", head(AS_BINS, -1) + 0.1)   # centre de tranche
  as_tibble(as.list(amax))
}

#' Ajuste le profil A-S sur un ensemble de séances (lignes de as_sessions)
#' Renvoie NULL si les données ne permettent pas un ajustement fiable.
as_fit <- function(sessions, cfg = CONFIG, min_sessions = 3) {
  if (is.null(sessions) || nrow(sessions) < min_sessions) return(NULL)
  cols <- grep("^a_", names(sessions), value = TRUE)
  vmid <- as.numeric(sub("^a_", "", cols))
  keep_bin <- vmid >= cfg$as_min_speed
  # Morin et al. (2021) : les DEUX accélérations les plus élevées de chaque
  # tranche de 0,2 m/s (et non la seule plus haute, plus sensible au bruit du GPS)
  pts <- do.call(rbind, lapply(which(keep_bin), function(j) {
    v <- sessions[[cols[j]]]; v <- sort(v[is.finite(v) & v > 0], decreasing = TRUE)
    if (!length(v)) return(NULL)
    data.frame(x = vmid[j], y = head(v, 2))
  }))
  if (is.null(pts) || length(unique(pts$x)) < cfg$as_min_points) return(NULL)
  fit <- stats::lm(y ~ x, data = pts)
  # retrait des points hors de l'intervalle de prédiction à 95 %, puis nouvel ajustement
  pr <- suppressWarnings(stats::predict(fit, pts, interval = "prediction", level = 0.95))
  inside <- pts$y >= pr[, "lwr"] & pts$y <= pr[, "upr"]
  if (sum(inside) >= 2 * cfg$as_min_points / 2 && any(!inside)) fit <- stats::lm(y ~ x, data = pts[inside, ])
  b <- stats::coef(fit)
  if (b[2] >= 0) return(NULL)
  a0 <- unname(b[1]); s0 <- unname(-b[1] / b[2])
  r2 <- summary(fit)$r.squared
  if (a0 < 2 || a0 > 12 || s0 < 5 || s0 > 12 || r2 < cfg$as_min_r2) return(NULL)
  list(a0 = a0, s0 = s0, r2 = r2, n_sessions = nrow(sessions))
}

#' Lecture du résultat de run_gps.R (si le fichier existe)
load_as_sessions <- function(path = "data/processed/as_sessions.csv") {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(date = as.Date(date)) |>
    filter(is.na(error))
}

#' Profil A-S mensuel par joueuse (pour le suivi et le contrôle qualité)
as_monthly <- function(as_sessions, labels, cfg = CONFIG) {
  if (is.null(as_sessions) || nrow(as_sessions) == 0) return(NULL)
  as_sessions |>
    mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
    group_by(player, month) |>
    group_modify(function(g, k) {
      f <- as_fit(g, cfg)
      if (is.null(f)) tibble() else as_tibble(f)
    }) |>
    ungroup() |>
    left_join(labels, by = "player")
}
