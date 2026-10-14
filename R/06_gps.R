# =============================================================================
# 06_gps.R — Charge externe : agrégation des fichiers GPS SoccerMon
#
# Format (STATSports APEX) : un fichier parquet par joueuse et par jour,
#   <aaaa>/<aaaa-mm>/<aaaa-mm-jj>/<aaaa-mm-jj>-<TeamX-identifiant>.parquet
# Colonnes utiles : time ("08:23:15.6"), speed (m/s), lat, num_satellites.
#
# Deux pièges traités ici :
#   1. Chaque instant GPS (10 Hz) est répété ~10 fois, car l'accéléromètre
#      enregistre à 100 Hz sur des lignes séparées : on ne garde qu'une ligne
#      par instant, sinon les distances seraient multipliées par 10.
#   2. Le signal a des trous : la distance est intégrée avec le vrai pas de
#      temps entre deux mesures, et les trous > 1 s ne sont pas comblés
#      (leur part est mesurée pour le contrôle qualité).
#
# Le traitement complet (~10 000 fichiers, ~99 Go) est long : il est lancé une
# seule fois par run_gps.R, avec reprise possible, et produit une petite table
# data/processed/gps_daily.csv que run_pipeline.R réutilise ensuite.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(readr); library(purrr) })

# Bandes de vitesse et d'accélération.
#  - « SoccerMon » : seuils utilisés dans la publication du jeu de données
#    (16 et 20 km/h), conservés pour comparaison ;
#  - « féminins » : seuils calibrés sur le football féminin d'élite présentés par
#    SkillCorner lors du webinaire public « Women's Football: Dedicated Data,
#    Smarter Decisions » (12/10/2026 ; présentation transmise aux inscrits) :
#    course 13-19 km/h, HSR 19-23 km/h, sprint > 23 km/h, accélération moyenne
#    1,4-2,7 m/s², haute > 2,7 m/s² ;
#  - « masculins » : 20-25 km/h, sprint > 25 km/h, accélération haute > 3 m/s²,
#    pour mesurer ce que les seuils masculins font perdre.
GPS_CONFIG <- list(
  hsr_speed      = 4.44,  # m/s = 16 km/h : course rapide (seuils SoccerMon)
  sprint_speed   = 5.55,  # m/s = 20 km/h : sprint (seuils SoccerMon)
  w_run_speed    = 13 / 3.6,  # féminins : course 13-19 km/h
  w_hi_speed     = 19 / 3.6,  # féminins : haute intensité > 19 km/h (HSR 19-23 + sprint)
  w_sprint_speed = 23 / 3.6,  # féminins : sprint > 23 km/h
  m_sprint_speed = 25 / 3.6,  # masculins : sprint > 25 km/h
  w_acc_med      = 1.4,       # féminins : accélération moyenne 1,4-2,7 m/s²
  w_acc_high     = 2.7,       # féminins : accélération haute > 2,7 m/s²
  m_acc_high     = 3,         # masculins : accélération haute > 3 m/s²
  sprint_min_samples = 10,    # un sprint compté = au moins 1 s au-dessus du seuil
  explo_start_speed  = 2,     # m/s : départ « arrêté ou au trot » pour l'explosivité
  explo_window       = 5,     # s : délai maximal pour atteindre 19 km/h
  max_speed      = 10,    # m/s = 36 km/h : au-delà, valeur aberrante
  acc_threshold  = 2,     # m/s² : accélération / décélération
  acc_min_samples = 5,    # durée minimale d'un effort : 5 x 0,1 s = 0,5 s
  max_gap        = 1,     # s : au-delà, trou de signal non intégré
  min_minutes    = 10,    # séance retenue si au moins 10 min de signal
  min_km         = 0.3    # ... et au moins 300 m parcourus
)

# "08:23:15.6" -> secondes depuis minuit
time_to_sec <- function(t) {
  t <- as.character(t)
  as.numeric(substr(t, 1, 2)) * 3600 + as.numeric(substr(t, 4, 5)) * 60 +
    as.numeric(substr(t, 7, nchar(t)))
}

# médiane glissante centrée sur 5 points (0,5 s) : supprime les pics isolés
roll_median <- function(x, k = 5) {
  if (length(x) < k) return(x)
  na <- is.na(x)
  out <- stats::runmed(ifelse(na, 0, x), k, endrule = "keep")
  out[na] <- NA
  out
}

# nombre d'efforts : séquences d'au moins `min_len` valeurs consécutives vraies
count_efforts <- function(cond, min_len) {
  cond[is.na(cond)] <- FALSE
  r <- rle(cond)
  sum(r$values & r$lengths >= min_len)
}

# efforts au-dessus de `lo` pendant `min_len` échantillons, dont le pic reste
# sous `hi` (accélérations « moyennes » : 1,4-2,7 m/s²)
count_efforts_band <- function(x, lo, hi, min_len) {
  cond <- !is.na(x) & x > lo
  r <- rle(cond); end <- cumsum(r$lengths); start <- end - r$lengths + 1
  k <- which(r$values & r$lengths >= min_len)
  sum(vapply(k, function(i) max(x[start[i]:end[i]]) <= hi, logical(1)))
}

# vitesse maximale maintenue au moins `k` échantillons (1 s à 10 Hz) :
# robuste aux pics GPS isolés (même objectif que la PSV-99 de SkillCorner)
vmax_sustained <- function(v, k = 10) {
  n <- length(v)
  if (n < k || sum(!is.na(v)) < k) return(NA_real_)
  r <- do.call(pmin, lapply(0:(k - 1), function(l) v[(1 + l):(n - k + 1 + l)]))
  if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE)
}

# explosivité : meilleur temps pour passer de < 2 m/s à 19 km/h en moins de 5 s
time_to_speed <- function(v, sec, from, to, window) {
  up <- which(!is.na(v[-1]) & v[-1] >= to & !is.na(v[-length(v)]) & v[-length(v)] < to) + 1
  if (!length(up)) return(c(best = NA_real_, n = 0))
  t <- vapply(up, function(i) {
    j <- i - 1
    while (j > 0 && sec[i] - sec[j] <= window && !is.na(v[j]) && v[j] >= from) j <- j - 1
    if (j < 1 || is.na(v[j]) || v[j] >= from || sec[i] - sec[j] > window) NA_real_ else sec[i] - sec[j]
  }, numeric(1))
  t <- t[!is.na(t)]
  c(best = if (length(t)) min(t) else NA_real_, n = length(t))
}

#' Résumé d'une séance à partir des données brutes (data.frame)
gps_summarise_df <- function(df, cfg = GPS_CONFIG) {
  d <- df[!duplicated(df$time), c("time", "speed", "lat")]
  d$sec <- time_to_sec(d$time)
  d <- d[order(d$sec), ]
  n_raw <- nrow(d)
  if (n_raw < 10) return(NULL)

  fix   <- !is.na(d$lat) & d$lat != 0
  speed <- ifelse(fix & d$speed >= 0 & d$speed <= cfg$max_speed, d$speed, NA)
  dt    <- c(diff(d$sec), 0.1)
  gap   <- dt > cfg$max_gap
  dt_ok <- ifelse(gap | dt <= 0, 0, dt)
  v_ok  <- ifelse(is.na(speed), 0, speed)

  dist_step <- v_ok * dt_ok
  smooth <- roll_median(speed, 5)
  # accélération sur des pas consécutifs de ~0,1 s uniquement
  acc <- c(NA, diff(smooth) / diff(d$sec))
  acc[c(FALSE, diff(d$sec) > 0.15)] <- NA

  ex <- time_to_speed(smooth, d$sec, cfg$explo_start_speed, cfg$w_hi_speed, cfg$explo_window)
  tibble(
    duration_min = (max(d$sec) - min(d$sec)) / 60,
    signal_min   = sum(dt_ok) / 60,
    gap_share    = 1 - sum(dt_ok) / max(1e-9, max(d$sec) - min(d$sec)),
    nofix_share  = mean(!fix),
    total_km     = sum(dist_step) / 1000,
    hsr_m        = sum(dist_step[v_ok > cfg$hsr_speed]),
    sprint_m     = sum(dist_step[v_ok > cfg$sprint_speed]),
    vmax_kmh     = suppressWarnings(max(smooth, na.rm = TRUE)) * 3.6,
    n_acc        = count_efforts(acc >  cfg$acc_threshold, cfg$acc_min_samples),
    n_dec        = count_efforts(acc < -cfg$acc_threshold, cfg$acc_min_samples),
    # seuils féminins (SkillCorner, webinaire 12/10/2026) et masculins
    run_w_m      = sum(dist_step[v_ok > cfg$w_run_speed & v_ok <= cfg$w_hi_speed]),
    hi_w_m       = sum(dist_step[v_ok > cfg$w_hi_speed]),
    sprint_w_m   = sum(dist_step[v_ok > cfg$w_sprint_speed]),
    sprint_men_m = sum(dist_step[v_ok > cfg$m_sprint_speed]),
    n_sprint_w   = count_efforts(smooth > cfg$w_sprint_speed, cfg$sprint_min_samples),
    n_sprint_men = count_efforts(smooth > cfg$m_sprint_speed, cfg$sprint_min_samples),
    n_acc_med_w  = count_efforts_band(acc, cfg$w_acc_med, cfg$w_acc_high, cfg$acc_min_samples),
    n_acc_high_w = count_efforts(acc >  cfg$w_acc_high, cfg$acc_min_samples),
    n_acc_high_men = count_efforts(acc >  cfg$m_acc_high, cfg$acc_min_samples),
    n_dec_high_w = count_efforts(acc < -cfg$w_acc_high, cfg$acc_min_samples),
    n_dec_high_men = count_efforts(acc < -cfg$m_acc_high, cfg$acc_min_samples),
    vmax_1s_kmh  = vmax_sustained(smooth, 10) * 3.6,
    t19_best_s   = unname(ex["best"]),
    n_t19        = unname(ex["n"]),
    n_samples    = n_raw
  )
}

#' Lecture d'un fichier (parquet en usage normal ; csv accepté pour les tests)
gps_read_file <- function(path) {
  if (grepl("\\.parquet$", path)) {
    as.data.frame(arrow::read_parquet(path, col_select = c("time", "speed", "lat")))
  } else {
    read.csv(path)[, c("time", "speed", "lat")]
  }
}

#' Liste des fichiers GPS et informations tirées du nom de fichier
gps_list_files <- function(dir) {
  files <- list.files(dir, pattern = "^\\d{4}-\\d{2}-\\d{2}-Team.*\\.(parquet|csv)$",
                      recursive = TRUE, full.names = TRUE)
  b <- basename(files)
  tibble(path = files,
         file = b,
         date = as.Date(substr(b, 1, 10)),
         player = sub("^\\d{4}-\\d{2}-\\d{2}-(.*)\\.(parquet|csv)$", "\\1", b))
}

#' Traite tous les fichiers, avec cache : un fichier déjà traité n'est pas relu.
#' Le cache est écrit par paquets, on peut donc interrompre et relancer.
process_gps <- function(dir, cache_file = "data/processed/gps_sessions_v2.csv",
                        as_file = "data/processed/as_sessions.csv",
                        cores = max(1, parallel::detectCores() - 2), chunk = 200) {
  if (!dir.exists(dir)) stop("Dossier GPS introuvable : ", dir)
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  files <- gps_list_files(dir)
  if (any(grepl("\\.parquet$", files$path)) && !requireNamespace("arrow", quietly = TRUE))
    stop("Le package 'arrow' est nécessaire : install.packages('arrow')")
  message("  ", nrow(files), " fichiers GPS trouvés")
  done <- if (file.exists(cache_file)) read_csv(cache_file, show_col_types = FALSE) else NULL
  todo <- if (is.null(done)) files else files |> filter(!file %in% done$file)
  message("  ", nrow(todo), " à traiter (", nrow(files) - nrow(todo), " déjà en cache) sur ",
          cores, " cœur(s)")
  if (nrow(todo) == 0) return(invisible(done))

  t0 <- Sys.time()
  idx <- split(seq_len(nrow(todo)), ceiling(seq_len(nrow(todo)) / chunk))
  for (k in seq_along(idx)) {
    rows <- todo[idx[[k]], ]
    # une seule lecture par fichier : résumé de séance ET profil accélération-vitesse
    res <- parallel::mclapply(seq_len(nrow(rows)), function(i) {
      df <- tryCatch(gps_read_file(rows$path[i]), error = function(e) e)
      if (inherits(df, "error"))
        return(list(s = bind_cols(rows[i, c("file", "date", "player")], tibble(error = conditionMessage(df))), a = NULL))
      out <- tryCatch(gps_summarise_df(df), error = function(e) tibble(error = conditionMessage(e)))
      if (is.null(out)) out <- tibble(error = "fichier vide")
      a <- if (exists("as_session_df")) tryCatch(as_session_df(df), error = function(e) NULL) else NULL
      list(s = bind_cols(rows[i, c("file", "date", "player")], out),
           a = if (is.null(a)) NULL else bind_cols(rows[i, c("file", "date", "player")], a, tibble(error = NA_character_)))
    }, mc.cores = cores)
    s_res <- bind_rows(lapply(res, `[[`, "s"))
    if (!"error" %in% names(s_res)) s_res$error <- NA_character_
    write_csv(s_res, cache_file, append = file.exists(cache_file), na = "")
    a_res <- bind_rows(lapply(res, `[[`, "a"))
    if (nrow(a_res)) write_csv(a_res |> mutate(across(starts_with("a_"), ~ round(.x, 3))),
                               as_file, append = file.exists(as_file), na = "")
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    n_done <- sum(lengths(idx[1:k]))
    message(sprintf("  %d / %d fichiers (%.0f %%) — %.1f min écoulées, ~%.0f min restantes",
                    n_done, nrow(todo), 100 * n_done / nrow(todo), el,
                    el / n_done * (nrow(todo) - n_done)))
  }
  invisible(read_csv(cache_file, show_col_types = FALSE))
}

#' Séances -> une ligne par joueuse et par jour, avec contrôle qualité
gps_daily_from_sessions <- function(sessions, cfg = GPS_CONFIG) {
  if (!"error" %in% names(sessions)) sessions$error <- NA_character_
  s <- sessions |>
    mutate(valid = is.na(error) & !is.na(total_km) &
             signal_min >= cfg$min_minutes & total_km >= cfg$min_km)
  daily <- s |>
    filter(valid) |>
    group_by(player, date) |>
    summarise(n_files = n(),
              # calculé AVANT de sommer duration_min (sinon la pondération
              # utiliserait le total du jour au lieu de la durée de chaque séance)
              gap_share = weighted.mean(gap_share, duration_min),
              duration_min = sum(duration_min), signal_min = sum(signal_min),
              total_km = sum(total_km), hsr_m = sum(hsr_m), sprint_m = sum(sprint_m),
              vmax_kmh = max(vmax_kmh), n_acc = sum(n_acc), n_dec = sum(n_dec),
              across(any_of(c("run_w_m", "hi_w_m", "sprint_w_m", "sprint_men_m", "n_sprint_w", "n_sprint_men",
                              "n_acc_med_w", "n_acc_high_w", "n_acc_high_men", "n_dec_high_w", "n_dec_high_men", "n_t19")),
                     ~ sum(.x, na.rm = TRUE)),
              across(any_of("vmax_1s_kmh"), ~ suppressWarnings(max(.x, na.rm = TRUE))),
              across(any_of("t19_best_s"), ~ suppressWarnings(min(.x, na.rm = TRUE))),
              .groups = "drop") |>
    mutate(across(any_of(c("vmax_1s_kmh", "t19_best_s")), ~ ifelse(is.finite(.x), round(.x, 2), NA_real_)),
           across(any_of(c("run_w_m", "hi_w_m", "sprint_w_m", "sprint_men_m")), ~ round(.x, 1))) |>
    mutate(across(c(duration_min, signal_min, total_km, vmax_kmh), ~ round(.x, 3)),
           across(c(hsr_m, sprint_m), ~ round(.x, 1)), gap_share = round(gap_share, 4))
  qc <- tibble(
    indicateur = c("Fichiers GPS", "Fichiers illisibles", "Séances écartées (< 10 min ou < 300 m)",
                   "Jours-joueuses retenus", "Part médiane de signal perdu",
                   "Vitesse max médiane (km/h)"),
    valeur = c(nrow(s), sum(!is.na(s$error)),
               sum(is.na(s$error) & !s$valid, na.rm = TRUE),
               nrow(daily),
               scales::percent(median(daily$gap_share, na.rm = TRUE), 0.1),
               format(round(median(daily$vmax_kmh, na.rm = TRUE), 1))) |> as.character()
  )
  list(daily = daily, qc = qc)
}

#' Chargement de la table journalière déjà calculée (si elle existe)
load_gps_daily <- function(path = "data/processed/gps_daily.csv") {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE) |> mutate(date = as.Date(date))
}

#' Colonnes GPS utilisées par l'analyse : seuils féminins si la table GPS a été
#' produite par la version actuelle de run_gps.R, seuils SoccerMon sinon.
#' Ajoute gps_hi_m, gps_sprint_m, gps_vmax et gps_t19 (explosivité) et renvoie
#' les libellés correspondants.
harmonise_gps <- function(metrics) {
  if (!"total_km" %in% names(metrics)) return(list(metrics = metrics, labels = NULL))
  women <- all(c("hi_w_m", "sprint_w_m") %in% names(metrics))
  m <- metrics |>
    mutate(gps_hi_m     = if (women) hi_w_m else hsr_m,
           gps_sprint_m = if (women) sprint_w_m else sprint_m,
           gps_vmax     = if ("vmax_1s_kmh" %in% names(metrics)) vmax_1s_kmh else vmax_kmh,
           gps_t19      = if ("t19_best_s" %in% names(metrics)) t19_best_s else NA_real_)
  labels <- if (women) list(hi = "> 19 km/h", sprint = "> 23 km/h", set = "féminins (SkillCorner)",
                            vmax = "vitesse max maintenue 1 s")
            else list(hi = "> 16 km/h", sprint = "> 20 km/h", set = "SoccerMon", vmax = "vitesse max")
  list(metrics = m, labels = labels)
}
