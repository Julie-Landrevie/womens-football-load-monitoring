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

GPS_CONFIG <- list(
  hsr_speed      = 4.44,  # m/s = 16 km/h : course à haute intensité (seuils SoccerMon)
  sprint_speed   = 5.55,  # m/s = 20 km/h : sprint
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
process_gps <- function(dir, cache_file = "data/processed/gps_sessions.csv",
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
    res <- parallel::mclapply(seq_len(nrow(rows)), function(i) {
      out <- tryCatch(gps_summarise_df(gps_read_file(rows$path[i])),
                      error = function(e) tibble(error = conditionMessage(e)))
      if (is.null(out)) out <- tibble(error = "fichier vide")
      bind_cols(rows[i, c("file", "date", "player")], out)
    }, mc.cores = cores)
    res <- bind_rows(res)
    write_csv(res, cache_file, append = file.exists(cache_file))
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
              .groups = "drop") |>
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
