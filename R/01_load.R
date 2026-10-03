# =============================================================================
# 01_load.R — Chargement des données SoccerMon (format "par variable")
#
# Format attendu (dépôt officiel github.com/simula/soccermon) :
#   - un CSV par variable journalière : daily_load.csv, fatigue.csv, ...
#     colonne 1 = "Date" (jj.mm.aaaa), colonnes suivantes = une par joueuse
#     (identifiant préfixé par l'équipe, ex. "TeamA-...")
#   - injuries.csv : player_name, type, timestamp
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(stringr)
})

#' Trouve tous les fichiers portant ce nom sous `dir` (ex. un par équipe)
find_files <- function(dir, name) {
  list.files(dir, pattern = paste0("^", name, "$"),
             recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
}

parse_dates <- function(x, formats = CONFIG$date_formats) {
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  for (f in formats) {
    todo <- is.na(out)
    if (!any(todo)) break
    out[todo] <- as.Date(x[todo], format = f)
  }
  out
}

#' Lit un CSV "large" (Date x joueuses) et le passe au format long
read_wide_variable <- function(path, variable) {
  raw <- read_csv(path, show_col_types = FALSE, progress = FALSE,
                  col_types = cols(.default = col_character()))
  names(raw)[1] <- "Date"
  raw |>
    pivot_longer(-Date, names_to = "player", values_to = "value_raw") |>
    mutate(
      date     = parse_dates(Date),
      value    = suppressWarnings(as.numeric(value_raw)),
      # trace des valeurs non numériques (pour le contrôle qualité)
      non_numeric = !is.na(value_raw) & value_raw != "" & is.na(value),
      variable = variable
    ) |>
    select(player, date, variable, value, non_numeric)
}

#' Charge toutes les variables journalières disponibles
#' @return tibble long : player, team, date, variable, value, non_numeric
load_daily <- function(dir = CONFIG$data_dir, vars = DAILY_VARS) {
  if (!dir.exists(dir)) stop("Dossier introuvable : ", dir,
                             "\nTélécharge SoccerMon (voir README) ou définis SOCCERMON_DIR.")
  found <- set_names(map(vars, ~ find_files(dir, paste0(.x, "\\.csv"))), vars)
  missing <- names(found)[lengths(found) == 0]
  if (length(missing)) message("  Variables absentes (ignorées) : ",
                               paste(missing, collapse = ", "))
  if (length(found$daily_load) == 0) stop("daily_load.csv est indispensable.")

  found <- found[lengths(found) > 0]
  message("  ", length(found), " variables journalières chargées (",
          sum(lengths(found)), " fichiers)")
  long <- imap_dfr(found, function(paths, v) map_dfr(paths, read_wide_variable, variable = v))
  labels <- make_player_labels(long)
  long |>
    left_join(labels, by = "player") |>
    relocate(team, label, .after = player)
}

#' Noms d'affichage lisibles : "Équipe A" et joueuses "A-01", "A-02"...
#' Numérotation dans l'ordre d'arrivée (premier jour avec une charge > 0), puis
#' par identifiant. L'identifiant SoccerMon d'origine reste dans la colonne
#' `player` et la correspondance est exportée (correspondance_joueuses.csv).
make_player_labels <- function(long) {
  long |>
    distinct(player) |>
    mutate(team_code = str_match(player, "^Team([A-Za-z])")[, 2] |> coalesce("X")) |>
    left_join(long |>
                filter(variable == "daily_load", !is.na(value), value > 0) |>
                group_by(player) |> summarise(first_day = min(date), .groups = "drop"),
              by = "player") |>
    arrange(team_code, first_day, player) |>
    group_by(team_code) |>
    mutate(label = sprintf("%s-%02d", team_code, row_number())) |>
    ungroup() |>
    transmute(player, team = paste("Équipe", team_code), label)
}

#' Convertit un horodatage dont le format n'est pas connu à l'avance :
#' ISO (2020-06-24...), européen (24.06.2020 ...), ou epoch (secondes / ms)
parse_timestamp <- function(x) {
  x <- trimws(as.character(x))
  d <- parse_dates(substr(x, 1, 10),
                   formats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d", "%m/%d/%Y"))
  num <- suppressWarnings(as.numeric(x))
  epoch <- is.na(d) & !is.na(num) & num > 1e8
  if (any(epoch)) {
    secs <- ifelse(num[epoch] > 1e11, num[epoch] / 1000, num[epoch])
    d[epoch] <- as.Date(as.POSIXct(secs, origin = "1970-01-01", tz = "UTC"))
  }
  d
}

empty_injuries <- function() {
  tibble(player = character(), date = as.Date(character()), type = character())
}

#' Charge les déclarations de blessure (si présentes). Ne bloque jamais le
#' pipeline : en cas de format inattendu, la section blessures est désactivée.
load_injuries <- function(dir = CONFIG$data_dir) {
  paths <- find_files(dir, "injur(y|ies)\\.csv")
  if (length(paths) == 0) {
    message("  injuries.csv absent : section blessures désactivée")
    return(empty_injuries())
  }
  raw <- map_dfr(paths, ~ read_csv(.x, show_col_types = FALSE,
                                   col_types = cols(.default = col_character())))
  message("  Fichier blessures : colonnes = ", paste(names(raw), collapse = ", "))
  if (nrow(raw)) message("  Exemple de date brute : ", raw[[ncol(raw)]][1])

  col_player <- intersect(c("player_name", "player", "name"), names(raw))[1]
  col_time   <- intersect(c("timestamp", "date", "Date", "time"), names(raw))[1]
  col_type   <- intersect(c("type", "severity"), names(raw))[1]
  if (is.na(col_player) || is.na(col_time)) {
    message("  ! colonnes joueuse/date non reconnues : section blessures désactivée")
    return(empty_injuries())
  }
  out <- raw |>
    transmute(player = .data[[col_player]],
              date   = parse_timestamp(.data[[col_time]]),
              type   = if (!is.na(col_type)) .data[[col_type]] else NA_character_)
  n_bad <- sum(is.na(out$date))
  if (n_bad) message("  ! ", n_bad, " date(s) de blessure illisible(s) ignorée(s) sur ", nrow(out))
  out |> filter(!is.na(date)) |> distinct()
}

#' Format "une ligne par joueuse et par jour"
to_wide <- function(daily_long) {
  daily_long |>
    select(player, team, label, date, variable, value) |>
    pivot_wider(names_from = variable, values_from = value,
                values_fn = ~ mean(.x, na.rm = TRUE)) |>   # doublons éventuels
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))) |>
    arrange(player, date)
}
