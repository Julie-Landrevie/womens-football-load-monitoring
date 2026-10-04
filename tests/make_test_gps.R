#!/usr/bin/env Rscript
# =============================================================================
# make_test_gps.R — Fichiers GPS SYNTHÉTIQUES au format SoccerMon (en CSV),
# pour tester run_gps.R sans les 99 Go de données réelles.
#   Rscript tests/make_test_gps.R      ->  data/test_gps/
# Reproduit les particularités réelles : 10 lignes par instant (accéléromètre
# 100 Hz), trous de signal, positions à 0 sans satellite.
# =============================================================================
set.seed(7)
out <- "data/test_gps"
players <- names(read.csv("data/test/daily_load.csv", check.names = FALSE))[-1]
dates <- seq(as.Date("2020-06-01"), as.Date("2021-10-31"), by = "day")

session <- function(minutes) {
  n <- minutes * 600                              # 10 Hz
  phase <- cumsum(rexp(n, 1 / 150) > 600) %% 4    # alternance de blocs
  base <- c(0.8, 1.6, 3.5, 5.8)[phase + 1]
  speed <- pmax(0, stats::filter(base + rnorm(n, 0, 0.4), rep(1 / 20, 20), circular = TRUE))
  sec <- 8 * 3600 + seq(0, by = 0.1, length.out = n)
  keep <- rep(TRUE, n); for (g in sample(n - 30, 5)) keep[g:(g + 15)] <- FALSE   # trous
  sec <- sec[keep]; speed <- speed[keep]
  lat <- ifelse(runif(length(sec)) < 0.01, 0, 63.45)
  hh <- sec %/% 3600; mm <- (sec %% 3600) %/% 60; ss <- sec %% 60
  time <- sprintf("%02d:%02d:%04.1f", hh, mm, ss)
  df <- data.frame(time = rep(time, each = 10), speed = rep(round(speed, 3), each = 10),
                   lat = rep(lat, each = 10), accl_x = rnorm(10 * length(time)))
  df
}
k <- 0
for (p in players) {
  for (d in sample(dates, 6)) {
    d <- as.Date(d, origin = "1970-01-01")
    dir <- file.path(out, format(d, "%Y"), format(d, "%Y-%m"), format(d, "%Y-%m-%d"))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(session(sample(c(30, 45, 60), 1)),
              file.path(dir, paste0(format(d, "%Y-%m-%d"), "-", p, ".csv")), row.names = FALSE)
    k <- k + 1
  }
}
message("Fichiers GPS de test : ", k, " dans ", out)
