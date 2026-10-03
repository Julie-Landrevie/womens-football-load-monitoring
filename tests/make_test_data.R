#!/usr/bin/env Rscript
# =============================================================================
# make_test_data.R — Génère un jeu de données SYNTHÉTIQUE au format SoccerMon
# Sert uniquement à tester le pipeline sans télécharger les vraies données.
# Les résultats obtenus dessus n'ont AUCUNE valeur sportive.
#   Rscript tests/make_test_data.R   ->  data/test/
#   SOCCERMON_DIR=data/test Rscript run_pipeline.R
# =============================================================================
set.seed(42)
out <- "data/test"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

dates <- seq(as.Date("2020-06-01"), as.Date("2021-10-31"), by = "day")
n <- length(dates)
players <- c(sprintf("TeamA-%s", replicate(12, paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))),
             sprintf("TeamB-%s", replicate(11, paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))))

sim_player <- function(p) {
  dow <- as.integer(format(dates, "%u"))
  base <- c(450, 550, 300, 600, 250, 700, 0)[dow]            # semaine type (match le samedi)
  season <- ifelse(format(dates, "%m") %in% c("12", "01"), 0.3, 1)  # coupure hivernale
  bloc <- 1 + 0.35 * sin(seq_len(n) / 20 + runif(1, 0, 6))   # blocs de charge
  load <- pmax(0, round(base * season * bloc * rlnorm(n, 0, 0.25)))
  rep_rate <- if (runif(1) < 0.15) 0.4 else runif(1, 0.75, 0.95)  # quelques joueuses peu assidues
  load[runif(n) > rep_rate] <- NA
  if (runif(1) < 0.3) load[sample(which(!is.na(load)), 1)] <- 9999   # valeur aberrante
  ref <- stats::filter(ifelse(is.na(load), 0, load), rep(1/7, 7), sides = 1)
  fatigue_signal <- as.numeric(scale(ifelse(is.na(ref), 0, ref)))
  w <- function(mu) {
    x <- round(mu - 0.45 * fatigue_signal + rnorm(n, 0, 0.6))
    x <- pmin(5, pmax(1, x)); x[runif(n) > rep_rate - 0.05] <- NA; x
  }
  list(load = load, fatigue = w(3.4), mood = w(3.6), sleep_quality = w(3.5),
       soreness = w(3.3), stress = w(3.6),
       readiness = { x <- pmin(10, pmax(0, round(6.5 - fatigue_signal + rnorm(n, 0, 1))));
                     x[runif(n) > rep_rate - 0.05] <- NA; x },
       sleep_duration = round(rnorm(n, 7.6, 0.8), 2))
}
sims <- setNames(lapply(players, sim_player), players)

write_var <- function(name, getter) {
  df <- data.frame(Date = format(dates, "%d.%m.%Y"), check.names = FALSE)
  for (p in players) df[[p]] <- getter(sims[[p]])
  write.csv(df, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}
# Comme dans SoccerMon : un jour sans déclaration de charge vaut 0
write_var("daily_load", function(s) ifelse(is.na(s$load), 0, s$load))
for (v in c("fatigue", "mood", "readiness", "sleep_duration", "sleep_quality", "soreness", "stress"))
  write_var(v, function(s) s[[v]])
# ACWR "fourni" (moyennes glissantes), pour tester la vérification de cohérence
write_var("acwr", function(s) {
  l <- ifelse(is.na(s$load) | s$load > 5000, 0, s$load)
  a <- stats::filter(l, rep(1/7, 7), sides = 1); c <- stats::filter(l, rep(1/28, 28), sides = 1)
  round(ifelse(c > 0, a / c, NA), 3)
})

inj <- do.call(rbind, lapply(players, function(p) {
  k <- rpois(1, 2); if (k == 0) return(NULL)
  data.frame(player_name = p, type = sample(c("minor", "major"), k, TRUE, prob = c(.75, .25)),
             timestamp = format(sample(dates[60:n], k), "%Y-%m-%d 08:00:00"))
}))
write.csv(inj, file.path(out, "injuries.csv"), row.names = FALSE)
message("Jeu de test écrit dans ", out, " (", length(players), " joueuses, ", n, " jours)")
