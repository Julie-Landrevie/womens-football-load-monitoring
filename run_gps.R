#!/usr/bin/env Rscript
# =============================================================================
# run_gps.R — Agrège les données GPS SoccerMon (à lancer UNE fois, ~10-30 min)
#
#   Rscript run_gps.R ~/Desktop/SoccerMon_GPS
#
# Lit les ~10 000 fichiers parquet (un par joueuse et par jour) sans rien
# copier, et écrit deux petites tables dans data/processed/ :
#   - gps_sessions.csv : une ligne par fichier (sert aussi de cache : en cas
#                        d'interruption, relancer la commande reprend là où
#                        elle s'était arrêtée)
#   - gps_daily.csv    : une ligne par joueuse et par jour, utilisée ensuite
#                        par run_pipeline.R et le tableau de bord
# =============================================================================

local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) setwd(dirname(normalizePath(sub("^--file=", "", a))))
})
suppressPackageStartupMessages({ library(dplyr); library(readr) })
source("R/06_gps.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
gps_dir <- if (length(args)) path.expand(args[1]) else Sys.getenv("SOCCERMON_GPS_DIR", "")
if (gps_dir == "") stop("Indique le dossier GPS : Rscript run_gps.R ~/Desktop/SoccerMon_GPS")

message("Agrégation GPS : ", gps_dir)
sessions <- process_gps(gps_dir)
res <- gps_daily_from_sessions(sessions)
write_csv(res$daily, "data/processed/gps_daily.csv")
write_csv(res$qc, "data/processed/gps_qc.csv")
print(as.data.frame(res$qc), right = FALSE)
message("-> data/processed/gps_daily.csv (", nrow(res$daily), " jours-joueuses)")
message("Relance maintenant : Rscript run_pipeline.R")
