#!/usr/bin/env Rscript
# =============================================================================
# run_pipeline.R — Exécute tout le pipeline
#   Rscript run_pipeline.R                 # données dans data/raw
#   SOCCERMON_DIR=/chemin Rscript run_pipeline.R
# Sorties : outputs/tables/*.csv, outputs/figures/*.png, outputs/rapport_charge.html
# =============================================================================

local({
  here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(here)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) here <- dirname(normalizePath(sub("^--file=", "", a)))
  }
  if (!is.null(here)) setwd(here)
})

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")

out_tab <- file.path(CONFIG$output_dir, "tables")
out_fig <- file.path(CONFIG$output_dir, "figures")
dir.create(out_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

message("1/5 Chargement des données : ", CONFIG$data_dir)
daily_long <- load_daily()
injuries   <- load_injuries()

message("2/5 Contrôle qualité")
completeness <- qc_completeness(daily_long)
out_of_range <- qc_out_of_range(daily_long)
duplicates   <- qc_duplicates(daily_long)
zero_runs    <- qc_zero_runs(daily_long)
summary_qc   <- qc_summary(daily_long, completeness, out_of_range, duplicates, zero_runs)
print(as.data.frame(summary_qc), right = FALSE)
wide <- daily_long |> clean_values(out_of_range) |> to_wide()

message("3/5 Indicateurs de charge et wellness")
metrics <- wide |>
  compute_load_metrics() |>
  compute_wellness_z() |>
  compute_flags() |>
  compute_base_level()
# Charge externe (GPS) : table journalière produite une fois par run_gps.R
gps_daily <- load_gps_daily()
gps_qc <- NULL
if (!is.null(gps_daily)) {
  message("   GPS : ", nrow(gps_daily), " jours-joueuses (data/processed/gps_daily.csv)")
  metrics <- metrics |>
    left_join(gps_daily |> select(player, date, total_km, hsr_m, sprint_m, vmax_kmh, n_acc, n_dec),
              by = c("player", "date"))
  if (file.exists("data/processed/gps_qc.csv")) gps_qc <- read_csv("data/processed/gps_qc.csv", show_col_types = FALSE)
} else {
  message("   GPS : pas de data/processed/gps_daily.csv (lancer run_gps.R pour l'ajouter)")
}
# ACWR de la charge externe (course > 16 km/h, sprint > 20 km/h), en m/jour
metrics  <- compute_external_acwr(metrics)
exposure <- exposure_summary(metrics)
message("   Ratio et valeurs absolues (chiffres du README, résultat 6) :")
for (k in names(exposure)) message("     ", k, " = ", paste(exposure[[k]], collapse = " / "))
consistency <- qc_consistency(metrics)
weekly      <- weekly_summary(metrics)
inj_win     <- injury_windows(metrics, injuries)

alerts <- metrics |>
  filter(n_flags > 0) |>
  transmute(team, joueuse = label, player, date,
            acwr_ewma = round(acwr_ewma, 2),
            charge_aigue_UA_j = round(acute_ewma), charge_chronique_UA_j = round(chronic_ewma),
            ecart_aigu_chronique = sprintf("%+d %%", round(100 * (acwr_ewma - 1))),
            base_vs_normale = ifelse(is.na(base_rel), NA, sprintf("%d %%", as.integer(floor(100 * base_rel)))),
            contexte = alert_context(flag_acwr_high, flag_acwr_low, base_level, base_rel),
            monotony = round(monotony_calc, 2),
            wellness_z = round(wellness_z, 2),
            motifs = paste0(
              ifelse(flag_acwr_high, "charge en hausse rapide; ", ""),
              ifelse(flag_acwr_low,  "sous-charge; ", ""),
              ifelse(flag_monotony,  "monotonie élevée; ", ""),
              ifelse(flag_wellness,  "wellness en baisse; ", "")) |> sub("; $", "", x = _))

message("4/5 Export des tables et figures")
write_csv(distinct(daily_long, label, team, player) |> arrange(label) |> rename(identifiant_soccermon = player),
          file.path(out_tab, "correspondance_joueuses.csv"))
write_csv(summary_qc,   file.path(out_tab, "qc_synthese.csv"))
write_csv(completeness, file.path(out_tab, "qc_completude.csv"))
write_csv(out_of_range, file.path(out_tab, "qc_valeurs_suspectes.csv"))
write_csv(zero_runs,    file.path(out_tab, "qc_plages_sans_charge.csv"))
write_csv(metrics,      file.path(out_tab, "indicateurs_journaliers.csv"))
write_csv(weekly,       file.path(out_tab, "synthese_hebdomadaire.csv"))
write_csv(alerts,       file.path(out_tab, "points_attention.csv"))
write_csv(tibble::enframe(lapply(exposure, paste, collapse = " / "), name = "indicateur", value = "valeur") |>
            mutate(valeur = unlist(valeur)),
          file.path(out_tab, "ratio_valeurs_absolues.csv"))

save_fig <- function(p, name, w = 9, h = 6) {
  if (!is.null(p)) ggsave(file.path(out_fig, name), p, width = w, height = h, dpi = 150, bg = "white")
}
save_fig(fig_qc_completeness(completeness), "01_qc_completude.png", h = 10)
for (tm in sort(unique(weekly$team))) {
  save_fig(fig_team_weekly(weekly, tm), paste0("02_equipe_", sub("^.* ", "", tm), ".png"), w = 11, h = 7)
}
# Joueuses d'exemple : celles avec la meilleure couverture de charge
example_players <- completeness |>
  filter(variable == "daily_load (> 0)") |>
  group_by(team) |> slice_max(completeness, n = 1, with_ties = FALSE) |> pull(player)
for (pl in example_players) {
  save_fig(fig_player_load(metrics, pl), paste0("03_charge_", player_label(metrics, pl), ".png"), h = 6.5)
  save_fig(fig_player_wellness(metrics, pl, injuries), paste0("04_wellness_", player_label(metrics, pl), ".png"), h = 4.5)
}
save_fig(fig_injury_profile(inj_win), "05_profil_pre_blessure.png", h = 4.5)
save_fig(fig_internal_external(metrics), "06_interne_externe.png", w = 10, h = 5)
save_fig(fig_exposure(metrics), "08_ratio_et_base.png", w = 10, h = 5.5)
for (pl in example_players) save_fig(fig_player_gps(metrics, pl), paste0("07_gps_", player_label(metrics, pl), ".png"), h = 6)

saveRDS(list(daily_long = daily_long, injuries = injuries, metrics = metrics,
             completeness = completeness, out_of_range = out_of_range,
             duplicates = duplicates, summary_qc = summary_qc, zero_runs = zero_runs,
             consistency = consistency, weekly = weekly, alerts = alerts,
             inj_win = inj_win, example_players = example_players, gps_qc = gps_qc,
             exposure = exposure,
             config = CONFIG, data_dir = CONFIG$data_dir),
        file.path(CONFIG$output_dir, "pipeline.rds"))

# Figures du README (docs/figures), régénérées à chaque exécution
dir.create("docs/figures", recursive = TRUE, showWarnings = FALSE)
save_doc <- function(p, name, w, h) if (!is.null(p)) ggsave(file.path("docs/figures", name), p, width = w, height = h, dpi = 150, bg = "white")
save_doc(fig_player_load(metrics, example_players[1]), "suivi_joueuse.png", 9, 6.5)
save_doc(fig_team_weekly(weekly, sort(unique(weekly$team))[1]), "charge_hebdo_equipe.png", 11, 7)
save_doc(fig_injury_profile(inj_win), "profil_pre_blessure.png", 9, 4.5)
save_doc(fig_qc_completeness(completeness), "qc_completude.png", 9, 10)
save_doc(fig_internal_external(metrics), "interne_externe.png", 10, 5)
save_doc(fig_exposure(metrics), "ratio_et_base.png", 10, 5.5)

message("   Tableau de bord web")
export_dashboard(readRDS(file.path(CONFIG$output_dir, "pipeline.rds")), out_dir = "docs")

message("5/5 Rapport HTML")
# Pandoc : si absent du PATH, on le cherche dans RStudio / Homebrew
if (requireNamespace("rmarkdown", quietly = TRUE) && !rmarkdown::pandoc_available()) {
  candidates <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/MacOS/pandoc",
    "/opt/homebrew/bin", "/usr/local/bin")
  hit <- candidates[file.exists(file.path(candidates, "pandoc"))][1]
  if (!is.na(hit)) {
    Sys.setenv(RSTUDIO_PANDOC = hit)
    rmarkdown::find_pandoc(cache = FALSE)
    message("   Pandoc trouvé : ", hit)
  }
}
if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
  rds_path <- normalizePath(file.path(CONFIG$output_dir, "pipeline.rds"))
  out_dir  <- normalizePath(CONFIG$output_dir)
  rmarkdown::render("report/rapport_charge.Rmd",
                    output_dir = out_dir, quiet = TRUE,
                    params = list(results = rds_path))
  message("   -> ", file.path(CONFIG$output_dir, "rapport_charge.html"))
} else {
  message("   Pandoc introuvable : rapport non généré.",
          "\n   -> Installe RStudio (posit.co/download/rstudio-desktop) ou Pandoc",
          "\n      (github.com/jgm/pandoc/releases, fichier arm64 .pkg), puis relance.",
          "\n   Les tables et figures sont déjà dans outputs/.")
}
message("Terminé.")
