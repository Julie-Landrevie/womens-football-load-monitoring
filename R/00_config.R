# =============================================================================
# 00_config.R — Paramètres du pipeline
# Tous les seuils sont réglables ici : ce sont des points d'attention pour le
# staff, pas des prédicteurs de blessure (cf. README, section Limites).
# =============================================================================

CONFIG <- list(
  # Dossier contenant les données SoccerMon (partie subjective) téléchargées
  # depuis Zenodo (DOI 10.5281/zenodo.10033832). Le chargeur cherche les
  # fichiers récursivement, l'arborescence exacte importe peu.
  data_dir   = Sys.getenv("SOCCERMON_DIR", "data/raw"),
  output_dir = "outputs",

  # Format de date des CSV SoccerMon (colonne "Date", ex. 24.06.2020)
  date_formats = c("%d.%m.%Y", "%Y-%m-%d", "%d/%m/%Y"),

  # Fenêtres de calcul (jours)
  acute_window   = 7,
  chronic_window = 28,

  # ACWR : zone de référence souvent citée (Gabbett 2016). Débattue dans la
  # littérature (Impellizzeri et al. 2020) : on l'utilise comme repère visuel.
  acwr_low  = 0.8,
  acwr_high = 1.5,

  # Monotonie (Foster 1998) : > 2 = peu de variation de charge sur la semaine
  monotony_high = 2,

  # Limiter la "fatigue d'alerte" (un staff qui reçoit trop d'alertes ne les
  # lit plus) :
  #  - persistance : une alerte de charge (ACWR, monotonie) n'est émise
  #    qu'après N jours consécutifs hors zone ;
  #  - écart à l'équipe : l'ACWR de la joueuse doit s'écarter d'au moins
  #    `team_margin` de la médiane de son équipe ce jour-là ;
  #  - les alertes sont comptées en épisodes (jours consécutifs = 1 épisode).
  alert_persistence = 3,
  team_margin       = 0.2,

  # Wellness : z-score individuel (référence glissante propre à la joueuse)
  wellness_baseline_window = 28,
  wellness_baseline_min    = 10,   # nb min de jours pour établir la référence
  wellness_z_alert         = -1.5,

  # Qualité des données : couverture minimale des 28 derniers jours pour
  # considérer l'ACWR comme interprétable
  min_coverage_chronic = 0.7,

  # Valeurs absolues à côté du ratio : un même ACWR peut correspondre à des
  # expositions très différentes. La charge chronique est comparée à la
  # « normale » de la joueuse (médiane de sa charge chronique sur l'année
  # précédente, jours interprétables uniquement).
  base_window   = 365,
  base_min_days = 28,     # nb min de jours pour établir la normale
  base_low      = 0.75,   # base « basse » : chronique < 75 % de sa normale
  base_high     = 1.25,   # base « haute » : chronique > 125 % de sa normale

  # ACWR GPS (course > 16 km/h, sprint > 20 km/h) : part minimale des jours
  # actifs des 28 derniers jours (charge > 0 ou GPS) qui ont un fichier GPS
  min_coverage_gps = 0.7,

  # Bornes plausibles par variable (contrôle qualité). Échelles d'après
  # Midoglu et al. 2024 (Scientific Data), tableau 1.
  valid_ranges = list(
    daily_load     = c(0, 3000),   # UA (sRPE = RPE x minutes, cumul journalier)
    fatigue        = c(1, 5),
    mood           = c(1, 5),
    readiness      = c(0, 10),
    sleep_duration = c(0, 16),     # heures
    sleep_quality  = c(1, 5),
    soreness       = c(1, 5),
    stress         = c(1, 5)
  ),

  # Sens des échelles wellness (TRUE = une valeur haute est favorable).
  # Dans l'application PMSys, 5 = état le plus favorable pour ces items :
  # À VÉRIFIER sur la documentation PMSys avant toute interprétation.
  wellness_higher_is_better = c(
    fatigue = TRUE, mood = TRUE, readiness = TRUE, sleep_quality = TRUE,
    soreness = TRUE, stress = TRUE
  )
)

# Variables journalières (un CSV par variable, colonnes = joueuses)
DAILY_VARS <- c("daily_load", "atl", "weekly_load", "monotony", "strain",
                "acwr", "ctl28", "ctl42",
                "fatigue", "mood", "readiness", "sleep_duration",
                "sleep_quality", "soreness", "stress")

WELLNESS_VARS <- c("fatigue", "mood", "readiness", "sleep_quality",
                   "soreness", "stress")
