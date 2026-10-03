# Monitoring de la charge en football féminin d'élite

Pipeline R de **suivi de la charge interne et de l'état de forme** de joueuses, construit
sur le jeu de données ouvert [SoccerMon](https://www.nature.com/articles/s41597-024-03386-x)
(deux équipes féminines de première division norvégienne, saisons 2020–2021).

L'objectif est de reproduire le travail quotidien d'un sport scientist :
**vérifier la donnée → calculer les indicateurs → les rendre lisibles pour le staff →
poser une question de recherche appliquée.**

![Suivi de charge d'une joueuse](docs/figures/suivi_joueuse.png)

## Résultats sur SoccerMon (50 joueuses, 2 équipes, 2020–2021)

**1. Une limite du jeu de données repérée avant toute interprétation.**
Dans SoccerMon, un jour sans déclaration de charge vaut 0 : la charge n'est jamais
« manquante », et un repos est indiscernable d'un questionnaire non rempli. Le
pipeline restreint donc chaque joueuse à sa période active et ne déclenche une
alerte que si au moins 70 % des 28 derniers jours sont documentés (charge > 0 ou
wellness rempli). Sur la période active, la part médiane de jours avec une charge
> 0 est de 42 %, et la complétude médiane du wellness de 48 %.

**2. Des indicateurs validés.**
L'ACWR recalculé reproduit celui fourni dans SoccerMon (corrélation 1,00 ; 98 % des
21 490 jours-joueuses à moins de 0,1 d'écart). La méthode d'origine (moyennes
glissantes 7 j / 28 j, jours sans déclaration à 0) est ainsi confirmée.

**3. Des alertes lues en contexte.**
Lors d'une coupure ou d'une reprise planifiée, toute l'équipe sort de la zone
d'ACWR en même temps. En ne signalant une joueuse que lorsqu'elle s'écarte de
la médiane de son équipe, la part de jours en « sous-charge » passe de **19 % à
13 %**. Les alertes restantes correspondent à des décrochages individuels
(absence probable) plutôt qu'au calendrier collectif.

![Charge hebdomadaire par joueuse](docs/figures/charge_hebdo_equipe.png)

**4. Piste de recherche : que se passe-t-il avant une blessure déclarée ?**
Sur 155 épisodes, l'ACWR médian augmente légèrement dans les 21 jours précédant
la déclaration (≈ 0,95 → 1,07) tout en restant dans la zone de référence, et le
wellness composite ne montre pas de baisse. **Dans ces données, le questionnaire
wellness ne semble pas anticiper les plaintes déclarées.** Résultat descriptif,
sans groupe témoin, sur des blessures auto-déclarées : il ne permet aucune
conclusion causale (voir *Limites*).

![Profil avant blessure](docs/figures/profil_pre_blessure.png)

## Ce que fait le pipeline

| Étape | Contenu | Fichier |
|---|---|---|
| Chargement | lecture des CSV SoccerMon (une colonne par joueuse), passage au format long, détection automatique des équipes | `R/01_load.R` |
| Contrôle qualité | complétude par joueuse et variable, valeurs hors bornes ou non numériques, doublons, cohérence entre l'ACWR fourni et l'ACWR recalculé | `R/02_quality.R` |
| Indicateurs | sRPE, charge aiguë/chronique (glissante et EWMA), ACWR, monotonie, strain, wellness en z-score individuel, points d'attention | `R/03_load_metrics.R` |
| Visualisation | vue qualité, vue équipe (charge hebdomadaire), vue joueuse (charge + ACWR, wellness), profil pré-blessure | `R/04_figures.R` |
| Rapport | rapport HTML autonome à destination du staff | `report/rapport_charge.Rmd` |

Tous les seuils (fenêtres, zone d'ACWR, monotonie, seuil wellness, couverture
minimale) sont réglables dans `R/00_config.R`.

## Choix méthodologiques

- **Jours sans déclaration** : valent 0 dans SoccerMon. Analyse limitée à la
  période active de chaque joueuse ; un jour est « documenté » s'il a une charge > 0
  ou un wellness rempli ; **aucune alerte sous 70 % de jours documentés** sur 28 j.
- **Contexte équipe** : une alerte d'ACWR n'est émise que si la joueuse sort de la
  zone alors que la médiane de son équipe y reste (pas d'alerte pendant une
  coupure ou une reprise collective).
- **Valeurs suspectes** : mises à NA pour le calcul, jamais corrigées en silence, et
  listées dans `outputs/tables/qc_valeurs_suspectes.csv`.
- **ACWR en EWMA** (Williams et al., 2017) plutôt qu'en moyennes glissantes seules,
  car il réagit moins aux pics isolés. Les deux versions sont calculées.
- **Wellness individualisé** : chaque item est comparé aux 28 jours *précédents* de la
  joueuse (sans le jour courant). Une joueuse qui note toujours 3/5 n'est pas
  comparée à une joueuse qui note toujours 5/5.
- **Points d'attention, pas prédictions** : l'ACWR est débattu dans la littérature
  (Impellizzeri et al., 2020). Il sert ici de repère de progressivité de la charge.

## Lancer le projet

### 1. Prérequis
R ≥ 4.2 et les packages :
```r
install.packages(c("dplyr", "tidyr", "readr", "purrr", "stringr",
                   "ggplot2", "scales", "rmarkdown", "knitr"))
```

### 2. Données
Télécharger **uniquement la partie subjective** de SoccerMon (environ 10 Mo ; la partie
GPS fait environ 92 Go) depuis Zenodo, DOI
[10.5281/zenodo.10033832](https://doi.org/10.5281/zenodo.10033832), puis décompresser
l'archive dans `data/raw/`. Le chargeur cherche les fichiers (`daily_load.csv`,
`fatigue.csv`, …) dans tous les sous-dossiers.

### 3. Exécution
```bash
Rscript run_pipeline.R
# ou avec un autre dossier de données :
SOCCERMON_DIR=/chemin/vers/soccermon Rscript run_pipeline.R
```
Sorties : `outputs/rapport_charge.html`, `outputs/figures/`, `outputs/tables/`.

### Tester sans les données
```bash
Rscript tests/make_test_data.R            # jeu SYNTHÉTIQUE au format SoccerMon
SOCCERMON_DIR=data/test Rscript run_pipeline.R
```
Les résultats obtenus sur ce jeu de test n'ont aucune valeur sportive. Il sert
uniquement à vérifier que le code tourne.

## Limites et suites

- Blessures **auto-déclarées** (douleur mineure ou majeure), sans diagnostic médical.
- Analyse pré-blessure **descriptive**. Étape suivante : comparer avec des fenêtres
  sans blessure, avec un modèle mixte (effet aléatoire joueuse).
- Charge **externe** (GPS STATSports 10 Hz) : extension prévue (distance totale,
  haute intensité, accélérations), puis croisement charge interne / charge externe.
- Tableau de bord interactif (Shiny ou Power BI) à partir des tables exportées.

## Références

- Midoglu C. et al. (2024). *A large-scale multivariate soccer athlete health, performance, and position monitoring dataset.* Scientific Data, 11, 553.
- Foster C. (1998). Monitoring training in athletes with reference to overtraining syndrome. *MSSE*.
- Foster C. et al. (2001). A new approach to monitoring exercise training. *JSCR*.
- Williams S. et al. (2017). Better way to determine the acute:chronic workload ratio? *BJSM*.
- Gabbett T. (2016). The training–injury prevention paradox. *BJSM*.
- Impellizzeri F. et al. (2020). Acute:chronic workload ratio: conceptual issues and fundamental pitfalls. *IJSPP*.

---
Julie Landrevie · [Portfolio](https://bit.ly/julie-landrevie-notion) · [LinkedIn](https://www.linkedin.com/in/julie-landrevie/)
