# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

# get functions used throughout this script
source(here::here("4_Characterisation","functions_characterisation.R"))


if((doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
}

if(doCharacterisation) {
  # Output folders for WP2
  output_lsc <- file.path(tempDir,"Large-scale Characterisation")
  if (!file.exists(output_lsc)){
    dir.create(output_lsc, recursive = TRUE)}
}

if(doDrugUtilisation) {
  # Output folders for WP2
  output_du <- file.path(tempDir,"Drug Utilisation")
  if (!file.exists(output_du)){
    dir.create(output_du, recursive = TRUE)}
}

if(doTreatmentPatterns) {
  output_tp <- file.path(tempDir,"Treatment Patterns")
  if (!file.exists(output_tp)){
    dir.create(output_tp, recursive = TRUE)}
  
  output_tp_settings <- file.path(tempDir, "Treatment Patterns", "settings")
  if (!file.exists(output_tp_settings)){
    dir.create(output_tp_settings, recursive = TRUE)}
}

names_final_cohorts <- read.csv(file.path(tempDir,paste0(db.name,"_cohorts.csv")))

# -------------------------------------------------------------------
if(doCharacterisation) {
  # CHARACTERISATION NO STRATA, SEX STRATA, AGE STRATA, VACCINATION STRATA, CALENDAR PERIOD STRATA
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest')
  
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(c(1:30), "all_base", BaseCohortsName, any = FALSE)
  do_lsc(c(1:60), "all_any", OverlapCohortsCName, any = FALSE)
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(1:30), "all_base", BaseCohortsName)
  do_vaccination_characterisation(c(1:60), "all_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  
  # Make code quicker by disregarding uninteresting visits
  cdm[["visit_occurrence"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(lubridate::year(.data$visit_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_hu(c(1:30), "all_base", BaseCohortsName)
  do_hu(c(1:60), "all_any", OverlapCohortsCName)
}
