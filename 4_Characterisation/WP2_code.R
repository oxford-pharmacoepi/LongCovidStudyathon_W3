# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

# get functions used throughout this script
source(here::here("4_Characterisation","functions_characterisation.R"))



if ((doCharacterisation || doClustering) && doTrajectories && vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && doTrajectories && !vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories && vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories && !vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories && vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories && !vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories && vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories && !vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
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

if(vaccine_data) {
  cohorts_interest_base <- c(1:60)
  cohorts_interest_any <- c(1:180)
} else {
  cohorts_interest_base <- c(1:44, 53:60)
  cohorts_interest_any <- c(1:132, 157:180)
}

if(doCharacterisation) {
  # CHARACTERISATION NO STRATA, SEX STRATA, AGE STRATA, VACCINATION STRATA, CALENDAR PERIOD STRATA
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest')
  
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_lsc(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  if(vaccine_data) {
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(cohorts_interest_base, "all_base", BaseCohortsName)
  do_vaccination_characterisation(cohorts_interest_any, "all_any", OverlapCohortsCName)
  }
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  
  # Make code quicker by disregarding uninteresting visits
  cdm[["visit_occurrence"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(lubridate::year(.data$visit_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_hu(cohorts_interest_base, "all_base", BaseCohortsName)
  do_hu(cohorts_interest_any, "all_any", OverlapCohortsCName)
}

if(doDrugUtilisation) {
  # DRUG UTILISATION NO STRATA
  info(logger, '--- Looking at drug utilisation')
  
  # Make code quicker by disregarding uninteresting drug data
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_du(cohorts_interest_any, "all_any", OverlapCohortsCName)
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  for(i in cohorts_interest_base) {
    do_tp(i, c(61:92), BaseCohortsName)
  }
  for(i in cohorts_interest_any) {
    do_tp(i, c(181:212), OverlapCohortsCName)
  }
}