# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

# get functions used throughout this script
source(here::here("4_Characterisation","functions_characterisation.R"))


if((doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
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

# CHANGE THIS AGAIN
names_final_cohorts <- read.csv(file.path(tempDir,paste0(db.name,"_cohorts.csv")))

# -------------------------------------------------------------------
if(doCharacterisation) {
  # CHARACTERISATION NO STRATA
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, no strata')
  
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(c(1:4), "all_base", BaseCohortsName)
  do_lsc(c(1:12), "all_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(1:4), "all_base", BaseCohortsName)
  do_vaccination_characterisation(c(1:12), "all_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  
  # Make code quicker by disregarding uninteresting visits
  cdm[["visit_occurrence"]] <- cdm[["visit_occurrence"]] %>%
    dplyr::filter(lubridate::year(.data$visit_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_hu(c(1:4), "all_base", BaseCohortsName)
  do_hu(c(1:12), "all_any", OverlapCohortsCName)
}

if(doDrugUtilisation) {
  # DRUG UTILISATION NO STRATA
  info(logger, '--- Looking at drug utilisation')
  
  # Make code quicker by disregarding uninteresting drug data
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(c(1:4), "all_base", BaseCohortsName)
  do_du(c(1:12), "all_any", OverlapCohortsCName)
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  for(i in c(1:4)) {
    do_tp(i, c(61:92), BaseCohortsName)
  }
  for(i in c(1:12)) {
    do_tp(i, c(181:212), OverlapCohortsCName)
  }
}


# -------------------------------------------------------------------
# CHARACTERISATION SEX STRATA
if(doCharacterisation) {
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, sex strata')
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(c(5:12), "sex_base", BaseCohortsName)
  do_lsc(c(13:36), "sex_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(5:12), "sex_base", BaseCohortsName)
  do_vaccination_characterisation(c(13:36), "sex_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(5:12), "sex_base", BaseCohortsName)
  do_hu(c(13:36), "sex_any", OverlapCohortsCName)
}

if(doDrugUtilisation) {
  # DRUG UTILISATION SEX STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(5:12), "sex_base", BaseCohortsName)
  do_du(c(13:36), "sex_any", OverlapCohortsCName)
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  for(i in c(5:12)) {
    do_tp(i, c(61:92), BaseCohortsName)
  }
  for(i in c(13:36)) {
    do_tp(i, c(181:212), OverlapCohortsCName)
  }
}

# -------------------------------------------------------------------
# CHARACTERISATION AGE STRATA

if(doCharacterisation) {
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, age strata')
  # Large scale characterisation
  do_lsc(c(13:44), "age_base", BaseCohortsName)
  do_lsc(c(37:132), "age_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(13:44), "age_base", BaseCohortsName)
  do_vaccination_characterisation(c(37:132), "age_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(13:44), "age_base", BaseCohortsName)
  do_hu(c(37:132), "age_any", OverlapCohortsCName)
}

if(doDrugUtilisation) {
  # DRUG UTILISATION AGE STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(13:44), "age_base", BaseCohortsName)
  do_du(c(37:132), "age_any", OverlapCohortsCName)
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  for(i in c(13:52)) {
    do_tp(i, c(61:92), BaseCohortsName)
  }
  for(i in c(37:132)) {
    do_tp(i, c(181:212), OverlapCohortsCName)
  }
}

# -------------------------------------------------------------------
# CHARACTERISATION CALENDAR PERIOD STRATA
if(doCharacterisation) {
  info(logger, '-- Calculating Large-scale characterisation for calendar period strata')
  # Here calendar period refers only to Delta and Omicron variants
  
  # Large scale characterisation
  do_lsc(c(53:60), "cp_base", BaseCohortsName)
  do_lsc(c(157:180), "cp_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(53:60), "cp_base", BaseCohortsName)
  do_vaccination_characterisation(c(157:180), "cp_any", OverlapCohortsCName)
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(53:60), "cp_base", BaseCohortsName)
  do_hu(c(157:180), "cp_any", OverlapCohortsCName)
}

if(doDrugUtilisation) {
  # DRUG UTILISATION CALENDAR PERIOD STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(53:60), "cp_base", BaseCohortsName)
  do_du(c(157:180), "cp_any", OverlapCohortsCName)
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  for(i in c(53:60)) {
    do_tp(i, c(61:92), BaseCohortsName)
  }
  for(i in c(157:180)) {
    do_tp(i, c(181:212), OverlapCohortsCName)
  }
}

# -------------------------------------------------------------------
# CHARACTERISATION VACCINATION STRATA
if(vaccine_data) {
  if(doCharacterisation) {
    info(logger, '-- Calculating Large-scale characterisation for vaccination strata')
    # Large scale characterisation
    do_lsc(c(45:52), "vacc_base", BaseCohortsName)
    do_lsc(c(133:156), "vacc_any", OverlapCohortsCName)
    
    info(logger, '--- Looking at vaccination outcomes for characterisation')
    # Vaccination
    do_vaccination_characterisation(c(45:52), "vacc_base", BaseCohortsName)
    do_vaccination_characterisation(c(133:156), "vacc_any", OverlapCohortsCName)
    
    info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
    # Healthcare Utilisation
    # sick leave missing
    do_hu(c(45:52), "vacc_base", BaseCohortsName)
    do_hu(c(133:156), "vacc_any", OverlapCohortsCName)
  }
  
  if(doDrugUtilisation) {
    # DRUG UTILISATION CALENDAR PERIOD STRATA
    info(logger, '--- Looking at drug utilisation')
    do_du(c(45:52), "vacc_base", BaseCohortsName)
    do_du(c(133:156), "vacc_any", OverlapCohortsCName)
  }
  
  if(doTreatmentPatterns) {
    info(logger, '-- Performing Treatment Patterns calculations')
    # Treatment Patterns
    for(i in c(45:52)) {
      do_tp(i, c(61:92), BaseCohortsName)
    }
    for(i in c(133:156)) {
      do_tp(i, c(181:212), OverlapCohortsCName)
    }
  }
  
}
