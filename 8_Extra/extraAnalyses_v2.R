# Read all cdm cohorts
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

# EXTRA COHORT: Infection + Any LC symptom with index date at symptom event
if(!onlyLC) {
  do_overlap_LCany(cdm, c(1), c(5:29), c(210), indexsymptom = TRUE)
  
  names_final_cohorts <- dplyr::tibble(table_name = OverlapCohortsName,
                                       cohort_definition_id = 210, cohort_name = "Any LC symptom index date symptom")
  
  write.csv(names_final_cohorts, here::here(tempDir, "name_extra_cohort.csv"))
}

# Output folders
output_lsc <- file.path(tempDir,"Large-scale Characterisation")
if (!file.exists(output_lsc)){
  dir.create(output_lsc, recursive = TRUE)}

if(!onlyLC) {
  output_du <- file.path(tempDir,"Drug Utilisation")
  if (!file.exists(output_du)){
    dir.create(output_du, recursive = TRUE)}
}

# Repeat some LSC an DU calculations
if(onlyLC) {
  source(here::here("4_Characterisation","functions_characterisation.R"))
  do_lsc(c(1:30), "all_base", BaseCohortsName, any = FALSE)
  do_lsc(c(1:60), "all_any", OverlapCohortsCName, any = FALSE)
  
} else if (sql_server) {
  source(here::here("4_Characterisation","functions_characterisation_sql.R"))
  if(vaccine_data) {
    cohorts_interest_base <- c(1:60)
    cohorts_interest_any <- c(1:180)
  } else {
    cohorts_interest_base <- c(1:44, 53:60)
    cohorts_interest_any <- c(1:132, 157:180)
  }
  
  do_lsc(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_lsc(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_du(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  
  # Extra drug utilisation cohort analysis
  do_du(210, "any_extra", OverlapCohortsCName, any = FALSE)
  
} else {
  source(here::here("4_Characterisation","functions_characterisation.R"))
  if(vaccine_data) {
    cohorts_interest_base <- c(1:60)
    cohorts_interest_any <- c(1:180)
  } else {
    cohorts_interest_base <- c(1:44, 53:60)
    cohorts_interest_any <- c(1:132, 157:180)
  }
  
  do_lsc(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_lsc(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_du(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  
  # Extra drug utilisation cohort analysis
  do_du(210, "any_extra", OverlapCohortsCName, any = FALSE)
}