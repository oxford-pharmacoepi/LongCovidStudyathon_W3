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

names_final_cohorts <- read.csv(file.path(paste0(db.name,"_Results/"),paste0(db.name,"_cohorts.csv")))

# Output folders for WP3
output_clustering <- file.path(tempDir,"Clustering")
if (!file.exists(output_clustering)){
  dir.create(output_clustering, recursive = TRUE)}

# Names of symptoms
symptoms_LC <- cdm[[OverlapCohortsIPName]] %>% 
  dplyr::filter(.data$cohort_definition_id %in% c(1:25))
symptoms_LC <- symptoms_LC %>%
  dplyr::full_join(
    cdm[[OverlapCohortsCName]] %>% 
      dplyr::filter(.data$cohort_definition_id == 5) %>%
      dplyr::mutate(cohort_definition_id = 27)) %>% collect()
  
names_symptoms <- names_final_cohorts %>% 
  dplyr::filter(.data$table_name == LongCovidCohortsName) %>%
  dplyr::filter(.data$cohort_definition_id %in% c(1:25)) %>%
  dplyr::select(cohort_definition_id, cohort_name) %>% compute()

names_symptoms <- names_symptoms %>% 
  dplyr::full_join(tibble::tibble(cohort_definition_id = 27,
                                  cohort_name = "Infection + LC code") %>% compute())

symptoms_LC <- symptoms_LC %>% 
  dplyr::left_join(names_symptoms, 
                   by = c("cohort_definition_id")) %>% 
  dplyr::select(cohort_name)

# Get the names of the symptoms or LC code
which_symptoms <- symptoms_LC %>% dplyr::group_by(cohort_name) %>%
  dplyr::tally() %>% ungroup() %>% dplyr::select(cohort_name, n)

write.csv(which_symptoms, here::here(tempDir, "groups_clustering.csv"))
