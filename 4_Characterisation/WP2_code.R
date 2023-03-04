# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts","lc_pasc_hucohorts","lc_pasc_tpcohorts"))

# get functions used throughout this script
source(here("4_Characterisation","functions_characterisation.R"))

if(doCharacterisation) {
  # Output folders for WP2
  output_lsc <- file.path(tempDir,"Large-scale Characterisation")
  if (!file.exists(output_lsc)){
    dir.create(output_lsc, recursive = TRUE)}
  
  # Get Healthcare Utilisation outcomes for characterisation
  HU_cohorts <- CDMConnector::readCohortSet(here::here("4_Characterisation","HU_cohorts"))
  cdm <- CDMConnector::generateCohortSet(cdm, HU_cohorts,
                                         cohortTableName = "lc_pasc_hucohorts",
                                         overwrite = TRUE)
  cdm$lc_pasc_hucohorts <- cdm$lc_pasc_hucohorts %>% left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>% compute()
}

if(doTreatmentPatterns) {
  output_tp <- file.path(tempDir,"Treatment Patterns")
  if (!file.exists(output_tp)){
    dir.create(output_tp, recursive = TRUE)}
  
  output_tp_db <- file.path(tempDir, db.name)
  if (!file.exists(output_tp_db)){
    dir.create(output_tp_db, recursive = TRUE)}
  
  # Get drug outcomes for Treatment Patterns
  TP_cohorts <- CDMConnector::readCohortSet(here::here("4_Characterisation","TreatmentPatterns_cohorts"))
  cdm <- CDMConnector::generateCohortSet(cdm, TP_cohorts,
                                         cohortTableName = "lc_pasc_tpcohorts",
                                         overwrite = TRUE)
  # Not to lose the instantiated cohorts from json from tampering with them
  cdm$lc_pasc_tpevents <- cdm$lc_pasc_tpcohorts
  cdm$lc_pasc_tpevents <- cdm$lc_pasc_tpevents %>% left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>% compute()
}

#K
# Should choose which temporalWindows, tables to Characterize, etc.
# Default temporalWindows =
# c(NA, -366), c(-365, -91),
# c(-365, -31), c(-90, -1), c(-30, -1),
# c(0, 0), c(1, 30), c(1, 90),
# c(31, 365), c(91, 365), c(366, NA)

# tablesToCharacterize = 
#  "condition_occurrence", "drug_era",
#  "procedure_occurrence", "measurement"

# -------------------------------------------------------------------
# Cohorts to characterise
#cohort_ids_interest <-  c(1:4,105:116)
cohort_ids_interest <-  c(1:4,105:108,113:116) # 109 to 112 missing (LC code)

# -------------------------------------------------------------------
if(doCharacterisation) {
  # CHARACTERISATION NO STRATA
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, no strata')
  
  info(logger, '--- Looking at baseline characterisation and drug utilisation')
  # Large scale characterisation, also includes DrugUtilisation
  do_lsc(cohort_ids_interest, "all")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(cohort_ids_interest, "all")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(cohort_ids_interest, "all")
}
  
if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  
  # Adam's take, not yet done
  cdm$lc_pasc_tptarget <- cdm$studyathon_final_cohorts %>%
    dplyr::filter(cohort_definition_id %in% cohort_ids_interest)
  
  cdm <- computeTreatmentPathways(cdm,targetCohortTable = "lc_pasc_tptarget",
                                  eventCohortTable = "lc_pasc_tpevents")
  
  
  # SOMETHING HERE TO GO FROM PATHWAYS in cdm$treatment_patterns TO HISTORY!!!
  
  findPatterns(cdm$treatment_patterns, minNumPatterns = 5)
}


# -------------------------------------------------------------------
# CHARACTERISATION SEX STRATA
do_sex_strata <- function(cohort_id,new_id) {
  sex_strata <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == cohort_id)
  females <- sex_strata %>% CohortProfiles::addSex(cdm) %>% dplyr::filter(sex == "Female") %>% mutate(cohort_definition_id = new_id)
  males <- sex_strata %>% CohortProfiles::addSex(cdm) %>% dplyr::filter(sex == "Male") %>% mutate(cohort_definition_id = new_id + 1)
  appendPermanent(females, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(males, name = "studyathon_final_cohorts",  schema = results_database_schema)
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(cohortId = c(new_id,new_id+1), 
                                             cohortName =c(paste0("Cohort ",i," females" ),paste0("Cohort ",i," males" ))))
}

id_new_sex <- c(seq(125,155,2))
lapply(cohort_ids_interest,id_new_sex,do_sex_strata) # Maybe Map instead

if(doCharacterisation) {
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, sex strata')
  # Large scale characterisation, also includes DrugUtilisation
  info(logger, '--- Looking at baseline characterisation and drug utilisation')
  # Large scale characterisation, also includes DrugUtilisation
  do_lsc(c(125:156), "sex")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(125:156), "sex")
  
  #K
  # Choose windows HU too
  # GP, vacc too?
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(125:156), "sex")
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  
}

# -------------------------------------------------------------------
# CHARACTERISATION AGE STRATA
do_age_strata <- function(cohort_id,new_id) {
  age_strata <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == cohort_id)
  age1 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(0:2)) %>% mutate(cohort_definition_id = new_id)
  age2 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(3:5)) %>% mutate(cohort_definition_id = new_id+1)
  age3 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(6:9)) %>% mutate(cohort_definition_id = new_id+2)
  age4 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(10:13)) %>% mutate(cohort_definition_id = new_id+3)
  age5 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(14:17)) %>% mutate(cohort_definition_id = new_id+4)
  age6 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(18:40)) %>% mutate(cohort_definition_id = new_id+5)
  age7 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(41:64)) %>% mutate(cohort_definition_id = new_id+6)
  age8 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(65:120)) %>% mutate(cohort_definition_id = new_id+7)
  
  appendPermanent(age1, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age2, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age3, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age4, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age5, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age6, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age7, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age8, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(cohortId = c(new_id : new_id+7), 
                                             cohortName =c(paste0("Cohort ",i," age (0,2)" ),
                                                           paste0("Cohort ",i," age (3,5)" ),
                                                           paste0("Cohort ",i," age (6,9)" ),
                                                           paste0("Cohort ",i," age (10,13)" ),
                                                           paste0("Cohort ",i," age (14,17)" ),
                                                           paste0("Cohort ",i," age (18,40)" ),
                                                           paste0("Cohort ",i," age (41,64)" ),
                                                           paste0("Cohort ",i," age (65,120)" ))))
  
}

id_new_age <- c(seq(1000,1127,8))
lapply(cohort_ids_interest,id_new_age,do_age_strata)

if(doCharacterisation) {
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, age strata')
  # Large scale characterisation, also includes DrugUtilisation
  info(logger, '--- Looking at baseline characterisation and drug utilisation')
  do_lsc(c(1000:1127), "age")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(1000:1127), "age")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(1000:1127), "age")
}
info(logger, '-- Calculating Large-scale characterisation for age strata')




info(logger, '-- Performing Treatment Patterns calculations')
# Treatment Patterns

# -------------------------------------------------------------------
# CHARACTERISATION CALENDAR PERIOD STRATA
if(doCharacterisation) {
  info(logger, '-- Calculating Large-scale characterisation for calendar period strata')
  # Here calendar period refers only to Delta and Omicron variants
  
  info(logger, '--- Looking at baseline characterisation and drug utilisation')
  # Large scale characterisation, also includes DrugUtilisation
  do_lsc(c(924:955), "calendar_period")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(924:955), "calendar_period")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(924:955), "calendar_period")
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  
}

# -------------------------------------------------------------------
# CHARACTERISATION VACCINATION STRATA
if(vaccine_data) {
  if(doCharacterisation) {
    info(logger, '-- Calculating Large-scale characterisation for vaccination strata')
    cohort_ids_interest <-  c(117:124,900:923) # 117:124 base cohorts strata vacc, 900:923 any/code cohorts strata vacc
    
    info(logger, '--- Looking at baseline characterisation and drug utilisation')
    # Large scale characterisation, also includes DrugUtilisation
    do_lsc(c(117:124,900:923), "vaccination")
    
    info(logger, '--- Looking at vaccination outcomes for characterisation')
    # Vaccination
    do_vaccination_characterisation(c(117:124,900:923), "vaccination")
    
    info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
    # Healthcare Utilisation
    # sick leave missing
    do_hu(c(117:124,900:923), "vaccination")
  }
  if(TreatmentPatterns) {
    info(logger, '--- Performing Treatment Patterns calculations')
    # Treatment Patterns
  }
}

# -------------------------------------------------------------------------
# Can save the file with cohortIds and cohortNames now, as no more will be created for the rest of the package

write.csv(
  names_final_cohorts,
  file = file.path(tempDir, "Cohort_names_ids.csv"),
  row.names = FALSE
)



# To be deleted soon, probably

# Try with TreatmentPatterns
# Get .csv with all event and target cohorts. Now just try event = 5 dummy drugs (2001 to 2006 ids), target AnyLC_inf cohort, id 100
#cohorts_treatmentpatterns <- dplyr::full_join(
#  cdm$studyathon_final_cohorts %>% dplyr::filter(cohort_definition_id == 105), 
#  cdm$lc_pasc_tpevents %>% mutate(cohort_definition_id = cohort_definition_id + 2000),
#  by = c("subject_id","cohort_definition_id","cohort_start_date","cohort_end_date")) %>% 
#  dplyr::select("cohortId" = "cohort_definition_id","personId" = "subject_id", "startDate" = "cohort_start_date", "endDate" = "cohort_end_date") %>% 
#  collect()

#write.csv(
#  cohorts_treatmentpatterns,
#  file = here::here("4_Characterisation", "TreatmentPatterns_cohorts","input_cohorts.csv"),
#  row.names = FALSE
#)

# The rest of the settings (1. Cohort name and id, 2. Pathway settings) are already in the folder
#dataSettings <- TreatmentPatterns::createDataSettings(
#  OMOP_CDM = FALSE, 
#  cohortLocation = here("4_Characterisation","TreatmentPatterns_cohorts","input_cohorts.csv")
#  )

#cohortSettings <- TreatmentPatterns::createCohortSettings(
#  cohortsToCreate_location = here("4_Characterisation","TreatmentPatterns_cohorts","cohorts_to_create.csv"),
#  cohortsFolder = here("4_Characterisation","TreatmentPatterns_cohorts")
#)
#pathwaySettings <- TreatmentPatterns::createPathwaySettings(
#  pathwaySettings_location = here("4_Characterisation","TreatmentPatterns_cohorts","pathway_settings.csv")
#)
#saveSettings <- TreatmentPatterns::createSaveSettings(databaseName = db.name,
#                                                      rootFolder = here(),
#                                                      outputFolder = here(output_tp))

#TreatmentPatterns::executeTreatmentPatterns(dataSettings = dataSettings, cohortSettings = cohortSettings,
#                                            pathwaySettings = pathwaySettings, saveSettings = saveSettings,
#                                            launchShiny = FALSE)

#TreatmentPatterns::launchResultsExplorer()


# https://github.com/mi-erasmusmc/TreatmentPatterns/blob/master/docs/TreatmentPatternsStudy.pdf


# If successful, this should be replicated with all base cohorts, and then tuned for all strata. Should be easy: only change target id and name file to read

