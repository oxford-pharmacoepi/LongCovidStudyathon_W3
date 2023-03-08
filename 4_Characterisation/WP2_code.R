# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts","lc_pasc_hucohorts","lc_pasc_tpcohorts"))

# get functions used throughout this script
source(here("4_Characterisation","functions_characterisation.R"))

if(doCharacterisation) {
  # Output folders for WP2
  output_lsc <- file.path(tempDir,"Large-scale Characterisation")
  if (!file.exists(output_lsc)){
    dir.create(output_lsc, recursive = TRUE)}
}

if(doTreatmentPatterns) {
  output_tp <- file.path(tempDir,"Treatment Patterns")
  if (!file.exists(output_tp)){
    dir.create(output_tp, recursive = TRUE)}
  
  output_tp_db <- file.path(tempDir, db.name)
  if (!file.exists(output_tp_db)){
    dir.create(output_tp_db, recursive = TRUE)}
}

names_final_cohorts <- read.csv(names_final_cohorts,
          file = file.path(tempDir,
                           paste0(db.name,"_cohorts.csv")
          ),
          row.names = FALSE
)

# -------------------------------------------------------------------
# Cohorts to characterise
cohort_ids_interest <-  c(1:4,105:116)

# -------------------------------------------------------------------
if(doCharacterisation) {
  # CHARACTERISATION NO STRATA
  info(logger, '-- Performing Large-scale characterisation for all the cohorts of interest, no strata')
  
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(cohort_ids_interest, "all")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(cohort_ids_interest, "all")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(cohort_ids_interest, "all")
}

if(doDrugUtilisation) {
  # DRUG UTILISATION NO STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(cohort_ids_interest, "all")
}
  
if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  lapply(cohort_ids_interest, do_tp)
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
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(c(125:156), "sex")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(125:156), "sex")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(125:156), "sex")
}

if(doDrugUtilisation) {
  # DRUG UTILISATION SEX STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(125:156), "sex")
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  lapply(c(125:156), do_tp)
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
  # Large scale characterisation
  info(logger, '--- Looking at baseline characterisation')
  do_lsc(c(1000:1127), "age")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(1000:1127), "age")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(1000:1127), "age")
}

if(doDrugUtilisation) {
  # DRUG UTILISATION AGE STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(1000:1127), "age")
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  lapply(c(1000:1127), do_tp)
}

# -------------------------------------------------------------------
# CHARACTERISATION CALENDAR PERIOD STRATA
if(doCharacterisation) {
  info(logger, '-- Calculating Large-scale characterisation for calendar period strata')
  # Here calendar period refers only to Delta and Omicron variants
  
  info(logger, '--- Looking at baseline characterisation')
  # Large scale characterisation
  do_lsc(c(924:955), "calendar_period")
  
  info(logger, '--- Looking at vaccination outcomes for characterisation')
  # Vaccination
  do_vaccination_characterisation(c(924:955), "calendar_period")
  
  info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
  # Healthcare Utilisation
  # sick leave missing
  do_hu(c(924:955), "calendar_period")
}

if(doDrugUtilisation) {
  # DRUG UTILISATION CALENDAR PERIOD STRATA
  info(logger, '--- Looking at drug utilisation')
  do_du(c(924:955), "calendar_period")
}

if(doTreatmentPatterns) {
  info(logger, '-- Performing Treatment Patterns calculations')
  # Treatment Patterns
  lapply(c(924:955), do_tp)
}

# -------------------------------------------------------------------
# CHARACTERISATION VACCINATION STRATA
if(vaccine_data) {
  if(doCharacterisation) {
    info(logger, '-- Calculating Large-scale characterisation for vaccination strata')
    cohort_ids_interest <-  c(117:124,900:923) 
    # 117:124 base cohorts strata vacc, 900:923 any/code cohorts strata vacc
    
    info(logger, '--- Looking at baseline characterisation')
    # Large scale characterisation
    do_lsc(c(117:124,900:923), "vaccination")
    
    info(logger, '--- Looking at vaccination outcomes for characterisation')
    # Vaccination
    do_vaccination_characterisation(c(117:124,900:923), "vaccination")
    
    info(logger, '--- Looking at healthcare utilisation outcomes for characterisation')
    # Healthcare Utilisation
    # sick leave missing
    do_hu(c(117:124,900:923), "vaccination")
  }
  if(doDrugUtilisation) {
    # DRUG UTILISATION CALENDAR PERIOD STRATA
    info(logger, '--- Looking at drug utilisation')
    do_du(c(117:124,900:923), "vaccination")
  }
  if(doTreatmentPatterns) {
    info(logger, '-- Performing Treatment Patterns calculations')
    # Treatment Patterns
    lapply(c(117:124,900:923), do_tp)
  }
}

# -------------------------------------------------------------------------
# Can save the file with cohortIds and cohortNames now, as no more will be created for the rest of the package

write.csv(
  names_final_cohorts,
  file = file.path(tempDir, "Cohort_names_ids.csv"),
  row.names = FALSE
)
