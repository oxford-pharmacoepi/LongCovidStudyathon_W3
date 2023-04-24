# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# get functions used throughout this script
source(here("2_StudyCohorts","functions_getCohorts.R"))

# observation period + death table
observation_death <- cdm$observation_period %>%
  dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
  left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"),
            by = "subject_id") %>%
  mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
  compute()

# Output folder for Attrition
output_at <- file.path(tempDir,"Attrition")
if (!file.exists(output_at)){
  dir.create(output_at, recursive = TRUE)}

# ---------------------------------------------------------------------
# BASE COHORTS

message("Getting base cohorts")
info(logger, '-- Getting base cohorts')

# Get initial cohorts to build the study cohorts
newinf_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 1) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

covid <- do_exclusion(cdm, newinf_init, id = 1,
                      S_start_date = study_start_date, covidcensor = FALSE)
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Attritions
attrition <- covid[[3]]
attrition <- attrition_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()

# Attritions for the censoring sub-part
attrition_censor <- covid[[4]]
attrition_censor <- attrition_censor_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()

computeQuery(new_infection,
             name = BaseCohortsName,
             temporary = FALSE,
             schema = results_database_schema,
             overwrite = TRUE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName))

write_csv(
  attrition,
  file = here::here(output_at, "attrition_base.csv")
)
write_csv(
  attrition_censor,
  file = here::here(output_at, "attrition_base_censoring.csv")
)

names_final_cohorts <- dplyr::tibble(table_name = BaseCohortsName,
                                     cohort_definition_id = c(1),
                                     cohort_name = c("Infection"))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
lcsymp <- create_outcome(cdm, window = c(2:26), filter_start = FALSE, first_event = FALSE, 
               new_ids = c(1:25), tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = c(1:25),
                                           cohort_name = Initial_cohorts$cohort_name[2:26]))
# LC code
lccode <- create_outcome(cdm, window = 31, new_ids = 26, tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 26, cohort_name = "LC code"))

longcovid <- dplyr::union_all(lcsymp,lccode)

computeQuery(longcovid,
             name = LongCovidCohortsName,
             temporary = FALSE,
             schema = results_database_schema,
             overwrite = TRUE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName))

# ---------------------------------------------------------------------
# STRATA COHORTS

message("Getting strata cohorts")
info(logger, '-- Getting strata cohorts')

# Vaccinated people
if(vaccine_data && db.name != "CPRDGold") {
  if(vaccine_brand) {
    vaccinated <- cdm[[InitialCohortsName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% c(27:30)) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "cohort_definition_id"
      ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
    # Only one and more than one dose
    vaccinated_multiple <- vaccinated %>%
      dplyr::filter(seq != 1) %>% compute()
    vaccinated_first <- vaccinated %>%
      dplyr::filter(seq == 1) %>% compute()
    vaccinated_second <- vaccinated %>%
      dplyr::filter(seq == 2) %>% compute()
    vaccinated_third <- vaccinated %>%
      dplyr::filter(seq == 3) %>% compute()
    vaccinated_JJ <- vaccinated_first %>% dplyr::filter(cohort_definition_id == 28) %>%
      compute()
    # Cohort fully vaccinated (one JJ dose or two any doses), add 14 days to vaccination day for full coverage
    vaccinated <- vaccinated_JJ %>% dplyr::union(vaccinated_multiple) %>%
      dplyr::rename(vacc_date = cohort_start_date) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(vacc_date, na.rm = TRUE)
      ) %>% dplyr::mutate(cohort_definition_id = 1) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_start_date = !!CDMConnector::dateadd("cohort_start_date", 14)) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
    nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
      dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                       by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(vacc_date) & .data$cohort_end_date > .data$vacc_date, .data$vacc_date, .data$cohort_end_date))) %>%
      dplyr::left_join(cdm$observation_period %>% 
                         dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
                       by = c("subject_id" = "person_id")) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    vacc_all <- dplyr::union_all(vaccinated, nonvaccinated, vaccinated_first, vaccinated_second, vaccinated_third)
    
    computeQuery(vacc_all, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = VaccCohortsName,
                                               cohort_definition_id = c(1:5), 
                                               cohort_name = c("Vaccinated", "Not_vaccinated", "First_dose", "Second_dose", "Third_dose")))
    
  } else {
    vaccinated <- cdm[[InitialCohortsName]] %>%
      dplyr::filter(.data$cohort_definition_id == 30) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "cohort_definition_id"
      ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
    # Only one and more than one dose
    vaccinated_multiple <- vaccinated %>%
      dplyr::filter(seq != 1) %>% compute()
    vaccinated_first <- vaccinated %>%
      dplyr::filter(seq == 1) %>% compute()
    vaccinated_second <- vaccinated %>%
      dplyr::filter(seq == 2) %>% compute()
    vaccinated_third <- vaccinated %>%
      dplyr::filter(seq == 3) %>% compute()
    # Cohort fully vaccinated add 14 days to vaccination day for full coverage
    vaccinated <- vaccinated %>%
      dplyr::rename(vacc_date = cohort_start_date) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(vacc_date, na.rm = TRUE)
      ) %>% dplyr::mutate(cohort_definition_id = 1) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_start_date = !!CDMConnector::dateadd("cohort_start_date", 14)) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
    nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
      dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                       by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(vacc_date) & .data$cohort_end_date > .data$vacc_date, .data$vacc_date, .data$cohort_end_date))) %>%
      dplyr::left_join(cdm$observation_period %>% 
                         dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
                       by = c("subject_id" = "person_id")) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    vacc_all <- dplyr::union_all(vaccinated, nonvaccinated, vaccinated_first, vaccinated_second, vaccinated_third)
    
    computeQuery(vacc_all, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = VaccCohortsName,
                                               cohort_definition_id = c(1:5), 
                                               cohort_name = c("Vaccinated", "Not_vaccinated", "First_dose", "Second_dose", "Third_dose")))
    
  }
} else if(vaccine_data && db.name == "CPRDGold") {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,"longcovid_project_vaccinated_cohort"))
  
  vaccinated <- cdm[["longcovid_project_vaccinated_cohort"]] %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_definition_id"
    ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::filter(!is.na(cohort_start_date)) %>%
    dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  # Only one and more than one dose
  vaccinated_multiple <- vaccinated %>%
    dplyr::filter(seq != 1) %>% compute()
  vaccinated_first <- vaccinated %>%
    dplyr::filter(seq == 1) %>% compute()
  vaccinated_second <- vaccinated %>%
    dplyr::filter(seq == 2) %>% compute()
  vaccinated_third <- vaccinated %>%
    dplyr::filter(seq == 3) %>% compute()
  vaccinated_JJ <- cdm[["longcovid_project_vaccinated_cohort"]] %>%
    dplyr::filter(cohort_definition_id == 3) %>%
    compute()
  # Cohort fully vaccinated (one JJ dose or two any doses), add 14 days to vaccination day for full coverage
  vaccinated <- vaccinated_JJ %>%  dplyr::union(vaccinated_multiple) %>%
    dplyr::rename(vacc_date = cohort_start_date) %>%
    dplyr::group_by(subject_id) %>%
    dplyr::summarise(
      cohort_start_date = min(vacc_date, na.rm = TRUE)
    ) %>% dplyr::mutate(cohort_definition_id = 1) %>%
    left_join(observation_death, by = c("subject_id"), copy = TRUE) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    dplyr::mutate(cohort_start_date = !!CDMConnector::dateadd("cohort_start_date", 14)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  
  # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
  nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
    dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                     by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
    dplyr::left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(vacc_date) & .data$cohort_end_date > .data$vacc_date, .data$vacc_date, .data$cohort_end_date))) %>%
    left_join(cdm$observation_period %>% 
                dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
              by = c("subject_id" = "person_id")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  
  vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_second <- vaccinated_second %>% mutate(cohort_definition_id = 4) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_third <- vaccinated_third %>% mutate(cohort_definition_id = 5) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  
  vacc_all <- dplyr::union_all(vaccinated, nonvaccinated, vaccinated_first, vaccinated_second, vaccinated_third)
  
  computeQuery(vacc_all, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = VaccCohortsName,
                                             cohort_definition_id = c(1:5), 
                                             cohort_name = c("Vaccinated", "Not_vaccinated", "First_dose", "Second_dose", "Third_dose")))
  
}


# ---------------------------------------------------------------------
# OVERLAPPING COHORTS
# Overlapping cohorts of single symptoms with base cohorts

counter <- 1
if(cdm[[LongCovidCohortsName]] %>% 
   dplyr::filter(cohort_definition_id == 1) %>% tally() %>% pull() > 5) {
  overlapip <- do_overlap(cdm, 1, 1, counter, tableName = LongCovidCohortsName,
                          overlapTableName = OverlapCohortsName, first = TRUE, tableold = bases)
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsName,
                                             cohort_definition_id = counter, 
                                             cohort_name =paste0("Base_",1,"_LC_outcome_",1) ))
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,OverlapCohortsCName,
                                     OverlapCohortsName))
}

counter <- counter + 1


base_ids <- c(1)
outcome_ids <- c(2:26)
for(i in base_ids) {
  for(j in outcome_ids){
    if(cdm[[LongCovidCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      overlapip <- do_overlap(cdm, i, j, counter, tableName = LongCovidCohortsName,
                              overlapTableName = OverlapCohortsName, tableold = overlapip)
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsName,
                                                 cohort_definition_id = counter, 
                                                 cohort_name =paste0("Base_",i,"_LC_outcome_",j) ))
    }
    counter <- counter + 1
  }
}

computeQuery(ovelapip, name = OverlapCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName,OverlapCohortsName))
}

# -------------------------------------------------------------------
# TREATMENT PATTERNS and HEALTHCARE UTILISATION COHORTS, TRAJ COHORT

if(doCharacterisation || doClustering) {
  # Get Healthcare Utilisation outcomes for characterisation and clustering
  HU_cohorts <- CDMConnector::readCohortSet(here::here("4_Characterisation","HU_cohorts"))
  cdm <- CDMConnector::generateCohortSet(cdm, HU_cohorts,
                                         name = HUCohortsName,
                                         overwrite = TRUE)
  cdm[[HUCohortsName]] <- cdm[[HUCohortsName]] %>% left_join(observation_death, 
                                                             by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    compute()
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = HUCohortsName,
                                             cohort_definition_id = c(1:4),
                                             cohort_name = HU_cohorts$cohort_name[1:4]))
  
}

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,OverlapCohortsName, HUCohortsName),
                    cdm_name = db.name)
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     OverlapCohortsName, HUCohortsName),
                    cdm_name = db.name)
}

finalCounts <- tibble::tibble(cohort_name = "start", n = 0)

for(name in CohortNames) {
  if(name %in% names(cdm)) {
    finalCounts <- finalCounts %>%
      dplyr::union(cdm[[name]] %>% 
      dplyr::group_by(cohort_definition_id) %>% 
      tally() %>% 
      collect() %>% 
      right_join(names_final_cohorts %>% dplyr::filter(table_name == name), 
                 by = c("cohort_definition_id")) %>% 
      dplyr::mutate(n = as.numeric(n)) %>% 
      dplyr::mutate(n = if_else(is.na(n), 0, n)) %>%
      dplyr::mutate(n = ifelse(n <= 5, NA, n)) %>% 
      dplyr::select(cohort_name, n))
  }
}

finalCounts <- finalCounts[-1,]

# Export csv
write_csv(finalCounts,
          file = file.path(tempDir,
            paste0(db.name,"_finalcounts.csv")
          )
)

write_csv(names_final_cohorts,
          file = file.path(tempDir,
                           paste0(db.name,"_cohorts.csv")
          )
)

# ------------------------------------------------------------------------------
# GET cohort for clustering: subject_id, symptoms (1 or 0), age and sex

# First get all people with LC symptoms (overlap with infection cohort at base)
symptoms_LC <- cdm[[OverlapCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id %in% c(1:26))

names_symptoms <- names_final_cohorts %>% 
  dplyr::filter(.data$table_name == LongCovidCohortsName) %>%
  dplyr::filter(.data$cohort_definition_id %in% c(1:26)) %>%
  dplyr::select(cohort_definition_id, cohort_name) %>% compute()

symptoms_LC <- symptoms_LC %>% addAge(cdm) %>% 
  addSex(cdm) %>% collect()
symptoms_LC <- symptoms_LC %>% 
  dplyr::left_join(names_symptoms, 
                   by = c("cohort_definition_id")) %>% 
  dplyr::select(cohort_name,subject_id,age,sex)

# Get the names of the symptoms or LC code
names_symptoms <- symptoms_LC %>% dplyr::select(cohort_name) %>% distinct() %>% 
  pull()

# Create table with columns of symptoms, 1 if individual has it, 0 otherwise
i = 1
working_name <- names_symptoms[i]
working_name <- enquo(working_name)
data_LCA <- symptoms_LC %>% dplyr::filter(cohort_name == !!working_name) %>% 
  dplyr::mutate(!!working_name := as.integer(1)) %>% 
  dplyr::select(subject_id,!!working_name)

for(i in 2:length(names_symptoms)) {
  working_name <- names_symptoms[i]
  working_name <- enquo(working_name)
  data_LCA <- data_LCA %>% full_join(symptoms_LC %>% 
                                       dplyr::filter(cohort_name == !!working_name) %>% 
                                       dplyr::mutate(!!working_name := as.integer(1)) %>% 
                                       dplyr::select(subject_id,!!working_name), 
                                     by = c("subject_id"))
  
}
data_LCA[is.na(data_LCA)] <- 0
data_LCA <- data_LCA %>% distinct()
data_LCA <- data_LCA %>% dplyr::left_join(symptoms_LC %>% 
                                            dplyr::select(subject_id,age,sex), 
                                          by = "subject_id") %>%
  distinct()

# Put column names well!


# Characterisation of the cluster people
cdm[["visit_occurrence"]] <- cdm[["visit_occurrence"]] %>%
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2016) %>%
  dplyr::compute()

ip.codes <- c(9201, 262)
# add all descendents
ip.codes.w.desc <- cdm$concept_ancestor %>%
  dplyr::filter(ancestor_concept_id  %in% ip.codes ) %>% 
  collect() %>% 
  dplyr::select(descendant_concept_id) %>% 
  dplyr::distinct() %>% 
  pull()

cohort_LC <- cohort_LC %>% 
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 1), c(-365,-1), "number_icu") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 2), c(-365,-1), "number_vent") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 3), c(-365,-1), "number_trach") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 4), c(-365,-1), "number_ecmo") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 1), c(1, 30), "number_icu_next") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 2), c(1, 30), "number_vent_next") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 3), c(1, 30), "number_trach_next") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 4), c(1, 30), "number_ecmo_next") %>%
  addNumberVisit(cdm, 9202, c(-365,-1), name = "number_gp_bef") %>% 
  addNumberVisit(cdm, 9202, c(1, 30), name = "number_gp_next") %>% 
  addNumberVisit(cdm, ip.codes.w.desc, c(-365,-1), name = "number_hosp_bef") %>% 
  addNumberVisit(cdm, ip.codes.w.desc, c(1,30), name = "number_hosp_next") %>% 
  compute()

cohort_set <- tibble(
  ancestor_concept_id = c(
    434621, 317009, 443392, 201820, 321588, 316866, 4030518, 255573, 4182210
  ),
  cohort_definition_id = 1:9,
  cohort_name = c(
    "autoimmune_disease", "asthma", "malignant_neoplastic_disease",
    "diabetes_mellitus", "heart_disease", "hypertensive_disorder",
    "renal_impairment", "copd", "dementia"
  )
)

for(i in c(1:9)) {
  ip.codes <- cohort_set$ancestor_concept_id[i]
  # add all descendents
  ip.codes.w.desc <- cdm$concept_ancestor %>%
    dplyr::filter(ancestor_concept_id  %in% ip.codes ) %>% 
    collect() %>% 
    dplyr::select(descendant_concept_id) %>% 
    dplyr::distinct() %>% 
    pull()
  
  name_col <- cohort_set$cohort_name[i]
  name_col <- rlang::enquo(name_col)
  
  cohort_LC <- cohort_LC %>% 
    addNumberEvent_in(cdm, "condition_occurrence", window = c(NA,-1), filter = list(condition_concept_id = ip.codes.w.desc), eventDate = "condition_start_date", name = cohort_set$cohort_name[i]) %>%
    compute()
}

if(vaccine_data) {
  cohort_LC <- cohort_LC %>% 
    addOverlap(cdm, VaccCohortsName, 3, "first_dose") %>% 
    addOverlap(cdm, VaccCohortsName, 4, "second_dose") %>% 
    addOverlap(cdm, VaccCohortsName, 5, "third_dose") %>% 
    compute()
}

computeQuery(data_LCA, name = clusterCohortName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
