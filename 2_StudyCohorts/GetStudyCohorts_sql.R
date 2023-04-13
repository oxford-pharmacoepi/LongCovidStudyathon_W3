# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# get functions used throughout this script
source(here("2_StudyCohorts","functions_getCohorts_sql.R"))

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

negative_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 3) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

censorcovid_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

influenza_init <- cdm[[InitialCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id == 4) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

covid <- do_exclusion(cdm, newinf_init, id = 1,
                      S_start_date = study_start_date, covidcensor = FALSE)
nocovid <- do_exclusion(cdm, negative_init, id = 3,
                        S_start_date = study_start_date)
influenza <- do_exclusion(cdm, influenza_init, id = 4,
                          S_start_date = as.Date("2017-01-01"),
                          influenza = TRUE)
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% dplyr::mutate(cohort_definition_id = 2) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Tested negative "final"
negativetest <- nocovid[[1]]
negativetest <- negativetest %>% dplyr::mutate(cohort_definition_id = 3) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Influenza
flu <- influenza[[1]]
flu <- flu %>% dplyr::mutate(cohort_definition_id = 4) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()
attrition_negative <- nocovid[[3]]
attrition_negative <- attrition_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
  compute()
attrition_flu <- influenza[[3]]
attrition_flu <- attrition_flu %>% dplyr::mutate(cohort_definition_id = 4) %>%
  compute()
attrition <- rbind(attrition_positive,attrition_negative,attrition_flu)

# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()
attrition_censor_negative <- nocovid[[4]]
attrition_censor_negative <- attrition_censor_negative %>% dplyr::mutate(cohort_definition_id = 3) %>%
  compute()
attrition_censor_flu <- influenza[[4]]
attrition_censor_flu <- attrition_censor_flu %>% dplyr::mutate(cohort_definition_id = 4) %>%
  compute()
attrition_censor <- rbind(attrition_censor_positive,attrition_censor_negative,attrition_censor_flu)

computeQuery(new_infection,
             name = BaseCohortsName,
             temporary = FALSE,
             schema = results_database_schema,
             overwrite = TRUE)
appendPermanent(reinfection, name = BaseCohortsName,
                schema = results_database_schema)
appendPermanent(negativetest, name = BaseCohortsName,
                schema = results_database_schema)
appendPermanent(flu, name = BaseCohortsName,
                schema = results_database_schema)

write_csv(
  attrition,
  file = here::here(output_at, "attrition_base.csv")
)
write_csv(
  attrition_censor,
  file = here::here(output_at, "attrition_base_censoring.csv")
)

names_final_cohorts <- dplyr::tibble(table_name = BaseCohortsName,
                                     cohort_definition_id = c(1:4),
                                     cohort_name = c("Infection","Reinfection","Test_negative","Influenza"))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
create_outcome(cdm, window = c(5:29), filter_start = FALSE, first_event = FALSE, 
               new_ids = c(1:25), tableName = LongCovidCohortsName)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = c(1:25),
                                           cohort_name = Initial_cohorts$cohort_name[5:29]))

# Any LC symptom
create_any_cohort(cdm, c(5:29), cohort_id = 26, LC = TRUE, 
                  tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 26, cohort_name = "Any LC symptom"))

# LC code
create_outcome(cdm, window = 101, new_ids = 27, tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 27, cohort_name = "LC code"))

# PASC events
create_outcome(cdm, window = c(30:39), filter_start = FALSE, 
               new_ids = c(1:10), tableName = PascCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = PascCohortsName,
                                           cohort_definition_id = c(1:10),
                                           cohort_name = Initial_cohorts$cohort_name[30:39]))

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                   PascCohortsName))


# Any PASC event
create_any_cohort(cdm, c(1:10), cohort_id = 11,
                  tableName = PascCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = PascCohortsName,
                                           cohort_definition_id = 11, cohort_name = "Any PASC event"))

# Medical conditions
create_outcome(cdm, window = c(40:63), end_outcome = FALSE, filter_start = FALSE, 
               new_ids = c(1:24), tableName = MedCondCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = MedCondCohortsName,
                                           cohort_definition_id = c(1:24),
                                           cohort_name = Initial_cohorts$cohort_name[40:63]))

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                   PascCohortsName,MedCondCohortsName))

# ---------------------------------------------------------------------
# STRATA COHORTS

message("Getting strata cohorts")
info(logger, '-- Getting strata cohorts')

# Vaccinated people
if(vaccine_data && db.name != "CPRDGold") {
  if(vaccine_brand) {
    vaccinated <- cdm[[InitialCohortsName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% c(64:67)) %>%
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
    vaccinated_JJ <- vaccinated_first %>% dplyr::filter(cohort_definition_id == 65) %>%
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
    
    computeQuery(vaccinated, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
    appendPermanent(nonvaccinated, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_first, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_second, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_third, name = VaccCohortsName,  schema = results_database_schema)
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = VaccCohortsName,
                                               cohort_definition_id = c(1:5), 
                                               cohort_name = c("Vaccinated", "Not_vaccinated", "First_dose", "Second_dose", "Third_dose")))
    
  } else {
    vaccinated <- cdm[[InitialCohortsName]] %>%
      dplyr::filter(.data$cohort_definition_id == 100) %>%
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
    
    computeQuery(vaccinated, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
    appendPermanent(nonvaccinated, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_first, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_second, name = VaccCohortsName,  schema = results_database_schema)
    appendPermanent(vaccinated_third, name = VaccCohortsName,  schema = results_database_schema)
    
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
  
  computeQuery(vaccinated, name = VaccCohortsName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)
  appendPermanent(nonvaccinated, name = VaccCohortsName,  schema = results_database_schema)
  appendPermanent(vaccinated_first, name = VaccCohortsName,  schema = results_database_schema)
  appendPermanent(vaccinated_second, name = VaccCohortsName,  schema = results_database_schema)
  appendPermanent(vaccinated_third, name = VaccCohortsName,  schema = results_database_schema)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = VaccCohortsName,
                                             cohort_definition_id = c(1:5), 
                                             cohort_name = c("Vaccinated", "Not_vaccinated", "First_dose", "Second_dose", "Third_dose")))
  
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName))
}

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS CHARACTERISATION

message("Getting overlapping cohorts")
info(logger, '-- Getting overlapping cohorts')

# LC any symptom + Infection / Reinfection / Test negative / Influenza
do_overlap_LCany(cdm, c(1:4), c(5:29), c(1:4))
# This is slow! #K

# LC code + Infection
do_overlap(cdm, 1, 27, 5, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)

# LC code + Reinfection
do_overlap(cdm, 2, 27, 6, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)

# LC code + Test negative
do_overlap(cdm, 3, 27, 7, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)

# LC code + Influenza
do_overlap(cdm, 4, 27, 8, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)

# PASC any symptom + Infection
do_overlap(cdm, 1, 11, 9, washout = FALSE, tableName = PascCohortsName,
           overlapTableName = OverlapCohortsCName)

# PASC any symptom + Reinfection
do_overlap(cdm, 2, 11, 10, washout = FALSE, tableName = PascCohortsName,
           overlapTableName = OverlapCohortsCName)

# PASC any symptom + Test negative
do_overlap(cdm, 3, 11, 11, washout = FALSE, tableName = PascCohortsName,
           overlapTableName = OverlapCohortsCName)

# PASC any symptom + Influenza
do_overlap(cdm, 4, 11, 12, washout = FALSE, tableName = PascCohortsName,
           overlapTableName = OverlapCohortsCName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(1:12), 
                                           cohort_name = c("LC_any_inf","LC_any_reinf","LC_any_neg", "LC_any_flu",
                                                          "LC_code_inf","LC_code_reinf","LC_code_neg","LC_code_flu",
                                                          "PASC_any_inf","PASC_any_reinf","PASC_any_neg","PASC_any_ flu")))

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName))
}                        


# Characterisation cohorts stratified by sex
do_sex_strata(1, 5, BaseCohortsName)
do_sex_strata(2, 7, BaseCohortsName)
do_sex_strata(3, 9, BaseCohortsName)
do_sex_strata(4, 11, BaseCohortsName)
do_sex_strata(1, 13, OverlapCohortsCName)
do_sex_strata(2, 15, OverlapCohortsCName)
do_sex_strata(3, 17, OverlapCohortsCName)
do_sex_strata(4, 19, OverlapCohortsCName)
do_sex_strata(5, 21, OverlapCohortsCName)
do_sex_strata(6, 23, OverlapCohortsCName)
do_sex_strata(7, 25, OverlapCohortsCName)
do_sex_strata(8, 27, OverlapCohortsCName)
do_sex_strata(9, 29, OverlapCohortsCName)
do_sex_strata(10, 31, OverlapCohortsCName)
do_sex_strata(11, 33, OverlapCohortsCName)
do_sex_strata(12, 35, OverlapCohortsCName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(5:12), 
                                           cohort_name =c("Inf_females", "Inf_males",
                                                          "Reinf_females", "Reinf_males",
                                                          "Neg_females", "Neg_males",
                                                          "Flu_females", "Flu_males")))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(13:36), 
                                           cohort_name =c("Inf_LC_Any_females", "Inf_LC_Any_males",
                                                          "Reinf_LC_Any_females", "Reinf_LC_Any_males",
                                                          "Neg_LC_Any_females", "Neg_LC_Any_males",
                                                          "Flu_LC_Any_females", "Flu_LC_Any_males",
                                                          "Inf_LC_Code_females", "Inf_LC_Code_males",
                                                          "Reinf_LC_Code_females", "Reinf_LC_Code_males",
                                                          "Neg_LC_Code_females", "Neg_LC_Code_males",
                                                          "Flu_LC_Code_females", "Flu_LC_Code_males",
                                                          "Inf_PASC Any_females", "Inf_PASC Any_males",
                                                          "Reinf_PASC Any_females", "Reinf_PASC Any_males",
                                                          "Neg_PASC Any_females", "Neg_PASC Any_males",
                                                          "Flu_PASC Any_females", "Flu_PASC Any_males")))


# Characterisation cohorts stratified by age
do_age_strata(1, 13, BaseCohortsName)
do_age_strata(2, 21, BaseCohortsName)
do_age_strata(3, 29, BaseCohortsName)
do_age_strata(4, 37, BaseCohortsName)
do_age_strata(1, 37, OverlapCohortsCName)
do_age_strata(2, 45, OverlapCohortsCName)
do_age_strata(3, 53, OverlapCohortsCName)
do_age_strata(4, 61, OverlapCohortsCName)
do_age_strata(5, 69, OverlapCohortsCName)
do_age_strata(6, 77, OverlapCohortsCName)
do_age_strata(7, 85, OverlapCohortsCName)
do_age_strata(8, 93, OverlapCohortsCName)
do_age_strata(9, 101, OverlapCohortsCName)
do_age_strata(10, 109, OverlapCohortsCName)
do_age_strata(11, 117, OverlapCohortsCName)
do_age_strata(12, 125, OverlapCohortsCName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(13:44), 
                                           cohort_name =c("Inf_age_(0,2)" , "Inf_age_(3,5)", 
                                                          "Inf_age_(6,9)", "Inf_age_(10,13)",
                                                          "Inf_age_(14,17)", "Inf_age_(18,40)",
                                                          "Inf_age_(41,64)", "Inf_age_(65,120)",
                                                          "Reinf_age_(0,2)" , "Reinf_age_(3,5)", 
                                                          "Reinf_age_(6,9)", "Reinf_age_(10,13)",
                                                          "Reinf_age_(14,17)", "Reinf_age_(18,40)",
                                                          "Reinf_age_(41,64)", "Reinf_age_(65,120)",
                                                          "Neg_age_(0,2)" , "Neg_age_(3,5)", 
                                                          "Neg_age_(6,9)", "Neg_age_(10,13)",
                                                          "Neg_age_(14,17)", "Neg_age_(18,40)",
                                                          "Neg_age_(41,64)", "Neg_age_(65,120)",
                                                          "Flu_age_(0,2)" , "Flu_age_(3,5)", 
                                                          "Flu_age_(6,9)", "Flu_age_(10,13)",
                                                          "Flu_age_(14,17)", "Flu_age_(18,40)",
                                                          "Flu_age_(41,64)", "Flu_age_(65,120)")))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(37:132), 
                                           cohort_name =c("Inf_LC_any_age_(0,2)" , "Inf_LC_any_age_(3,5)", 
                                                          "Inf_LC_any_age_(6,9)", "Inf_LC_any_age_(10,13)",
                                                          "Inf_LC_any_age_(14,17)", "Inf_LC_any_age_(18,40)",
                                                          "Inf_LC_any_age_(41,64)", "Inf_LC_any_age_(65,120)",
                                                          "Reinf_LC_any_age_(0,2)" , "Reinf_LC_any_age_(3,5)", 
                                                          "Reinf_LC_any_age_(6,9)", "Reinf_LC_any_age_(10,13)",
                                                          "Reinf_LC_any_age_(14,17)", "Reinf_LC_any_age_(18,40)",
                                                          "Reinf_LC_any_age_(41,64)", "Reinf_LC_any_age_(65,120)",
                                                          "Neg_LC_any_age_(0,2)" , "Neg_LC_any_age_(3,5)", 
                                                          "Neg_LC_any_age_(6,9)", "Neg_LC_any_age_(10,13)",
                                                          "Neg_LC_any_age_(14,17)", "Neg_LC_any_age_(18,40)",
                                                          "Neg_LC_any_age_(41,64)", "Neg_LC_any_age_(65,120)",
                                                          "Flu_LC_any_age_(0,2)" , "Flu_LC_any_age_(3,5)", 
                                                          "Flu_LC_any_age_(6,9)", "Flu_LC_any_age_(10,13)",
                                                          "Flu_LC_any_age_(14,17)", "Flu_LC_any_age_(18,40)",
                                                          "Flu_LC_any_age_(41,64)", "Flu_LC_any_age_(65,120)",
                                                          "Inf_LC_code_age_(0,2)" , "Inf_LC_code_age_(3,5)", 
                                                          "Inf_LC_code_age_(6,9)", "Inf_LC_code_age_(10,13)",
                                                          "Inf_LC_code_age_(14,17)", "Inf_LC_code_age_(18,40)",
                                                          "Inf_LC_code_age_(41,64)", "Inf_LC_code_age_(65,120)",
                                                          "Reinf_LC_code_age_(0,2)" , "Reinf_LC_code_age_(3,5)", 
                                                          "Reinf_LC_code_age_(6,9)", "Reinf_LC_code_age_(10,13)",
                                                          "Reinf_LC_code_age_(14,17)", "Reinf_LC_code_age_(18,40)",
                                                          "Reinf_LC_code_age_(41,64)", "Reinf_LC_code_age_(65,120)",
                                                          "Neg_LC_code_age_(0,2)" , "Neg_LC_code_age_(3,5)", 
                                                          "Neg_LC_code_age_(6,9)", "Neg_LC_code_age_(10,13)",
                                                          "Neg_LC_code_age_(14,17)", "Neg_LC_code_age_(18,40)",
                                                          "Neg_LC_code_age_(41,64)", "Neg_LC_code_age_(65,120)",
                                                          "Flu_LC_code_age_(0,2)" , "Flu_LC_code_age_(3,5)", 
                                                          "Flu_LC_code_age_(6,9)", "Flu_LC_code_age_(10,13)",
                                                          "Flu_LC_code_age_(14,17)", "Flu_LC_code_age_(18,40)",
                                                          "Flu_LC_code_age_(41,64)", "Flu_LC_code_age_(65,120)",
                                                          "Inf_PASC_any_age_(0,2)" , "Inf_PASC any_age_(3,5)", 
                                                          "Inf_PASC_any_age_(6,9)", "Inf_PASC any_age_(10,13)",
                                                          "Inf_PASC_any_age_(14,17)", "Inf_PASC any_age_(18,40)",
                                                          "Inf_PASC_any_age_(41,64)", "Inf_PASC any_age_(65,120)",
                                                          "Reinf_PASC_any_age_(0,2)" , "Reinf_PASC any_age_(3,5)", 
                                                          "Reinf_PASC_any_age_(6,9)", "Reinf_PASC any_age_(10,13)",
                                                          "Reinf_PASC_any_age_(14,17)", "Reinf_PASC any_age_(18,40)",
                                                          "Reinf_PASC_any_age_(41,64)", "Reinf_PASC any_age_(65,120)",
                                                          "Neg_PASC_any_age_(0,2)" , "Neg_PASC any_age_(3,5)", 
                                                          "Neg_PASC_any_age_(6,9)", "Neg_PASC any_age_(10,13)",
                                                          "Neg_PASC_any_age_(14,17)", "Neg_PASC any_age_(18,40)",
                                                          "Neg_PASC_any_age_(41,64)", "Neg_PASC any_age_(65,120)",
                                                          "Flu_PASC_any_age_(0,2)" , "Flu_PASC_any_age_(3,5)", 
                                                          "Flu_PASC_any_age_(6,9)", "Flu_PASC_any_age_(10,13)",
                                                          "Flu_PASC_any_age_(14,17)", "Flu_PASC_any_age_(18,40)",
                                                          "Flu_PASC_any_age_(41,64)", "Flu_PASC_any_age_(65,120)")))


if(vaccine_data) {
  # Characterisation cohorts stratified by vaccination
  do_overlap_vacc(1, 45, BaseCohortsName)
  do_overlap_vacc(2, 47, BaseCohortsName)
  do_overlap_vacc(3, 49, BaseCohortsName)
  do_overlap_vacc(4, 51, BaseCohortsName)
  do_overlap_vacc(1, 133, OverlapCohortsCName)
  do_overlap_vacc(2, 135, OverlapCohortsCName)
  do_overlap_vacc(3, 137, OverlapCohortsCName)
  do_overlap_vacc(4, 139, OverlapCohortsCName)
  do_overlap_vacc(5, 141, OverlapCohortsCName)
  do_overlap_vacc(6, 143, OverlapCohortsCName)
  do_overlap_vacc(7, 145, OverlapCohortsCName)
  do_overlap_vacc(8, 147, OverlapCohortsCName)
  do_overlap_vacc(9, 149, OverlapCohortsCName)
  do_overlap_vacc(10, 151, OverlapCohortsCName)
  do_overlap_vacc(11, 153, OverlapCohortsCName)
  do_overlap_vacc(12, 155, OverlapCohortsCName)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = BaseCohortsName,
                                             cohort_definition_id = c(45:52), 
                                             cohort_name = c("Inf_vacc","Inf_not_vacc",
                                                             "Reinf_vacc","Reinf_not_vacc",
                                                             "Neg_vacc", "Neg_not_vacc",
                                                             "Flu_vacc", "Flu_not_vacc")))
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsCName,
                                             cohort_definition_id = c(133:156), 
                                             cohort_name = c("Inf_LC_any_vacc",
                                                             "Inf_LC_any_not_vacc",
                                                             "Reinf_LC_any_vacc",
                                                             "Reinf_LC_any_not_vacc",
                                                             "Neg_LC_any_vacc", 
                                                             "Neg_LC_any_not_vacc",
                                                             "Flu_LC_any_vacc", 
                                                             "Flu_LC_any_not_vacc",
                                                             "Inf_LC_code_vacc",
                                                             "Inf_LC_code_not_vacc",
                                                             "Reinf_LC_code_vacc",
                                                             "Reinf_LC_code_not_vacc",
                                                             "Neg_LC_code_vacc", 
                                                             "Neg_LC_code_not_vacc",
                                                             "Flu_LC_code_vacc", 
                                                             "Flu_LC_code_not_vacc",
                                                             "Inf_PASC_any_vacc",
                                                             "Inf_PASC_any_not_vacc",
                                                             "Reinf_PASC_any_vacc",
                                                             "Reinf_PASC_any_not_vacc",
                                                             "Neg_PASC_any_vacc", 
                                                             "Neg_PASC_any_not_vacc",
                                                             "Flu_PASC_any_vacc", 
                                                             "Flu_PASC_any_not_vacc") ))
  
  
}

# Characterisation cohorts stratified by calendar period
do_strata_calendar(1, 53, BaseCohortsName)
do_strata_calendar(2, 55, BaseCohortsName)
do_strata_calendar(3, 57, BaseCohortsName)
do_strata_calendar(4, 59, BaseCohortsName)
do_strata_calendar(1, 157, OverlapCohortsCName)
do_strata_calendar(2, 159, OverlapCohortsCName)
do_strata_calendar(3, 161, OverlapCohortsCName)
do_strata_calendar(4, 163, OverlapCohortsCName)
do_strata_calendar(5, 165, OverlapCohortsCName)
do_strata_calendar(6, 167, OverlapCohortsCName)
do_strata_calendar(7, 169, OverlapCohortsCName)
do_strata_calendar(8, 171, OverlapCohortsCName)
do_strata_calendar(9, 173, OverlapCohortsCName)
do_strata_calendar(10, 175, OverlapCohortsCName)
do_strata_calendar(11, 177, OverlapCohortsCName)
do_strata_calendar(12, 179, OverlapCohortsCName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(53:60), 
                                           cohort_name =c("Inf_Delta", "Inf_Omicron",
                                                          "Reinf_Delta", "Reinf_Omicron",
                                                          "Neg_Delta", "Neg_Omicron",
                                                          "Flu_Delta", "Flu_Omicron")))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(157:180), 
                                           cohort_name =c("Inf_LC_Any_Delta", "Inf_LC_Any_Omicron",
                                                          "Reinf_LC_Any_Delta", "Reinf_LC_Any_Omicron",
                                                          "Neg_LC_Any_Delta", "Neg_LC_Any_Omicron",
                                                          "Flu_LC_Any_Delta", "Flu_LC_Any_Omicron",
                                                          "Inf_LC_Code_Delta", "Inf_LC_Code_Omicron",
                                                          "Reinf_LC_Code_Delta", "Reinf_LC_Code_Omicron",
                                                          "Neg_LC_Code_Delta", "Neg_LC_Code_Omicron",
                                                          "Flu_LC_Code_Delta", "Flu_LC_Code_Omicron",
                                                          "Inf_PASC_Any_Delta", "Inf_PASC_Any_Omicron",
                                                          "Reinf_PASC_Any_Delta", "Reinf_PASC_Any_Omicron",
                                                          "Neg_PASC_Any_Delta", "Neg_PASC_Any_Omicron",
                                                          "Flu_PASC_Any_Delta", "Flu_PASC_Any_Omicron")))

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS INCIDENCE
# Overlapping cohorts of single symptoms / events / medical conditions with base cohorts

counter <- 1
if(cdm[[LongCovidCohortsName]] %>% 
   dplyr::filter(cohort_definition_id == 1) %>% tally() %>% pull() > 5) {
  do_overlap(cdm, 1, 1, counter, tableName = LongCovidCohortsName,
             overlapTableName = OverlapCohortsIPName, first = TRUE)
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = OverlapCohortsIPName,
                                             cohort_definition_id = counter, 
                                             cohort_name =paste0("Base_",1,"_LC_outcome_",1) ))
  counter <- counter + 1
  
  for(i in c(2:4)) {
    do_overlap(cdm, i, 1, counter, tableName = LongCovidCohortsName,
               overlapTableName = OverlapCohortsIPName)
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsIPName,
                                               cohort_definition_id = counter, 
                                               cohort_name =paste0("Base_",i,"_LC_outcome_",1) ))
    counter <- counter + 1
  }
}

base_ids <- c(1:4)
outcome_ids <- c(2:25)
for(i in base_ids) {
  for(j in outcome_ids){
    if(cdm[[LongCovidCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      do_overlap(cdm, i, j, counter, tableName = LongCovidCohortsName,
                 overlapTableName = OverlapCohortsIPName)
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsIPName,
                                                 cohort_definition_id = counter, 
                                                 cohort_name =paste0("Base_",i,"_LC_outcome_",j) ))
    }
    counter <- counter + 1
  }
}

outcome_ids <- c(1:10)
for(i in base_ids) {
  for(j in outcome_ids){
    if(cdm[[PascCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      do_overlap(cdm, i, j, counter, tableName = PascCohortsName,
                 overlapTableName = OverlapCohortsIPName)
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsIPName,
                                                 cohort_definition_id = counter, 
                                                 cohort_name =paste0("Base_",i,"_PASC_outcome_",j) ))
    }
    counter <- counter + 1
  }
}

outcome_ids <- c(1:24)
for(i in base_ids) {
  for(j in outcome_ids){
    if(cdm[[MedCondCohortsName]] %>% 
       dplyr::filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      do_overlap(cdm, i, j, counter, tableName = MedCondCohortsName,
                 overlapTableName = OverlapCohortsIPName)
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(table_name = OverlapCohortsIPName,
                                                 cohort_definition_id = counter, 
                                                 cohort_name =paste0("Base_",i,"_MC_outcome_",j) ))
    }
    counter <- counter + 1
  }
}

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
}

# -------------------------------------------------------------------
# TREATMENT PATTERNS and HEALTHCARE UTILISATION COHORTS, TRAJ COHORT

if(doTreatmentPatterns) {
create_outcome(cdm, window = c(68:99), filter_start = FALSE, first_event = FALSE,
               new_ids = c(61:92), tableName = BaseCohortsName)
create_outcome(cdm, window = c(68:99), filter_start = FALSE, first_event = FALSE,
                 new_ids = c(181:212), tableName = OverlapCohortsCName)  
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(61:92),
                                           cohort_name = Initial_cohorts$cohort_name[68:99]))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(181:212),
                                           cohort_name = Initial_cohorts$cohort_name[68:99]))

}

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

if(doTrajectories) {
  # Save the "any LC symptom + infection" or "infection + LC code" cohort in a table which can be read by Trajectories package
  traj_table <- cdm[[OverlapCohortsCName]] %>%
    dplyr::filter(cohort_definition_id %in% c(1,5)) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    compute()
  computeQuery(traj_table, name = TrajCohortsName,  
               temporary = FALSE,
               schema = results_database_schema, overwrite = TRUE)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = TrajCohortsName,
                                             cohort_definition_id = c(1),
                                             cohort_name = "LC_any_code_Trajectories"))
}

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

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
