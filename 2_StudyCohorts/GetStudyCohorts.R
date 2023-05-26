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
  computeQuery()

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
  ) %>% computeQuery() 

covid <- do_exclusion(cdm, newinf_init, id = 1,
                      S_start_date = study_start_date)
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% dplyr::mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  computeQuery()

# Attritions
attrition <- covid[[3]]
attrition <- attrition %>% dplyr::mutate(cohort_definition_id = 1) %>%
  computeQuery()

# Attritions for the censoring sub-part
attrition_censor <- covid[[4]]
attrition_censor <- attrition_censor %>% dplyr::mutate(cohort_definition_id = 1) %>%
  computeQuery()

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
                                     cohort_name = c("infection"))

# Save attributes of the cohort
attr(new_infection, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == BaseCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(new_infection, "cohort_count") <- getCohortCount(new_infection)

cdm[[BaseCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(new_infection, BaseCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(new_infection, "cohort_set"), cdm, paste0(BaseCohortsName, "_set")),
  cohortCountRef = insertTable(attr(new_infection, "cohort_count"), cdm, paste0(BaseCohortsName, "_count"))
)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
lcsymp <- create_outcome(cdm, window = c(2:26), new_ids = c(1:25), tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = c(1:25),
                                           cohort_name = Initial_cohorts$cohort_name[2:26]))
# LC code
lccode <- create_outcome(cdm, window = 32, new_ids = 26, tableName = LongCovidCohortsName)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = LongCovidCohortsName,
                                           cohort_definition_id = 26, cohort_name = "lc_code"))

longcovid <- dplyr::union_all(lcsymp,lccode) %>% computeQuery()

# Save attributes of the cohort
attr(longcovid, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == LongCovidCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(longcovid, "cohort_count") <- getCohortCount(longcovid)

cdm[[LongCovidCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(longcovid, LongCovidCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(longcovid, "cohort_set"), cdm, paste0(LongCovidCohortsName, "_set")),
  cohortCountRef = insertTable(attr(longcovid, "cohort_count"), cdm, paste0(LongCovidCohortsName, "_count"))
)

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
      dplyr::filter(.data$cohort_definition_id %in% c(27:29,31)) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "cohort_definition_id"
      ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% computeQuery()
    # Only one and more than one dose
    vaccinated_multiple <- vaccinated %>%
      dplyr::filter(seq != 1) %>% computeQuery()
    vaccinated_first <- vaccinated %>%
      dplyr::filter(seq == 1) %>% computeQuery()
    vaccinated_second <- vaccinated %>%
      dplyr::filter(seq == 2) %>% computeQuery()
    vaccinated_third <- vaccinated %>%
      dplyr::filter(seq == 3) %>% computeQuery()
    vaccinated_JJ <- vaccinated_first %>% dplyr::filter(cohort_definition_id == 28) %>%
      computeQuery()
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
      computeQuery()
    
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
      computeQuery()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    
    vacc_all <- dplyr::union_all(vaccinated, nonvaccinated)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_first)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_second)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_third)
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = VaccCohortsName,
                                               cohort_definition_id = c(1:5), 
                                               cohort_name = c("vaccinated", "not_vaccinated", "first_dose", "second_dose", "third_dose")))
    
    # Save attributes of the cohort
    attr(vacc_all, "cohort_set") <- names_final_cohorts %>% 
      dplyr::filter(table_name == VaccCohortsName) %>%
      dplyr::select(cohort_definition_id, cohort_name) 
    
    attr(vacc_all, "cohort_count") <- getCohortCount(vacc_all)
    
    cdm[[VaccCohortsName]] <- newGeneratedCohortSet(
      cohortRef = computeQuery(vacc_all, VaccCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
      cohortSetRef = insertTable(attr(vacc_all, "cohort_set"), cdm, paste0(VaccCohortsName, "_set")),
      cohortCountRef = insertTable(attr(vacc_all, "cohort_count"), cdm, paste0(VaccCohortsName, "_count"))
    )
    
  } else {
    vaccinated <- cdm[[InitialCohortsName]] %>%
      dplyr::filter(.data$cohort_definition_id == 30) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "cohort_definition_id"
      ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% computeQuery()
    # Only one and more than one dose
    vaccinated_multiple <- vaccinated %>%
      dplyr::filter(seq != 1) %>% computeQuery()
    vaccinated_first <- vaccinated %>%
      dplyr::filter(seq == 1) %>% computeQuery()
    vaccinated_second <- vaccinated %>%
      dplyr::filter(seq == 2) %>% computeQuery()
    vaccinated_third <- vaccinated %>%
      dplyr::filter(seq == 3) %>% computeQuery()
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
      computeQuery()
    
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
      computeQuery()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    
    vacc_all <- dplyr::union_all(vaccinated, nonvaccinated)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_first)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_second)
    vacc_all <- dplyr::union_all(vacc_all, vaccinated_third)    
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = VaccCohortsName,
                                               cohort_definition_id = c(1:5), 
                                               cohort_name = c("vaccinated", "not_vaccinated", "first_dose", "second_dose", "third_dose")))
    
    # Save attributes of the cohort
    attr(vacc_all, "cohort_set") <- names_final_cohorts %>% 
      dplyr::filter(table_name == VaccCohortsName) %>%
      dplyr::select(cohort_definition_id, cohort_name) 
    
    attr(vacc_all, "cohort_count") <- getCohortCount(vacc_all)
    
    cdm[[VaccCohortsName]] <- newGeneratedCohortSet(
      cohortRef = computeQuery(vacc_all, VaccCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
      cohortSetRef = insertTable(attr(vacc_all, "cohort_set"), cdm, paste0(VaccCohortsName, "_set")),
      cohortCountRef = insertTable(attr(vacc_all, "cohort_count"), cdm, paste0(VaccCohortsName, "_count"))
    )
    
  }
} else if(vaccine_data && db.name == "CPRDGold") {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,"lcp_vaccinated"))
  
  vaccinated <- cdm[["lcp_vaccinated"]] %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      "cohort_definition_id"
    ) %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::filter(!is.na(cohort_start_date)) %>%
    dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% computeQuery()
  # Only one and more than one dose
  vaccinated_multiple <- vaccinated %>%
    dplyr::filter(seq != 1) %>% computeQuery()
  vaccinated_first <- vaccinated %>%
    dplyr::filter(seq == 1) %>% computeQuery()
  vaccinated_second <- vaccinated %>%
    dplyr::filter(seq == 2) %>% computeQuery()
  vaccinated_third <- vaccinated %>%
    dplyr::filter(seq == 3) %>% computeQuery()
  vaccinated_JJ <- cdm[["lcp_vaccinated"]] %>%
    dplyr::filter(cohort_definition_id == 3) %>%
    computeQuery()
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
    computeQuery()
  
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
    computeQuery()
  
  vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    computeQuery()
  vaccinated_second <- vaccinated_second %>% mutate(cohort_definition_id = 4) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    computeQuery()
  vaccinated_third <- vaccinated_third %>% mutate(cohort_definition_id = 5) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    computeQuery()
  
  vacc_all <- dplyr::union_all(vaccinated, nonvaccinated)
  vacc_all <- dplyr::union_all(vacc_all, vaccinated_first)
  vacc_all <- dplyr::union_all(vacc_all, vaccinated_second)
  vacc_all <- dplyr::union_all(vacc_all, vaccinated_third)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = VaccCohortsName,
                                             cohort_definition_id = c(1:5), 
                                             cohort_name = c("vaccinated", "not_vaccinated", "first_dose", "second_dose", "third_dose")))
  
  # Save attributes of the cohort
  attr(vacc_all, "cohort_set") <- names_final_cohorts %>% 
    dplyr::filter(table_name == VaccCohortsName) %>%
    dplyr::select(cohort_definition_id, cohort_name) 
  
  attr(vacc_all, "cohort_count") <- getCohortCount(vacc_all)
  
  cdm[[VaccCohortsName]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(vacc_all, VaccCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = insertTable(attr(vacc_all, "cohort_set"), cdm, paste0(VaccCohortsName, "_set")),
    cohortCountRef = insertTable(attr(vacc_all, "cohort_count"), cdm, paste0(VaccCohortsName, "_count"))
  )
  
}

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS
# Overlapping cohorts of single symptoms with base cohorts

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName))
}

message("Getting overlap cohorts")
info(logger, '-- Getting overlap cohorts')

if(cdm[[LongCovidCohortsName]] %>% 
   dplyr::filter(cohort_definition_id == 1) %>% tally() %>% pull() > 5) {
  overlapip <- do_overlap(cdm, 1, 1, 1, tableName = LongCovidCohortsName)
}
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsName,
                                           cohort_definition_id = 1, 
                                           cohort_name =paste0("inf_",Initial_cohorts$cohort_name[2])))

base_ids <- c(1)
outcome_ids <- c(2:25)
for(i in base_ids) {
  for(j in outcome_ids){
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsName,
                                               cohort_definition_id = j, 
                                               cohort_name = paste0("inf_",Initial_cohorts$cohort_name[j+1])))
    if(cdm[[LongCovidCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      overlapip_w <- do_overlap(cdm, i, j, j, tableName = LongCovidCohortsName)
      overlapip <- dplyr::union_all(overlapip, overlapip_w) %>%
        computeQuery()
    }
  }
}

if(cdm[[LongCovidCohortsName]] %>% 
   dplyr::filter(cohort_definition_id == 26) %>% tally() %>% pull() > 5) {
  overlapip_w <- do_overlap(cdm, 1, 26, 26, tableName = LongCovidCohortsName)
  overlapip <- dplyr::union_all(overlapip, overlapip_w) %>%
    computeQuery()
}

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsName,
                                           cohort_definition_id = 26, 
                                           cohort_name =paste0("inf_",Initial_cohorts$cohort_name[32])))

# Save attributes of the cohort
attr(overlapip, "cohort_set") <- names_final_cohorts %>% 
  dplyr::filter(table_name == OverlapCohortsName) %>%
  dplyr::select(cohort_definition_id, cohort_name) 

attr(overlapip, "cohort_count") <- getCohortCount(overlapip)

cdm[[OverlapCohortsName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(overlapip, OverlapCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
  cohortSetRef = insertTable(attr(overlapip, "cohort_set"), cdm, paste0(OverlapCohortsName, "_set")),
  cohortCountRef = insertTable(attr(overlapip, "cohort_count"), cdm, paste0(OverlapCohortsName, "_count"))
)

if(vaccine_data) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,OverlapCohortsName))
} else {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     OverlapCohortsName))
}

# -------------------------------------------------------------------
# HEALTHCARE UTILISATION COHORTS
  # Get Healthcare Utilisation outcomes for clustering
hucohorts <- cdm[[InitialCohortsName]] %>%
  dplyr::filter(cohort_definition_id %in% c(33:36))
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = HUCohortsName,
                                             cohort_definition_id = c(1:4),
                                             cohort_name = Initial_cohorts$cohort_name[33:36]))
  
  # Save attributes of the cohort
  attr(hucohorts, "cohort_set") <- names_final_cohorts %>% 
    dplyr::filter(table_name == HUCohortsName) %>%
    dplyr::select(cohort_definition_id, cohort_name) 
  
  attr(hucohorts, "cohort_count") <- getCohortCount(hucohorts)
  
  cdm[[HUCohortsName]] <- newGeneratedCohortSet(
    cohortRef = computeQuery(hucohorts, HUCohortsName, FALSE, attr(cdm, "write_schema"), TRUE),
    cohortSetRef = insertTable(attr(hucohorts, "cohort_set"), cdm, paste0(HUCohortsName, "_set")),
    cohortCountRef = insertTable(attr(hucohorts, "cohort_count"), cdm, paste0(HUCohortsName, "_count"))
  )
  

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

if(vaccine_data) {
  cdm <- cdm_from_con(db, cdm_database_schema, write_schema = results_database_schema,
                    cohort_tables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,OverlapCohortsName, HUCohortsName),
                    cdm_name = db.name)
} else {
  cdm <- cdm_from_con(db, cdm_database_schema, write_schema = results_database_schema,
                    cohort_tables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
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
# GET cohort for clustering: subject_id, symptoms (1 or 0), age, sex, HU characteristics, co-morbidities, vaccination

# First get all people with LC symptoms (overlap with infection cohort at base)
symptoms_LC <- cdm[[OverlapCohortsName]] %>% 
  dplyr::filter(.data$cohort_definition_id %in% c(1:26))

names_symptoms <- names_final_cohorts %>% 
  dplyr::filter(.data$table_name == LongCovidCohortsName) %>%
  dplyr::filter(.data$cohort_definition_id %in% c(1:26)) %>%
  dplyr::select(cohort_definition_id, cohort_name) %>% 
  dplyr::distinct() %>% computeQuery()

symptoms_LC <- symptoms_LC %>% addDemographics(cdm, futureObservation = FALSE, priorHistory = FALSE) %>% 
  computeQuery()
symptoms_LC <- symptoms_LC %>% 
  dplyr::left_join(names_symptoms, 
                   by = c("cohort_definition_id"), copy = TRUE) %>%
  computeQuery()

# Get the names of the symptoms or LC code
names_symptoms <- Initial_cohorts$cohort_name[c(2:26,32)]

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
data_LCA <- data_LCA %>% 
  dplyr::mutate(dplyr::across(colnames(data_LCA), ~ifelse(is.na(.x), 0, .x))) %>%
  computeQuery()
for(n in names_symptoms) {
  if(!(n %in% colnames(data_LCA))) {
    n <- enquo(n)
    data_LCA <- data_LCA %>% 
      dplyr::mutate(!!n := c(0))
  }
}
data_LCA <- data_LCA %>% distinct()
data_LCA <- data_LCA %>% dplyr::left_join(symptoms_LC %>% 
                                            dplyr::select(subject_id,age,sex,cohort_start_date, cohort_end_date, cohort_definition_id), 
                                          by = "subject_id") %>%
  dplyr::group_by(subject_id) %>%
  dplyr::filter(cohort_end_date == min(cohort_end_date)) %>%
  dplyr::filter(cohort_definition_id == min(cohort_definition_id)) %>%
  distinct()

# Characterisation of the cluster people
cdm[["visit_occurrence"]] <- cdm[["visit_occurrence"]] %>%
  dplyr::filter(lubridate::year(.data$visit_start_date) >= 2016) %>%
  computeQuery()

ip.codes <- c(9201, 262)
# add all descendents
ip.codes.w.desc <- cdm$concept_ancestor %>%
  dplyr::filter(ancestor_concept_id  %in% ip.codes ) %>% 
  collect() %>% 
  dplyr::select(descendant_concept_id) %>% 
  dplyr::distinct() %>% 
  pull()

data_LCA <- data_LCA %>% 
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 1, window = list(c(-365, -1)), nameStyle = "number_icu") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 2, window = list(c(-365, -1)), nameStyle = "number_vent") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 3, window = list(c(-365, -1)), nameStyle = "number_trach") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 4, window = list(c(-365, -1)), nameStyle = "number_ecmo") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 1, window = list(c(1, 30)), nameStyle = "number_icu_next") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 2, window = list(c(1, 30)), nameStyle = "number_vent_next") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 3, window = list(c(1, 30)), nameStyle = "number_trach_next") %>%
  addCohortIntersectCount(cdm, targetCohortTable = HUCohortsName, targetCohortId = 4, window = list(c(1, 30)), nameStyle = "number_ecmo_next") %>%
  addNumberVisit(cdm, 9202, c(-365,-1), name = "number_gp_bef") %>% 
  addNumberVisit(cdm, 9202, c(1, 30), name = "number_gp_next") %>% 
  addNumberVisit(cdm, ip.codes.w.desc, c(-365,-1), name = "number_hosp_bef") %>% 
  addNumberVisit(cdm, ip.codes.w.desc, c(1,30), name = "number_hosp_next") %>% 
  computeQuery()

cohort_set <- tibble(
  ancestor_concept_id = c(
    434621, 317009, 443392, 201820, 321588, 316866, 4030518, 255573, 4182210
  ),
  cohort_definition_id = c(1:9),
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
  
  data_LCA <- data_LCA %>% 
    addFlagEvent_in(cdm, "condition_occurrence", window = c(-NA,-1), filter = list(condition_concept_id = ip.codes.w.desc), eventDate = "condition_start_date", name = cohort_set$cohort_name[i]) %>%
    computeQuery()
}



if(vaccine_data) {
  data_LCA <- data_LCA %>% 
    addCohortIntersectFlag(cdm, targetCohortTable = VaccCohortsName, targetCohortId = 3, window = list(c(-Inf, -1)), nameStyle = "first_dose") %>%
    addCohortIntersectFlag(cdm, targetCohortTable = VaccCohortsName, targetCohortId = 4, window = list(c(-Inf, -1)), nameStyle = "second_dose") %>%
    addCohortIntersectFlag(cdm, targetCohortTable = VaccCohortsName, targetCohortId = 5, window = list(c(-Inf, -1)), nameStyle = "third_dose") %>%
    computeQuery()
}

computeQuery(data_LCA, name = clusterCohortName,  temporary = FALSE, schema = results_database_schema, overwrite = TRUE)

