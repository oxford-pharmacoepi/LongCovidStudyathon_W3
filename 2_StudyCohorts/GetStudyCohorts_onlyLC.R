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

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% dplyr::mutate(cohort_definition_id = 2) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()
attrition <- rbind(attrition_positive)

# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% dplyr::mutate(cohort_definition_id = 1) %>%
  compute()
attrition_censor <- rbind(attrition_censor_positive)

computeQuery(new_infection,
             name = BaseCohortsName,
             temporary = FALSE,
             schema = results_database_schema,
             overwrite = TRUE)
appendPermanent(reinfection, name = BaseCohortsName,
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
                                     cohort_definition_id = c(1:2),
                                     cohort_name = c("Infection","Reinfection"))

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
      ) %>% dplyr::group_by(subject_id) %>% dplyr::arrange(.data$cohort_start_date) %>%
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
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date, .data$death_date))) %>%
      dplyr::mutate(cohort_start_date = .data$cohort_start_date + lubridate::days(14)) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
    nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
      dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                       by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date, .data$vacc_date))) %>%
      dplyr::left_join(cdm$observation_period %>% 
                         dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
                       by = c("subject_id" = "person_id")) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
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
      ) %>% dplyr::group_by(subject_id) %>% dplyr::arrange(.data$cohort_start_date) %>%
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
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date, death_date))) %>%
      dplyr::mutate(cohort_start_date = cohort_start_date + lubridate::days(14)) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
    nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
      dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                       by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date, .data$vacc_date))) %>%
      dplyr::left_join(cdm$observation_period %>% 
                         dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
                       by = c("subject_id" = "person_id")) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_second <- vaccinated_second %>% dplyr::mutate(cohort_definition_id = 4) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_third <- vaccinated_third %>% dplyr::mutate(cohort_definition_id = 5) %>%
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date))) %>%
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
    ) %>% dplyr::group_by(subject_id) %>% arrange(.data$cohort_start_date) %>%
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
    dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    dplyr::mutate(cohort_start_date = cohort_start_date + lubridate::days(14)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  
  # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
  nonvaccinated <- cdm$person %>% dplyr::mutate(subject_id = person_id) %>% 
    dplyr::left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                     by = "subject_id") %>% dplyr::mutate(cohort_definition_id = 2) %>%
    dplyr::left_join(observation_death, by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(.data$observation_period_end_date, .data$death_date, .data$vacc_date))) %>%
    left_join(cdm$observation_period %>% 
                dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
              by = c("subject_id" = "person_id")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  
  vaccinated_first <- vaccinated_first %>% dplyr::mutate(cohort_definition_id = 3) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_second <- vaccinated_second %>% mutate(cohort_definition_id = 4) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(.data$cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_third <- vaccinated_third %>% mutate(cohort_definition_id = 5) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
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

# LC any symptom + Infection / Reinfection
do_overlap_LCany(cdm, c(1:2), c(5:29), c(1:2))

# LC code + Infection
do_overlap(cdm, 1, 27, 3, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)

# LC code + Reinfection
do_overlap(cdm, 2, 27, 4, washout = FALSE, tableName = LongCovidCohortsName,
           overlapTableName = OverlapCohortsCName)


cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                   VaccCohortsName,OverlapCohortsCName))

# Characterisation cohorts stratified by sex
do_sex_strata(1, 3, BaseCohortsName)
do_sex_strata(2, 5, BaseCohortsName)

do_sex_strata(1, 5, OverlapCohortsCName)
do_sex_strata(2, 7, OverlapCohortsCName)
do_sex_strata(3, 9, OverlapCohortsCName)
do_sex_strata(4, 11, OverlapCohortsCName)


names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(3:6), 
                                           cohort_name =c("Inf_females", "Inf_males",
                                                          "Reinf_females", "Reinf_males")))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(5:12), 
                                           cohort_name =c("Inf_LC_Any_females", "Inf_LC_Any_males",
                                                          "Reinf_LC_Any_females", "Reinf_LC_Any_males",
                                                          "Inf_LC_Code_females", "Inf_LC_Code_males",
                                                          "Reinf_LC_Code_females", "Reinf_LC_Code_males")))


# Characterisation cohorts stratified by age
do_age_strata(1, 7, BaseCohortsName)
do_age_strata(2, 15, BaseCohortsName)

do_age_strata(1, 13, OverlapCohortsCName)
do_age_strata(2, 21, OverlapCohortsCName)
do_age_strata(3, 29, OverlapCohortsCName)
do_age_strata(4, 37, OverlapCohortsCName)


names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(7:22), 
                                           cohort_name =c("Inf_age_(0,2)" , "Inf_age_(3,5)", 
                                                          "Inf_age_(6,9)", "Inf_age_(10,13)",
                                                          "Inf_age_(14,17)", "Inf_age_(18,40)",
                                                          "Inf_age_(41,64)", "Inf_age_(65,120)",
                                                          "Reinf_age_(0,2)" , "Reinf_age_(3,5)", 
                                                          "Reinf_age_(6,9)", "Reinf_age_(10,13)",
                                                          "Reinf_age_(14,17)", "Reinf_age_(18,40)",
                                                          "Reinf_age_(41,64)", "Reinf_age_(65,120)"
                                                          )))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(13:44), 
                                           cohort_name =c("Inf_LC_any_age_(0,2)" , "Inf_LC_any_age_(3,5)", 
                                                          "Inf_LC_any_age_(6,9)", "Inf_LC_any_age_(10,13)",
                                                          "Inf_LC_any_age_(14,17)", "Inf_LC_any_age_(18,40)",
                                                          "Inf_LC_any_age_(41,64)", "Inf_LC_any_age_(65,120)",
                                                          "Reinf_LC_any_age_(0,2)" , "Reinf_LC_any_age_(3,5)", 
                                                          "Reinf_LC_any_age_(6,9)", "Reinf_LC_any_age_(10,13)",
                                                          "Reinf_LC_any_age_(14,17)", "Reinf_LC_any_age_(18,40)",
                                                          "Reinf_LC_any_age_(41,64)", "Reinf_LC_any_age_(65,120)",
                                                          "Inf_LC_code_age_(0,2)" , "Inf_LC_code_age_(3,5)", 
                                                          "Inf_LC_code_age_(6,9)", "Inf_LC_code_age_(10,13)",
                                                          "Inf_LC_code_age_(14,17)", "Inf_LC_code_age_(18,40)",
                                                          "Inf_LC_code_age_(41,64)", "Inf_LC_code_age_(65,120)",
                                                          "Reinf_LC_code_age_(0,2)" , "Reinf_LC_code_age_(3,5)", 
                                                          "Reinf_LC_code_age_(6,9)", "Reinf_LC_code_age_(10,13)",
                                                          "Reinf_LC_code_age_(14,17)", "Reinf_LC_code_age_(18,40)",
                                                          "Reinf_LC_code_age_(41,64)", "Reinf_LC_code_age_(65,120)"
                                                         )))



# Characterisation cohorts stratified by vaccination
do_overlap_vacc(1, 23, BaseCohortsName)
do_overlap_vacc(2, 25, BaseCohortsName)

do_overlap_vacc(1, 45, OverlapCohortsCName)
do_overlap_vacc(2, 47, OverlapCohortsCName)
do_overlap_vacc(3, 49, OverlapCohortsCName)
do_overlap_vacc(4, 51, OverlapCohortsCName)


names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(23:26), 
                                           cohort_name = c("Inf_vacc","Inf_not_vacc",
                                                           "Reinf_vacc","Reinf_not_vacc")))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(45:52), 
                                           cohort_name = c("Inf_LC_any_vacc",
                                                           "Inf_LC_any_not_vacc",
                                                           "Reinf_LC_any_vacc",
                                                           "Reinf_LC_any_not_vacc",
                                                           "Inf_LC_code_vacc",
                                                           "Inf_LC_code_not_vacc",
                                                           "Reinf_LC_code_vacc",
                                                           "Reinf_LC_code_not_vacc")))

# Characterisation cohorts stratified by calendar period
do_strata_calendar(1, 27, BaseCohortsName)
do_strata_calendar(2, 29, BaseCohortsName)

do_strata_calendar(1, 53, OverlapCohortsCName)
do_strata_calendar(2, 55, OverlapCohortsCName)
do_strata_calendar(3, 57, OverlapCohortsCName)
do_strata_calendar(4, 59, OverlapCohortsCName)


names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = BaseCohortsName,
                                           cohort_definition_id = c(27:30), 
                                           cohort_name =c("Inf_Delta", "Inf_Omicron",
                                                          "Reinf_Delta", "Reinf_Omicron")))
names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(table_name = OverlapCohortsCName,
                                           cohort_definition_id = c(53:60), 
                                           cohort_name =c("Inf_LC_Any_Delta", "Inf_LC_Any_Omicron",
                                                          "Reinf_LC_Any_Delta", "Reinf_LC_Any_Omicron",
                                                          "Inf_LC_Code_Delta", "Inf_LC_Code_Omicron",
                                                          "Reinf_LC_Code_Delta", "Reinf_LC_Code_Omicron")))

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
  
  for(i in c(2)) {
    do_overlap(cdm, i, 1, counter, tableName = LongCovidCohortsName,
               overlapTableName = OverlapCohortsIPName)
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(table_name = OverlapCohortsIPName,
                                               cohort_definition_id = counter, 
                                               cohort_name =paste0("Base_",i,"_LC_outcome_",1) ))
    counter <- counter + 1
  }
}

base_ids <- c(1:2)
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

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                   VaccCohortsName,OverlapCohortsCName,OverlapCohortsIPName))

# -------------------------------------------------------------------
# HEALTHCARE UTILISATION COHORTS
if(doCharacterisation || doClusteringLCA) {
  # Get Healthcare Utilisation outcomes for characterisation and clustering
  HU_cohorts <- CDMConnector::readCohortSet(here::here("4_Characterisation","HU_cohorts"))
  cdm <- CDMConnector::generateCohortSet(cdm, HU_cohorts,
                                         name = HUCohortsName,
                                         overwrite = TRUE)
  cdm[[HUCohortsName]] <- cdm[[HUCohortsName]] %>% left_join(observation_death, 
                                                             by = c("subject_id")) %>%
    dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date, death_date))) %>% 
    compute()
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = HUCohortsName,
                                             cohort_definition_id = c(1:4),
                                             cohort_name = HU_cohorts$cohort_name[1:4]))
  
}

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

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
