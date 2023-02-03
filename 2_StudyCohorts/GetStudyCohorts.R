# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# BE IN OBSERVATION AT COHORT ENTRY!!!!!!!!!!!

# ---------------------------------------------------------------------
# BASE COHORTS

# Get initial cohorts to build the study cohorts
info(logger, 'GETTING BASE COHORTS')
newinf_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 1) %>% dplyr::select(
    "person_id" = "subject_id",
    "cohort_start_date"
  ) %>% compute() 

negative_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 3) %>% dplyr::select(
    "person_id" = "subject_id",
    "cohort_start_date"
  ) %>% compute() 

censorcovid_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "person_id" = "subject_id",
    "cohort_start_date"
  ) %>% compute() 

influenza_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 4) %>% dplyr::select(
    "person_id" = "subject_id",
    "cohort_start_date"
  ) %>% compute() 

covid <- do_exclusion(newinf_init, id = 1, "cohort_start_date", S_start_date = study_start_date)
nocovid <- do_exclusion(negative_init, id = 3, "cohort_start_date", S_start_date = study_start_date)
influenza <- do_exclusion(influenza_init, id = 4, "cohort_start_date", S_start_date = as.Date("2017-09-01"))
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% mutate(cohort_definition_id = 1) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% mutate(cohort_definition_id = 2) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Tested negative "final"
negativetest <- nocovid[[1]]
negativetest <- negativetest %>% mutate(cohort_definition_id = 3) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Influenza
flu <- influenza[[1]]
flu <- flu %>% mutate(cohort_definition_id = 4) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% mutate(cohort_definition_id = 1)
attrition_negative <- nocovid[[3]]
attrition_negative <- attrition_negative %>% mutate(cohort_definition_id = 3)
attrition_flu <- influenza[[3]]
attrition_flu <- attrition_flu %>% mutate(cohort_definition_id = 4)

computePermanent(new_infection, name = "studyathon_final_cohorts",  schema = results_database_schema, overwrite = TRUE)
appendPermanent(reinfection, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(negativetest, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(flu, name = "studyathon_final_cohorts",  schema = results_database_schema)

# This might not have a good structure, THINK / or just have csv already in results
#computePermanent(attrition_positive, name = "studyathon_attrition",  schema = results_database_schema, overwrite = TRUE)
#appendPermanent(attrition_negative, name = "studyathon_attrition",  schema = results_database_schema)
#appendPermanent(attrition_flu, name = "studyathon_attrition",  schema = results_database_schema)

# ---------------------------------------------------------------------
# OUTCOME COHORTS
# Long covid symptoms

# Can take this function out to make it neater!!

observation_death <- cdm$observation_period %>%
  select(person_id, observation_period_end_date) %>%
  left_join(cdm$death %>% select (person_id, death_date), by = "person_id") %>%
  mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
  compute()

for(i in c(5:29)){
  name_cohort <- initialCohortSet$cohortName[i]
  current_symptom <- cdm[[cohort_table_name]] %>% 
    dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date"
    ) %>% compute() 
  
  # How to deal with repeated events??
  # current_symptom <- current_symptom %>% group_by(person_id) %>% arrange(cohort_start_date) %>%
  #  mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  current_symptom <- current_symptom %>% left_join(observation_death, by = c("person_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>% compute()
    
  current_symptom <- current_symptom %>% mutate(cohort_definition_id = i) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  appendPermanent(current_symptom, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  #first_event <- cough %>% filter(seq == 1) %>% select(-seq) %>% compute()
  #subs_events <- cough %>% filter(seq != 1) %>% select(-seq) %>% compute()
}
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Any LC symptom
symptoms_cohorts <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% c(5:29))
# people (381170) or events (441641)?
any_symp_cohort <- symptoms_cohorts %>% select(-cohort_definition_id) %>% mutate(cohort_definition_id = 100) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
    # group_by(person_id, symptom_date) %>%
    # keep date of first symptom recorded
    # filter(cohort_end_date == min(cohort_end_date)) %>%
    # ungroup()
appendPermanent(any_symp_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)

# LC code???
# id 101

# PASC events
for(i in c(30:39)){
  name_cohort <- initialCohortSet$cohortName[i]
  current_event <- cdm[[cohort_table_name]] %>% 
    dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date"
    ) %>% compute() 
  current_event <- current_event %>% filter(cohort_start_date > study_start_date) %>% compute()
  
  # current_event <- current_event %>% group_by(person_id) %>% arrange(cohort_start_date) %>%
  #  mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  
  current_event <- current_event %>% left_join(observation_death, by = c("person_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) 
  %>% compute()
  
  current_event <- current_event %>% mutate(cohort_definition_id = i) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  appendPermanent(current_event, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  #first_event <- cough %>% filter(seq == 1) %>% select(-seq) %>% compute()
  #subs_events <- cough %>% filter(seq != 1) %>% select(-seq) %>% compute()
}
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Any PASC event
events_cohorts <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% c(30:39))
any_pasc_cohort <- events_cohorts %>% select(-cohort_definition_id) %>% mutate(cohort_definition_id = 102) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
# group_by(person_id, symptom_date) %>%
# keep date of first symptom recorded
# filter(cohort_end_date == min(cohort_end_date)) %>%
# ungroup()
appendPermanent(any_pasc_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)


# Medical conditions
for(i in c(40:59)){
  name_cohort <- initialCohortSet$cohortName[i]
  current_condition <- cdm[[cohort_table_name]] %>% 
    dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date"
    ) %>% compute() 
  current_condition <- current_condition %>% filter(cohort_start_date > study_start_date) %>% compute()
  
  # current_condition <- current_condition %>% group_by(person_id) %>% arrange(cohort_start_date) %>%
  #  mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  
  current_condition <- current_condition %>% left_join(observation_death, by = c("person_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) 
  %>% compute()
  
  current_condition <- current_condition %>% mutate(cohort_definition_id = i) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  appendPermanent(current_condition, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  #first_event <- cough %>% filter(seq == 1) %>% select(-seq) %>% compute()
  #subs_events <- cough %>% filter(seq != 1) %>% select(-seq) %>% compute()
}

# ---------------------------------------------------------------------
# STRATA COHORTS
# Vaccinated people
for(i in c(60:63)){
  name_cohort <- initialCohortSet$cohortName[i]
  current_condition <- cdm[[cohort_table_name]] %>% 
    dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date"
    ) %>% compute() 
  current_condition <- current_condition %>% filter(cohort_start_date > study_start_date) %>% compute()
  
  current_condition <- current_condition %>% group_by(person_id) %>% arrange(cohort_start_date) %>%
    mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  
  current_condition <- current_condition %>% mutate(cohort_definition_id = i) %>% mutate(cohort_end_date = cohort_start_date + lubridate::days(365)) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  appendPermanent(current_condition, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  #first_event <- cough %>% filter(seq == 1) %>% select(-seq) %>% compute()
  #subs_events <- cough %>% filter(seq != 1) %>% select(-seq) %>% compute()
}
vacc_cohorts <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% c(60:63))
any_vacc_cohort <- vacc_cohorts %>% select(-cohort_definition_id) %>% mutate(cohort_definition_id = 103) %>% select(person_id,cohort_definition_id,cohort_start_date,cohort_end_date)
# group_by(person_id, symptom_date) %>%
# keep date of first symptom recorded
# filter(cohort_end_date == min(cohort_end_date)) %>%
# ungroup()


# What kind of exclusion or censoring?

appendPermanent(any_vacc_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)

# Update cdm
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS CHARACTERISATION

# LC any symptom + Infection
do_overlap(1, 100, 104)

# LC any symptom + Reinfection
do_overlap(2, 100, 105)

# LC any symptom + Test negative
do_overlap(3, 100, 106)

# LC any symptom + Influenza
do_overlap(4, 100, 107)

# LC code + Infection
# id 108

# LC code + Reinfection
# id 109

# LC code + Test negative
# id 110

# LC code + Influenza
# id 111

# PASC any symptom + Infection
do_overlap(1, 102, 112, washout = FALSE)

# PASC any symptom + Reinfection
do_overlap(2, 102, 113, washout = FALSE)

# PASC any symptom + Test negative
do_overlap(3, 102, 114, washout = FALSE)

# PASC any symptom + Influenza
do_overlap(4, 102, 115, washout = FALSE)


# -------------------------------------------------------------------

# Think: do we need overlapping cohorts of single symptoms + base? Or medical conditions + base? 
# If so, id base*100 + medical/symptom -> e.g. cough and influenza is 412

# appendPermanent(working_symptom, name = "er_long_covid_final_cohorts",  schema = write_schema)
