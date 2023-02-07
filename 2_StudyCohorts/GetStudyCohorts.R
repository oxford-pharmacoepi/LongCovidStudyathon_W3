# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# SHOULD PUT LOGGER

# get functions used throughout this script
source(here("2_StudyCohorts","functions_getCohorts.R"))

# observation period + death table
observation_death <- cdm$observation_period %>%
  dplyr::select("subject_id" = "person_id", "observation_period_end_date") %>%
  left_join(cdm$death %>% dplyr::select("subject_id" = "person_id", "death_date"), by = "subject_id") %>%
  mutate(death = ifelse(!(is.na(death_date)), 1,0)) %>%
  compute()

# ---------------------------------------------------------------------
# BASE COHORTS

message("Getting base cohorts")

# QUESTION Do we censor cohorts 1 and 2 for COVID?

# Get initial cohorts to build the study cohorts
info(logger, 'GETTING BASE COHORTS')
newinf_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 1) %>% dplyr::select(
    "subject_id",
    "cohort_start_date"
  ) %>% compute() 

negative_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 3) %>% dplyr::select(
    "subject_id",
    "cohort_start_date"
  ) %>% compute() 

censorcovid_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "subject_id",
    "cohort_start_date"
  ) %>% compute() 

influenza_init <- cdm[[cohort_table_name]] %>% 
  dplyr::filter(.data$cohort_definition_id == 4) %>% dplyr::select(
    "subject_id",
    "cohort_start_date"
  ) %>% compute() 

covid <- do_exclusion(newinf_init, id = 1, "cohort_start_date", S_start_date = study_start_date)
nocovid <- do_exclusion(negative_init, id = 3, "cohort_start_date", S_start_date = study_start_date)
influenza <- do_exclusion(influenza_init, id = 4, "cohort_start_date", S_start_date = as.Date("2017-09-01"))
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% mutate(cohort_definition_id = 1) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% mutate(cohort_definition_id = 2) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Tested negative "final"
negativetest <- nocovid[[1]]
negativetest <- negativetest %>% mutate(cohort_definition_id = 3) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Influenza
flu <- influenza[[1]]
flu <- flu %>% mutate(cohort_definition_id = 4) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% mutate(cohort_definition_id = 1)
attrition_negative <- nocovid[[3]]
attrition_negative <- attrition_negative %>% mutate(cohort_definition_id = 3)
attrition_flu <- influenza[[3]]
attrition_flu <- attrition_flu %>% mutate(cohort_definition_id = 4)
attrition <- rbind(attrition_positive,attrition_negative,attrition_flu)

computePermanent(new_infection, name = "studyathon_final_cohorts",  schema = results_database_schema, overwrite = TRUE)
appendPermanent(reinfection, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(negativetest, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(flu, name = "studyathon_final_cohorts",  schema = results_database_schema)

write.csv(
  attrition,
  file = here::here(output.folder, "attrition_studies.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")

# QUESTION Washout for outcome and intersection? Annika: all for PASC/MC, 180(intersection)/365(alone) for symptoms
# QUESTION How to treat repeated events, both for Characterisation and IP?
# QUESTION Do we use these cohorts without overlap for anything?

# KNOW All PASC events have 90d washout

# Long covid symptoms
create_outcome(window = c(5:29), filter_start = FALSE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Any LC symptom
crate_any_cohort(c(5:29), 100)

# LC code?
# id 101

# PASC events
create_outcome(window = c(30:39))

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Any PASC event
crate_any_cohort(c(30:39), 102)

# Medical conditions
create_outcome(window = c(40:59))

# ---------------------------------------------------------------------
# STRATA COHORTS

message("Getting strata cohorts")

# QUESTION How we define vaccinated strata (any/brands, doses...)?

# Vaccinated people
create_outcome(window = c(60:63))
crate_any_vacc_cohort(c(60:63), 103)

# Update cdm
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS CHARACTERISATION

message("Getting overlapping cohorts")

# LC any symptom + Infection
do_overlap(1, 100, 105)

# LC any symptom + Reinfection
do_overlap(2, 100, 106)

# LC any symptom + Test negative
do_overlap(3, 100, 107)

# LC any symptom + Influenza
do_overlap(4, 100, 108)

# LC code + Infection
# id 108

# LC code + Reinfection
# id 109

# LC code + Test negative
# id 110

# LC code + Influenza
# id 111

# PASC any symptom + Infection
do_overlap(1, 102, 113, washout = FALSE)

# PASC any symptom + Reinfection
do_overlap(2, 102, 114, washout = FALSE)

# PASC any symptom + Test negative
do_overlap(3, 102, 115, washout = FALSE)

# PASC any symptom + Influenza
do_overlap(4, 102, 116, washout = FALSE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Base cohorts plus vaccination
do_overlap_vacc(1,117)
do_overlap_vacc(2,119)
do_overlap_vacc(3,121)
do_overlap_vacc(3,123)

# -------------------------------------------------------------------

# Think: do we need overlapping cohorts of single symptoms + base? Or medical conditions + base? 
# If so, id base*100 + medical/symptom -> e.g. cough and influenza is 412
