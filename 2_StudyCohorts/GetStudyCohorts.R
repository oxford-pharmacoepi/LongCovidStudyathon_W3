# From initial cohorts, get base and outcome cohorts for the study
# Get attrition too

# LC any overlap cohort must be done separately: joining washed-out individually LC symptoms, 
# not washing out on the LC any cohort. The rest whould be fine
# Also, washout only outcomes?
#K

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
info(logger, '-- Getting base cohorts')

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

covid <- do_exclusion(cdm, newinf_init, id = 1, "cohort_start_date", S_start_date = study_start_date, covidcensor = FALSE)
nocovid <- do_exclusion(cdm, negative_init, id = 3, "cohort_start_date", S_start_date = study_start_date)
influenza <- do_exclusion(cdm, influenza_init, id = 4, "cohort_start_date", S_start_date = as.Date("2017-09-01"))
  
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

# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% mutate(cohort_definition_id = 1)
attrition_censor_negative <- nocovid[[4]]
attrition_censor_negative <- attrition_censor_negative %>% mutate(cohort_definition_id = 3)
attrition_censor_flu <- influenza[[4]]
attrition_censor_flu <- attrition_censor_flu %>% mutate(cohort_definition_id = 4)
attrition_censor <- rbind(attrition_censor_positive,attrition_censor_negative,attrition_censor_flu)

computePermanent(new_infection, name = "studyathon_final_cohorts",  schema = results_database_schema, overwrite = TRUE)
appendPermanent(reinfection, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(negativetest, name = "studyathon_final_cohorts",  schema = results_database_schema)
appendPermanent(flu, name = "studyathon_final_cohorts",  schema = results_database_schema)

write.csv(
  attrition,
  file = file.path(tempDir, "attrition_studies.csv"),
  row.names = FALSE
)
write.csv(
  attrition_censor,
  file = file.path(tempDir, "attrition_censoring_studies.csv"),
  row.names = FALSE
)

names_final_cohorts <- dplyr::tibble(cohortId = c(1:4), cohortName = c("Infection","Reinfection","Test negative","Influenza"))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
create_outcome(cdm, window = c(5:29), filter_start = FALSE, first_event = FALSE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = c(5:59), cohortName = initialCohortSet$cohortName[5:59]))

# Any LC symptom
create_any_cohort(cdm, c(5:29), 100, LC = TRUE)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = 100, cohortName = "Any LC symptom"))

# LC code
# id 101

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = 101, cohortName = "LC code"))

# PASC events
create_outcome(cdm, window = c(30:39))

# Any PASC event
create_any_cohort(cdm, c(30:39), 102)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = 102, cohortName = "Any PASC event"))

# Medical conditions
create_outcome(cdm, window = c(40:59))

# ---------------------------------------------------------------------
# STRATA COHORTS

message("Getting strata cohorts")
info(logger, '-- Getting strata cohorts')

# Vaccinated people
if(vaccine_data) {
  if(vaccine_brand) {
    vaccinated <- cdm[[cohort_table_name]] %>%
      dplyr::filter(cohort_definition_id %in% c(60:63)) %>%
      dplyr::select(
        "subject_id",
        "cohort_start_date",
        "cohort_definition_id"
      ) %>% group_by(subject_id) %>% arrange(.data$cohort_start_date) %>%
      mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
    # Only one and more than one dose
    vaccinated_multiple <- vaccinated %>%
      dplyr::filter(seq != 1) %>% compute()
    vaccinated_first <- vaccinated %>%
      dplyr::filter(seq == 1) %>% compute()
    vaccinated_second <- vaccinated %>%
      dplyr::filter(seq == 2) %>% compute()
    vaccinated_third <- vaccinated %>%
      dplyr::filter(seq == 3) %>% compute()
    vaccinated_JJ <- vaccinated_first %>% dplyr::filter(cohort_definition_id == 61) %>%
      compute()
    # Cohort fully vaccinated (one JJ dose or two any doses), add 14 days to vaccination day for full coverage
    vaccinated <- vaccinated_JJ %>% dplyr::union(vaccinated_multiple) %>%
      dplyr::rename(vacc_date = cohort_start_date) %>%
      dplyr::group_by(subject_id) %>%
      dplyr::summarise(
        cohort_start_date = min(vacc_date, na.rm = TRUE)
      ) %>% dplyr::mutate(cohort_definition_id = 103) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
      mutate(cohort_start_date = cohort_start_date + lubridate::days(14)) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
    nonvaccinated <- cdm$person %>% mutate(subject_id = person_id) %>% 
      left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
                by = "subject_id") %>% mutate(cohort_definition_id = 104) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date, vacc_date))) %>%
      left_join(cdm$observation_period %>% dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
                by = c("subject_id" = "person_id")) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    vaccinated_first <- vaccinated_first %>% mutate(cohort_definition_id = 350) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_second <- vaccinated_second %>% mutate(cohort_definition_id = 351) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    vaccinated_third <- vaccinated_third %>% mutate(cohort_definition_id = 352) %>%
      left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      dplyr::compute()
    
    appendPermanent(vaccinated, name = "studyathon_final_cohorts",  schema = results_database_schema)
    appendPermanent(nonvaccinated, name = "studyathon_final_cohorts",  schema = results_database_schema)
    appendPermanent(vaccinated_first, name = "studyathon_final_cohorts",  schema = results_database_schema)
    appendPermanent(vaccinated_second, name = "studyathon_final_cohorts",  schema = results_database_schema)
    appendPermanent(vaccinated_third, name = "studyathon_final_cohorts",  schema = results_database_schema)
    
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(cohortId = c(103,104,350:352), 
                                               cohortName = c("Vaccinated", "Not vaccinated", "First dose", "Second dose", "Third dose")))
    
  } else {
    # What to do with people who have no brand data? We must build some different vaccination cohort!!
    #K
  }
}

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS CHARACTERISATION

message("Getting overlapping cohorts")
info(logger, '-- Getting overlapping cohorts')

# LC any symptom + Infection
do_overlap(cdm, 1, 100, 105)

# LC any symptom + Reinfection
do_overlap(cdm, 2, 100, 106)

# LC any symptom + Test negative
do_overlap(cdm, 3, 100, 107)

# LC any symptom + Influenza
do_overlap(cdm, 4, 100, 108)

# LC code + Infection
# id 109

# LC code + Reinfection
# id 110

# LC code + Test negative
# id 111

# LC code + Influenza
# id 112

# PASC any symptom + Infection
do_overlap(cdm, 1, 102, 113, washout = FALSE)

# PASC any symptom + Reinfection
do_overlap(cdm, 2, 102, 114, washout = FALSE)

# PASC any symptom + Test negative
do_overlap(cdm, 3, 102, 115, washout = FALSE)

# PASC any symptom + Influenza
do_overlap(cdm, 4, 102, 116, washout = FALSE)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = c(105:116), 
                                           cohortName = c("LC any + inf","LC any + reinf","LC any + neg", "LC any + flu",
                                                          "LC code + inf","LC code + reinf","LC code + neg","LC code + flu",
                                                          "PASC any + inf","PASC any + reinf","PASC any + neg","PASC any + flu")))

# Base cohorts plus vaccination or non vaccination
do_overlap_vacc(cdm, 1,117)
do_overlap_vacc(cdm, 2,119)
do_overlap_vacc(cdm, 3,121)
do_overlap_vacc(cdm, 4,123)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = c(117:124), 
                                           cohortName = c("Inf + vacc","Inf + not vacc",
                                                          "Reinf + vacc","Reinf + not vacc",
                                                          "Neg + vacc", "Neg + not vacc",
                                                          "Flu + vacc", "Flu + not vacc")))


# Overlapping cohorts of single symptoms / events / medical conditions with base cohorts
# Cohort_definition if is id_base*200 + id_medical/symptom -> e.g. cough and influenza is 812
base_ids <- c(1:4)
outcome_ids <- c(5:59)
for(i in base_ids) {
  for(j in outcome_ids){
    do_overlap(cdm, i, j, i*200+j)
    names_final_cohorts <- rbind(names_final_cohorts,
                                 dplyr::tibble(cohortId = i*200+j, 
                                               cohortName =paste0("Base ",i," outcome ",j) ))
    
  }
}


# Overlapping cohorts of any symptom LC or any PASC or LC code plus base cohort (i.e. characterisation cohorts) stratified by vaccination
do_overlap_vacc(cdm,105,900)
do_overlap_vacc(cdm,106,902)
do_overlap_vacc(cdm,107,904)
do_overlap_vacc(cdm,108,906)
do_overlap_vacc(cdm,109,908)
do_overlap_vacc(cdm,110,910)
do_overlap_vacc(cdm,111,912)
do_overlap_vacc(cdm,112,914)
do_overlap_vacc(cdm,113,916)
do_overlap_vacc(cdm,114,918)
do_overlap_vacc(cdm,115,920)
do_overlap_vacc(cdm,116,922)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohortId = c(900:923), 
                                           cohortName = c("Inf + LC any + vacc",
                                                          "Inf + LC any + not vacc",
                                                          "Reinf + LC any + vacc",
                                                          "Reinf + LC any + not vacc",
                                                          "Neg + LC any + vacc", 
                                                          "Neg + LC any + not vacc",
                                                          "Flu + LC any + vacc", 
                                                          "Flu + LC any + not vacc",
                                                          "Inf + LC code + vacc",
                                                          "Inf + LC code + not vacc",
                                                          "Reinf + LC code + vacc",
                                                          "Reinf + LC code + not vacc",
                                                          "Neg + LC code + vacc", 
                                                          "Neg + LC code + not vacc",
                                                          "Flu + LC code + vacc", 
                                                          "Flu + LC code + not vacc",
                                                          "Inf + PASC any + vacc",
                                                          "Inf + PASC any + not vacc",
                                                          "Reinf + PASC any + vacc",
                                                          "Reinf + PASC any + not vacc",
                                                          "Neg + PASC any + vacc", 
                                                          "Neg + PASC any + not vacc",
                                                          "Flu + PASC any + vacc", 
                                                          "Flu + PASC any + not vacc") ))

# Strata for calendar period, for base cohorts and any symptom LC or any PASC or LC code plus base cohort (i.e. characterisation cohorts)
do_strata_calendar(cdm,1,924)
do_strata_calendar(cdm,2,926)
do_strata_calendar(cdm,3,928)
do_strata_calendar(cdm,4,930)
do_strata_calendar(cdm,105,932)
do_strata_calendar(cdm,106,934)
do_strata_calendar(cdm,107,936)
do_strata_calendar(cdm,108,938)
do_strata_calendar(cdm,109,940)
do_strata_calendar(cdm,110,942)
do_strata_calendar(cdm,111,944)
do_strata_calendar(cdm,112,946)
do_strata_calendar(cdm,113,948)
do_strata_calendar(cdm,114,950)
do_strata_calendar(cdm,115,952)
do_strata_calendar(cdm,116,954)

# -------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

# put names of cohorts: create tibble and append numbers and names, etc.

finalCounts <- cdm[["studyathon_final_cohorts"]] %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  right_join(names_final_cohorts, by = c("cohort_definition_id"="cohortId")) %>% 
  mutate(n = as.numeric(n)) %>% mutate(n = if_else(is.na(n), 0, n)) %>% mutate(n = ifelse(n <= 5, NA, n)) %>% dplyr::select(cohortName,n)

# Export csv
write.csv(finalCounts,
          file = file.path(tempDir,
            paste0(db.name,"_finalcounts.csv")
          ),
          row.names = FALSE
)
