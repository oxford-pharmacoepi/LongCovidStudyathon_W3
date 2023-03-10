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

# ---------------------------------------------------------------------
# BASE COHORTS

message("Getting base cohorts")
info(logger, '-- Getting base cohorts')

# Get initial cohorts to build the study cohorts
newinf_init <- cdm[["studyathon_lcpasc"]] %>% 
  dplyr::filter(.data$cohort_definition_id == 1) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

negative_init <- cdm[["studyathon_lcpasc"]] %>% 
  dplyr::filter(.data$cohort_definition_id == 3) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

censorcovid_init <- cdm[["studyathon_lcpasc"]] %>% 
  dplyr::filter(.data$cohort_definition_id == 2) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

influenza_init <- cdm[["studyathon_lcpasc"]] %>% 
  dplyr::filter(.data$cohort_definition_id == 4) %>% dplyr::select(
    "subject_id",
    "cohort_definition_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %>% compute() 

covid <- do_exclusion(cdm, newinf_init, id = 1, "cohort_start_date",
                      S_start_date = study_start_date, covidcensor = FALSE)
nocovid <- do_exclusion(cdm, negative_init, id = 3, "cohort_start_date",
                        S_start_date = study_start_date)
influenza <- do_exclusion(cdm, influenza_init, id = 4, "cohort_start_date",
                          S_start_date = as.Date("2017-01-01"),
                          influenza = TRUE)
  
# New infection "final": inclusion/exclusion
new_infection <- covid[[1]]
new_infection <- new_infection %>% mutate(cohort_definition_id = 1) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Reinfection
reinfection <- covid[[2]]
reinfection <- reinfection %>% mutate(cohort_definition_id = 2) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Tested negative "final"
negativetest <- nocovid[[1]]
negativetest <- negativetest %>% mutate(cohort_definition_id = 3) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Influenza
flu <- influenza[[1]]
flu <- flu %>% mutate(cohort_definition_id = 4) %>%
  dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
  compute()

# Attritions
attrition_positive <- covid[[3]]
attrition_positive <- attrition_positive %>% mutate(cohort_definition_id = 1) %>%
  compute()
attrition_negative <- nocovid[[3]]
attrition_negative <- attrition_negative %>% mutate(cohort_definition_id = 3) %>%
  compute()
attrition_flu <- influenza[[3]]
attrition_flu <- attrition_flu %>% mutate(cohort_definition_id = 4) %>%
  compute()
attrition <- rbind(attrition_positive,attrition_negative,attrition_flu)

# Attritions for the censoring sub-part
attrition_censor_positive <- covid[[4]]
attrition_censor_positive <- attrition_censor_positive %>% mutate(cohort_definition_id = 1) %>%
  compute()
attrition_censor_negative <- nocovid[[4]]
attrition_censor_negative <- attrition_censor_negative %>% mutate(cohort_definition_id = 3) %>%
  compute()
attrition_censor_flu <- influenza[[4]]
attrition_censor_flu <- attrition_censor_flu %>% mutate(cohort_definition_id = 4) %>%
  compute()
attrition_censor <- rbind(attrition_censor_positive,attrition_censor_negative,attrition_censor_flu)

computeQuery(new_infection,
             name = "studyathon_final_cohorts",
             temporary = FALSE,
             schema = results_database_schema,
             overwrite = TRUE)
#computePermanent(new_infection, name = "studyathon_final_cohorts",
#                 schema = results_database_schema, overwrite = TRUE)
appendPermanent(reinfection, name = "studyathon_final_cohorts",
                schema = results_database_schema)
appendPermanent(negativetest, name = "studyathon_final_cohorts",
                schema = results_database_schema)
appendPermanent(flu, name = "studyathon_final_cohorts",
                schema = results_database_schema)

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

names_final_cohorts <- dplyr::tibble(cohort_definition_id = c(1:4),
                                     cohort_name = c("Infection","Reinfection","Test negative","Influenza"))

# ---------------------------------------------------------------------
# OUTCOME COHORTS

message("Getting outcome cohorts")
info(logger, '-- Getting outcome cohorts')

# Long covid symptoms
create_outcome(cdm, window = c(5:29), filter_start = FALSE, first_event = FALSE)

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts"))

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = c(5:59),
                                           cohort_name = initialCohortSet$cohort_name[5:59]))

# Any LC symptom
create_any_cohort(cdm, c(5:29), 100, LC = TRUE)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = 100, cohort_name = "Any LC symptom"))

# LC code
create_outcome(cdm, window = 64, 101)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = 101, cohort_name = "LC code"))

# PASC events
create_outcome(cdm, window = c(30:39))

# Any PASC event
create_any_cohort(cdm, c(30:39), 102)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = 102, cohort_name = "Any PASC event"))

# Medical conditions
create_outcome(cdm, window = c(40:59), end_outcome = FALSE)

# ---------------------------------------------------------------------
# STRATA COHORTS

message("Getting strata cohorts")
info(logger, '-- Getting strata cohorts')

#K
# Missingness in cohort_start_date and cohort_end_date in 103,350,351 and 352. Check

# Vaccinated people
if(vaccine_data && db.name != "CPRDGold") {
  if(vaccine_brand) {
    vaccinated <- cdm[["studyathon_lcpasc"]] %>%
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
      left_join(cdm$observation_period %>% 
                  dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
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
                                 dplyr::tibble(cohort_definition_id = c(103,104,350:352), 
                                               cohort_name = c("Vaccinated", "Not vaccinated", "First dose", "Second dose", "Third dose")))
    
  } else {
    # What to do with people who have no brand data? We must build some different vaccination cohort!!
    #K
  }
} else if(vaccine_data && db.name == "CPRDGold") {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts","longcovid_project_vaccinated_cohort"))
  
  vaccinated <- cdm[["longcovid_project_vaccinated_cohort"]] %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(
      "subject_id",
      "cohort_start_date",
      "cohort_definition_id"
    ) %>% group_by(subject_id) %>% arrange(.data$cohort_start_date) %>%
    dplyr::filter(!is.na(cohort_start_date)) %>%
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
  vaccinated_JJ <- cdm[["longcovid_project_vaccinated_cohort"]] %>%
    dplyr::filter(cohort_definition_id == 3) %>%
    compute()
  # Cohort fully vaccinated (one JJ dose or two any doses), add 14 days to vaccination day for full coverage
  vaccinated <- vaccinated %>% dplyr::mutate(cohort_definition_id = 103) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    mutate(cohort_start_date = cohort_start_date + lubridate::days(14)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  
  # Other individuals in the database who are not fully vaccinated: Only needed for stratification in IP source population part
  nonvaccinated <- cdm$person %>% mutate(subject_id = person_id) %>% 
    left_join(vaccinated %>% dplyr::select("vacc_date" = "cohort_start_date","subject_id"),
              by = "subject_id") %>% mutate(cohort_definition_id = 104) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date, vacc_date))) %>%
    left_join(cdm$observation_period %>% 
                dplyr::select("cohort_start_date" = "observation_period_start_date","person_id"),
              by = c("subject_id" = "person_id")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  
  vaccinated_first <- vaccinated_first %>% mutate(cohort_definition_id = 350) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_second <- vaccinated_second %>% mutate(cohort_definition_id = 351) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  vaccinated_third <- vaccinated_third %>% mutate(cohort_definition_id = 352) %>%
    left_join(observation_death, by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>%
    dplyr::filter(!is.na(cohort_end_date)) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    dplyr::compute()
  
  appendPermanent(vaccinated, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(nonvaccinated, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(vaccinated_first, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(vaccinated_second, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(vaccinated_third, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(cohort_definition_id = c(103,104,350:352), 
                                             cohort_name = c("Vaccinated", "Not vaccinated", "First dose", "Second dose", "Third dose")))
}

# ---------------------------------------------------------------------
# OVERLAPPING COHORTS CHARACTERISATION

message("Getting overlapping cohorts")
info(logger, '-- Getting overlapping cohorts')

# LC any symptom + Infection / Reinfection / Test negative / Influenza
do_overlap_LCany(cdm, c(1:4), c(5:29), c(105:108))

# LC code + Infection
do_overlap(cdm, 1, 101, 109, washout = FALSE)

# LC code + Reinfection
do_overlap(cdm, 1, 101, 110, washout = FALSE)

# LC code + Test negative
do_overlap(cdm, 1, 101, 111, washout = FALSE)

# LC code + Influenza
do_overlap(cdm, 1, 101, 112, washout = FALSE)

# PASC any symptom + Infection
do_overlap(cdm, 1, 102, 113, washout = FALSE)

# PASC any symptom + Reinfection
do_overlap(cdm, 2, 102, 114, washout = FALSE)

# PASC any symptom + Test negative
do_overlap(cdm, 3, 102, 115, washout = FALSE)

# PASC any symptom + Influenza
do_overlap(cdm, 4, 102, 116, washout = FALSE)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = c(105:116), 
                                           cohort_name = c("LC any + inf","LC any + reinf","LC any + neg", "LC any + flu",
                                                          "LC code + inf","LC code + reinf","LC code + neg","LC code + flu",
                                                          "PASC any + inf","PASC any + reinf","PASC any + neg","PASC any + flu")))

# Base cohorts plus vaccination or non vaccination
do_overlap_vacc(cdm, 1,117)
do_overlap_vacc(cdm, 2,119)
do_overlap_vacc(cdm, 3,121)
do_overlap_vacc(cdm, 4,123)

names_final_cohorts <- rbind(names_final_cohorts,
                             dplyr::tibble(cohort_definition_id = c(117:124), 
                                           cohort_name = c("Inf + vacc","Inf + not vacc",
                                                          "Reinf + vacc","Reinf + not vacc",
                                                          "Neg + vacc", "Neg + not vacc",
                                                          "Flu + vacc", "Flu + not vacc")))


# Overlapping cohorts of single symptoms / events / medical conditions with base cohorts
# Cohort_definition if is id_base*200 + id_medical/symptom -> e.g. cough and influenza is 812
base_ids <- c(1:4)
outcome_ids <- c(5:59)
for(i in base_ids) {
  for(j in outcome_ids){
    if(cdm$studyathon_final_cohorts %>% filter(cohort_definition_id == j) %>% tally() %>% pull() > 5) {
      do_overlap(cdm, i, j, i*200+j)
      names_final_cohorts <- rbind(names_final_cohorts,
                                   dplyr::tibble(cohort_definition_id = i*200+j, 
                                                 cohort_name =paste0("Base ",i," outcome ",j) ))
    }
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
                             dplyr::tibble(cohort_definition_id = c(900:923), 
                                           cohort_name = c("Inf + LC any + vacc",
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
# TREATMENT PATTERNS and HEALTHCARE UTILISATION COHORTS (FOR LATER)
if(doTreatmentPatterns) {
#K
# Whichever they are in the end  
#  create_outcome(cdm, window = c(65:99), filter_start = FALSE, first_event = FALSE)
}

if(doCharacterisation || doClusteringLCA) {
  # Get Healthcare Utilisation outcomes for characterisation and clustering
  HU_cohorts <- CDMConnector::readCohortSet(here::here("4_Characterisation","HU_cohorts"))
  cdm <- CDMConnector::generateCohortSet(cdm, HU_cohorts,
                                         cohortTableName = "lc_pasc_hucohorts",
                                         overwrite = TRUE)
  cdm$lc_pasc_hucohorts <- cdm$lc_pasc_hucohorts %>% left_join(observation_death, 
                                                               by = c("subject_id")) %>%
    mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>% 
    compute()
}

# --------------------------------------------------------------------
# Print counts of all cohorts (if >5) 

finalCounts <- cdm[["studyathon_final_cohorts"]] %>% 
  group_by(cohort_definition_id) %>% 
  tally() %>% 
  collect() %>% 
  right_join(names_final_cohorts, by = c("cohort_definition_id"="cohort_definition_id")) %>% 
  mutate(n = as.numeric(n)) %>% mutate(n = if_else(is.na(n), 0, n)) %>%
  mutate(n = ifelse(n <= 5, NA, n)) %>% dplyr::select(cohort_name,n)

# Export csv
write.csv(finalCounts,
          file = file.path(tempDir,
            paste0(db.name,"_finalcounts.csv")
          ),
          row.names = FALSE
)

write.csv(names_final_cohorts,
          file = file.path(tempDir,
                           paste0(db.name,"_cohorts.csv")
          ),
          row.names = FALSE
)