# Functions used throughout the study

do_exclusion <- function(cohort, id, name_date_col, databefore = TRUE, 
                         before_look = 365, dataafter = TRUE, after_look = 120,
                         influenza_preid = TRUE, influenza_look = 90,
                         covid_preid = TRUE, covid_look = NA,
                         washout = 42, S_start_date
) {
  
  # function that applies exclusion criteria and gets attrition of specified cohort
  
  attrition <- dplyr::tibble(
    number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  # Apply washout 
  cohort <- cohort %>% CohortProfiles::addEvent(cdm, cohort_table_name, 
              eventAt = name_date_col, filter = list(cohort_definition_id = id), 
              window = c(NA,-1), order = "last") %>% 
              mutate(date_previous = !!datediff(name_date_col, "event")) %>% compute()
  
  cohort <- cohort %>% filter(is.na(date_previous) | date_previous < -washout) %>% compute()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(), reason = "Event washout"))
  cohort <- cohort %>% dplyr::select(-c(event,date_previous)) %>% compute()
  
  # Check the individuals are in observation at cohort entry: addInObservation not exported in CohortProfiles, so waiting for a function
#  cohort <- cohort %>% CohortProfiles::addInObservation(cdm) %>%
#    filter(inObservation == 1) %>% compute()
#  attrition <- rbind(attrition,
#                     dplyr::tibble(number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(), reason = "In observation at cohort entry"))
  
  if(databefore) {
    # Prior history
    cohort <- cohort %>% CohortProfiles::addPriorHistory(cdm, priorHistoryAt = 
               name_date_col) %>% compute() # it does not work with S_start_date (and ER does it with cohort_start_date too, so...??)
    # try again priorHistory with date etc
    cohort <- cohort %>% filter(prior_history >= before_look) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                       %>% dplyr::pull(), reason = paste0(before_look, 
                        " days of prior history")))
    
    cohort <- cohort %>% dplyr::select(-c(prior_history)) %>% compute()
  }
  
  if(influenza_preid) {
    # Historical influenza
    cohort <- cohort %>% CohortProfiles::addEvent(cdm, cohort_table_name, 
            eventAt = name_date_col, filter = list(cohort_definition_id = 4), 
            window = c(-influenza_look,-1), order = "last") %>% 
            mutate(last_flu = !!datediff(name_date_col, "event")) %>% compute()
    
    cohort <- cohort %>% filter(is.na(last_flu)) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                       %>% dplyr::pull(), reason = "Historical influenza"))
    cohort <- cohort %>% dplyr::select(-c(event,last_flu)) %>% compute()
  }
  
  # keep only people starting after S_start_date
  cohort <- cohort %>% filter(.data[[name_date_col]] > S_start_date) %>% compute()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                     %>% dplyr::pull(), reason = paste0("Entry after ",
                                                        S_start_date)))
  
  # censor on observation_end, death, study end date, or covid (re)infection
  cohort <- cohort %>% mutate(one_year_date = 
    lubridate::as_date(.data[[name_date_col]] + lubridate::days(365))) %>%
    mutate(end_covid_testing_date =  as.Date(covid_end_date)) %>% # asked in the CodeToRun
    left_join(observation_death, by = c("subject_id")) %>%
    compute()
  
  # Right now, everyone gets censored on covid. Susceptible to change
  cohort <- cohort %>% CohortProfiles::addEvent(cdm, cohort_table_name, 
            eventAt = name_date_col, filter = list(cohort_definition_id = 2), 
            window = c(1,365), order = "first", name = "covid_censoring_date") %>% compute()

  cohort <- cohort %>% mutate(cohort_end_date = lubridate::as_date(pmin(
    observation_period_end_date, 
    death_date, 
    covid_censoring_date - lubridate::days(1), 
    one_year_date,
    end_covid_testing_date - lubridate::days(1),
    na.rm = F))) %>%
    mutate(follow_up_days = cohort_end_date - .data[[name_date_col]]) %>% 
    mutate(reason_censoring = ifelse(!(is.na(covid_censoring_date)) & cohort_end_date == covid_censoring_date-lubridate::days(1), "COVID-19",
                                     ifelse(!(is.na(death_date)) & cohort_end_date == death_date, "death",
                                            ifelse(cohort_end_date == one_year_date, "one year of follow_up",
                                                   ifelse(cohort_end_date == end_covid_testing_date, "End of COVID-19 testing",
                                                          ifelse(cohort_end_date == observation_period_end_date,
                                                                 "end of data collection or exit from database",NA )))))) %>% compute()
  
  if(dataafter) {
    # exclude if follow-up < specified days
    excluded_followup <- cohort %>% filter(follow_up_days < after_look) %>% compute()
    reason_exclusion <- excluded_followup %>% group_by(reason_censoring) %>% tally() %>% collect()
    cohort <- cohort %>% filter(follow_up_days >= after_look) %>% 
      dplyr::select(-follow_up_days, -reason_censoring) %>% compute()
    
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                       %>% dplyr::pull(), reason = paste0("> ",after_look, " days follow-up")))
    # try add exclusion for each 120d!
  }
  
  # think of which are distinct(), etc.
  # compute too!

  # get first or subsequent events
  cohort <- cohort %>% group_by(subject_id) %>% arrange(.data[[name_date_col]]) %>%
    mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  
  cohort <- cohort %>% dplyr::select(subject_id, all_of(name_date_col), cohort_end_date, 
            seq) %>% compute()
  
  first_event <- cohort %>% filter(seq == 1) %>% dplyr::select(-seq) %>% compute()
  subs_events <- cohort %>% filter(seq != 1) %>% dplyr::select(-seq) %>% compute()
    # more than one cohort for re-event (infection)??
    # using cohort_id 2 from now on, no problem with previous usage for censoring? DO WE HAVE TO? Erased it now.
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                     %>% dplyr::pull(), reason = "First event only"))
  
  # Not for any "re-event" cohort
  if(covid_preid) {
    # No historical covid-19 infection
    first_event <- first_event %>% CohortProfiles::addEvent(cdm, cohort_table_name, 
            eventAt = name_date_col, filter = list(cohort_definition_id = 2),
            window = c(-covid_look,-1), order = "last") %>% 
      mutate(last_covid = !!datediff(name_date_col, "event")) %>% compute()
    
    first_event <- first_event %>% filter(is.na(last_covid)) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                       %>% dplyr::pull(), reason = "Historical COVID-19"))
    first_event <- first_event %>% dplyr::select(-c(last_covid,event)) %>% compute()
  }
  
  #first_event <- first_event %>% collect()
  #subs_events <- subs_events %>% collect()
  #attrition <- attrition %>% collect()
  
  return(list(first_event,subs_events,attrition,reason_exclusion))
}


do_overlap <- function(base_cohort_id, outcome_cohort_id, overlap_cohort_id, washout = TRUE) {
  base <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_cohort_id)
  outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_cohort_id)
  overlap <- base %>% inner_join(outcome %>% 
    dplyr::select(subject_id, outcome_date = cohort_start_date, outcome_end = cohort_end_date),
    by = "subject_id") %>% mutate(cohort_definition_id = overlap_cohort_id) %>%
    mutate(time_diff = CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    filter(time_diff < -90 & time_diff > -366) %>% dplyr::select(-time_diff) %>% compute()
  
  if(washout) {
    overlap <- overlap %>% CohortProfiles::addEvent(cdm,"studyathon_final_cohorts", filter = list(cohort_definition_id = outcome_cohort_id), window = c(-180,-1), order = "last", eventAt = "cohort_start_date", eventDate = "cohort_start_date") %>%
      filter(is.na(event)) %>% dplyr::select(-event) %>% compute()
  }

  overlap <- overlap %>% mutate(cohort_end_date = pmin(cohort_end_date, outcome_date)) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>% compute()
  appendPermanent(overlap, name = "studyathon_final_cohorts",  schema = results_database_schema)
}


create_outcome <- function(window, filter_start = TRUE) {
  for(i in window){
    name_cohort <- initialCohortSet$cohortName[i]
    current <- cdm[[cohort_table_name]] %>% 
      dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
        "subject_id",
        "cohort_start_date"
      ) %>% compute() 
    
    if(filter_start) {
      current <- current %>% dplyr::filter(cohort_start_date > study_start_date) %>% compute()
    }
    
    # How to deal with repeated events??
    # current_symptom <- current_symptom %>% group_by(subject_id) %>% arrange(cohort_start_date) %>%
    #  mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
    
    current <- current %>% left_join(observation_death, by = c("subject_id")) %>%
      mutate(cohort_end_date = lubridate::as_date(pmin(observation_period_end_date,death_date))) %>% compute()
    
    current <- current %>% mutate(cohort_definition_id = i) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
    appendPermanent(current, name = "studyathon_final_cohorts",  schema = results_database_schema)
    
    #first_event <- cough %>% filter(seq == 1) %>% dplyr::select(-seq) %>% compute()
    #subs_events <- cough %>% filter(seq != 1) %>% dplyr::select(-seq) %>% compute()
  }
}


crate_any_cohort <- function(window, cohort_id) {
  cohorts <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% window)
  # people (381170) or events (441641)?
  any_cohort <- cohorts %>% dplyr::select(-cohort_definition_id) %>% mutate(cohort_definition_id = cohort_id) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  
  # group_by(subject_id, symptom_date) %>%
  # keep date of first symptom recorded
  # filter(cohort_end_date == min(cohort_end_date)) %>%
  # ungroup()
  
  appendPermanent(any_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)
}


crate_any_vacc_cohort <- function(window, cohort_id) {
  cohorts <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% window)
  # people (381170) or events (441641)?
  any_cohort <- cohorts %>% dplyr::select(-cohort_definition_id) %>% mutate(cohort_definition_id = cohort_id) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  any_no_cohort <- cdm$person %>% mutate(subject_id = person_id) %>% anti_join(any_cohort, by = subject_id) %>% mutate(cohort_definition_id = cohort_id+1) %>%dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  # group_by(subject_id, symptom_date) %>%
  # keep date of first symptom recorded
  # filter(cohort_end_date == min(cohort_end_date)) %>%
  # ungroup()
  
  appendPermanent(any_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(any_no_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
}


do_overlap_vacc <- function(base_id, new_id) {
  vaccinated_cohort <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id) %>% 
    left_join(cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == 103) %>% dplyr::select(subject_id, "vacc_date" = "cohort_start_date"), by = "subject_id") %>%
    filter(cohort_start_date > vacc_date) %>% filter(!(is.na(vacc_date))) %>% dplyr::select(subject_id,cohort_start_date,cohort_end_date) %>%
    mutate(cohort_definition_id = new_id) %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>% compute()
  nonvaccinated_cohort <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id) %>% 
    anti_join(vaccinated_cohort, by="subject_id") %>% mutate(cohort_definition_id = new_id+1)  %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)%>% 
    compute()
  appendPermanent(vaccinated_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(nonvaccinated_cohort, name = "studyathon_final_cohorts",  schema = results_database_schema)
  
}
