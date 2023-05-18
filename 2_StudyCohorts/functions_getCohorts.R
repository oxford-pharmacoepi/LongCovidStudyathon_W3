# Functions used throughout the study

do_exclusion <- function(cdm, cohort, id, S_start_date) {
  
  # function that applies exclusion criteria and gets attrition of specified base cohort
  attrition <- dplyr::tibble(
    number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  # Apply washout 42 days
  cohort <- cohort %>% 
    addCohortIntersectDays(cdm, targetCohortTable = InitialCohortsName, targetCohortId = id, window = list(c(-Inf,-1)), order = "last", nameStyle = "date_previous") %>%
    computeQuery()
  
  cohort <- cohort %>%
    filter(is.na(.data$date_previous) | .data$date_previous < -42) %>% computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(
                       number_observations = cohort %>% dplyr::tally() %>%
                         dplyr::pull(), reason = "Event washout"))
  cohort <- cohort %>% dplyr::select(-date_previous) %>% computeQuery()
  
  # Check the individuals are in observation at cohort entry
  cohort <- cohort %>% addInObservation(cdm) %>%
    filter(.data$in_observation == 1) %>% computeQuery()
  attrition <- rbind(attrition,
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally() %>% dplyr::pull(),
                                   reason = "In observation at cohort entry"))
  
    # Prior history 365 days
    cohort <- cohort %>% addPriorHistory(cdm) %>% computeQuery() 
    cohort <- cohort %>% filter(.data$prior_history >= 365) %>% computeQuery()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = paste0("365 days of prior history")))
    
    cohort <- cohort %>% dplyr::select(-c(prior_history)) %>% computeQuery()
  
  
    # Historical influenza 90 days
    cohort <- cohort %>%
      addCohortIntersectDays(cdm, targetCohortTable = InitialCohortsName, targetCohortId = 37, window = list(c(-90,-1)), order = "last", nameStyle = "last_flu") %>%
      computeQuery()
    
    cohort <- cohort %>% dplyr::filter(is.na(.data$last_flu)) %>% computeQuery()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = "Historical influenza"))
    cohort <- cohort %>% dplyr::select(-last_flu) %>% computeQuery()
  
  
  # keep only people starting after S_start_date
  cohort <- cohort %>% dplyr::filter(.data$cohort_start_date > S_start_date) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = paste0("Entry after ",
                                                                      S_start_date)))
  
  # censor on observation_end, death, end of covid testing or study end date
  cohort <- cohort %>% dplyr::mutate(one_year_date = 
                                       !!CDMConnector::dateadd("cohort_start_date", 365)) %>%
    dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
    left_join(observation_death, by = c("subject_id")) %>%
    computeQuery()
  cohort <- cohort %>% 
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$one_year_date)) & .data$cohort_end_date > .data$one_year_date, .data$one_year_date, .data$cohort_end_date))) %>%
    dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
      !(is.na(.data$end_covid_testing_date)) & .data$cohort_end_date > .data$end_covid_testing_date, .data$end_covid_testing_date, .data$cohort_end_date))) %>%
    dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>% 
    dplyr::mutate(reason_censoring = ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
                                            ifelse(cohort_end_date == .data$one_year_date, "one year of follow_up",
                                                   ifelse(cohort_end_date == .data$end_covid_testing_date, "End of COVID-19 testing",
                                                          ifelse(cohort_end_date == .data$observation_period_end_date,
                                                                 "end of data collection or exit from database",NA ))))) %>% computeQuery()
  
    # exclude if follow-up < 120 days
    excluded_followup <- cohort %>% dplyr::filter(.data$follow_up_days < 120) %>%
      computeQuery()
    reason_exclusion <- excluded_followup %>% dplyr::group_by(.data$reason_censoring) %>%
      tally() %>% collect()
    cohort <- cohort %>% dplyr::filter(.data$follow_up_days >= 120) %>% 
      dplyr::select(-follow_up_days, -reason_censoring) %>% computeQuery()
    
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = paste0(
                                       "> 120 days follow-up")))
  
  # get first or subsequent events
  cohort <- cohort %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% computeQuery()
  
  cohort <- cohort %>% dplyr::select(subject_id, cohort_definition_id, 
                                     cohort_start_date, cohort_end_date, seq) %>% 
    computeQuery()
  
  first_event <- cohort %>% dplyr::filter(seq == 1) %>% dplyr::select(-seq) %>% computeQuery()
  subs_events <- cohort %>% dplyr::filter(seq != 1) %>% dplyr::select(-seq) %>% computeQuery()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "First event only"))
  
  # Not for any "re-event" cohort
  # No historical covid-19 infection
  first_event <- first_event %>% 
    addCohortIntersectDate(cdm, targetCohortTable = InitialCohortsName, targetCohortId = 1, window = list(c(-Inf, -1)), order = "last", nameStyle = "event") %>%
    computeQuery()
  
  first_event <- first_event %>% dplyr::filter(is.na(.data$event)) %>% computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Historical COVID-19"))
  first_event <- first_event %>% dplyr::select(-c(event)) %>% computeQuery()
  
  
  return(list(first_event,subs_events,attrition,reason_exclusion))
}

do_overlap <- function(cdm, base_cohort_id, outcome_cohort_id, overlap_cohort_id, tableName) {
  base <- cdm[[BaseCohortsName]] %>% 
    dplyr::filter(cohort_definition_id == base_cohort_id) %>%
    computeQuery()
  outcome <- cdm[[tableName]] %>% 
    dplyr::filter(cohort_definition_id == outcome_cohort_id) %>%
    computeQuery()
  
  overlap <- base %>% 
    dplyr::inner_join(
      outcome %>% 
        dplyr::select(subject_id, outcome_date = cohort_start_date, 
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>% distinct() %>% computeQuery()
  attrition <- dplyr::tibble(
    number_observations = overlap %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  overlap <- overlap %>%
    dplyr::mutate(cohort_definition_id = overlap_cohort_id) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(time_diff < -90 & time_diff > -366) %>%
    dplyr::select(-time_diff) %>% 
    computeQuery()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Window (90,365)"))
  overlap <- overlap %>%
    dplyr::group_by(subject_id,cohort_start_date) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(seq = row_number()) %>%
    dbplyr::window_order() %>% 
    distinct() %>% 
    ungroup() %>%
    dplyr::filter(seq == 1) %>% 
    dplyr::mutate(cohort_end_date = min(.data$cohort_end_date, .data$outcome_date)) %>%
    computeQuery()
  # We are only asking for the first outcome event in the window of interest, per each index base event
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Only first event"))
  overlap <- overlap %>% 
    addCohortIntersectDate(cdm, targetCohortTable = tableName, targetCohortId = outcome_cohort_id, window = list(c(-180, -1)), order = "last", nameStyle = "date_previous") %>%
    dplyr::filter(is.na(.data$date_previous)) %>% dplyr::select(-c(date_previous)) %>% 
    computeQuery()
     attrition <- rbind(attrition, 
                        dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                      %>% dplyr::pull(), reason = "180 days of washout"))
  
  write_csv(
    attrition,
    file = here::here(output_at, paste0("attrition_overlap_",overlap_cohort_id,".csv"))
  )
    overlap <- overlap %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      distinct() %>%
      computeQuery()
  
  return(overlap)
}

create_outcome <- function(cdm, window, new_ids, tableName) {
  counter <- 1
  for(i in window){
    new_id <- new_ids[counter]
    name_cohort <- Initial_cohorts$cohort_name[i]
    current <- cdm[[InitialCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == i) %>% dplyr::select(
        "subject_id",
        "cohort_start_date"
      ) %>% computeQuery() 
    attrition <- dplyr::tibble(
      number_observations = current %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Starting events"
    )
  
    current <- current %>% dplyr::mutate(cohort_end_date = .data$cohort_start_date) %>%
        computeQuery()
    
    current <- current %>% dplyr::mutate(cohort_definition_id = new_id) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      computeQuery()
    
    if(isTRUE(counter == 1)) {
      currenttable <- current %>% computeQuery()
      
    } else {
      currenttable <- dplyr::union_all(currenttable, current) %>% computeQuery()
    }
    write_csv(
      attrition,
      file = here::here(output_at, paste0("attrition_outcome_",tableName,"_",new_id,".csv"))
    )
    counter <- counter + 1
  }
  return(currenttable)
}
