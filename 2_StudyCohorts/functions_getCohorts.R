# Functions used throughout the study

do_exclusion <- function(cdm, cohort, id, databefore = TRUE, 
                         before_look = 365, dataafter = TRUE, after_look = 120,
                         influenza_preid = TRUE, influenza_look = 90,
                         covid_preid = TRUE, covid_look = NA,
                         washout = 42, S_start_date, covidcensor = TRUE,
                         influenza = FALSE
) {
  
  # function that applies exclusion criteria and gets attrition of specified base cohort
  
  attrition <- dplyr::tibble(
    number_observations = cohort %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  
  # Apply washout 
  cohort <- cohort %>% 
    addEvent(cdm, InitialCohortsName, id, c(NA,-1), order = "last") %>%
    mutate(date_previous = !!CDMConnector::datediff("cohort_start_date", "event")) %>%
    compute()
  
  cohort <- cohort %>%
    filter(is.na(.data$date_previous) | .data$date_previous < -washout) %>% compute()
  attrition <- rbind(attrition, 
                     dplyr::tibble(
                       number_observations = cohort %>% dplyr::tally() %>%
                         dplyr::pull(), reason = "Event washout"))
  cohort <- cohort %>% dplyr::select(-c(event,date_previous)) %>% compute()
  
  # Check the individuals are in observation at cohort entry
  cohort <- cohort %>% addInObservation(cdm) %>%
    filter(.data$in_observation == 1) %>% compute()
  attrition <- rbind(attrition,
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally() %>% dplyr::pull(),
                                   reason = "In observation at cohort entry"))
  
  if(databefore) {
    # Prior history
    cohort <- cohort %>% addPriorHistory(cdm) %>% compute() 
    cohort <- cohort %>% filter(.data$prior_history >= before_look) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = paste0(before_look, 
                                                                        " days of prior history")))
    
    cohort <- cohort %>% dplyr::select(-c(prior_history)) %>% compute()
  }
  
  if(influenza_preid) {
    # Historical influenza
    cohort <- cohort %>%
      addEvent(cdm, InitialCohortsName, 4, c(-influenza_look,-1), order = "last") %>%
      dplyr::mutate(last_flu = !!CDMConnector::datediff("cohort_start_date", "event")) %>% 
      compute()
    
    cohort <- cohort %>% dplyr::filter(is.na(.data$last_flu)) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = "Historical influenza"))
    cohort <- cohort %>% dplyr::select(-c(event,last_flu)) %>% compute()
  }
  
  # keep only people starting after S_start_date
  cohort <- cohort %>% dplyr::filter(.data$cohort_start_date > S_start_date) %>% compute()
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = cohort %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = paste0("Entry after ",
                                                                      S_start_date)))
  
  # censor on covid infection too if due
  if(covidcensor == TRUE && influenza == FALSE) {
    cohort <- cohort %>% 
      addEvent(cdm, InitialCohortsName, 1, c(1, 365)) %>%
      compute()
    # censor on observation_end, death, study end date, or covid (re)infection
    cohort <- cohort %>% dplyr::mutate(one_year_date = 
                                         !!CDMConnector::dateadd("cohort_start_date",
                                                                 365)) %>%
      dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      compute()
    
    cohort <- cohort %>% 
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$event)) & .data$cohort_end_date > .data$event, .data$event, .data$cohort_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$one_year_date)) & .data$cohort_end_date > .data$one_year_date, .data$one_year_date, .data$cohort_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$end_covid_testing_date)) & .data$cohort_end_date > .data$end_covid_testing_date, .data$end_covid_testing_date, .data$cohort_end_date))) %>%
      dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>% 
      dplyr::mutate(reason_censoring = ifelse(
        !(is.na(event)) &
          cohort_end_date == event, "COVID-19",
        ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
               ifelse(cohort_end_date == .data$one_year_date, "one year of follow_up",
                      ifelse(cohort_end_date == .data$end_covid_testing_date, "End of COVID-19 testing",
                             ifelse(cohort_end_date == .data$observation_period_end_date,
                                    "end of data collection or exit from database",NA )))))) %>%
      compute()
  } else if (covidcensor == TRUE && influenza == TRUE) {
    cohort <- cohort %>% 
      addEvent(cdm, InitialCohortsName, 1, c(1, 365)) %>%
      compute()
    # censor on observation_end, death, study end date, or covid (re)infection
    cohort <- cohort %>% dplyr::mutate(one_year_date = 
                                         !!CDMConnector::dateadd("cohort_start_date", 365)) %>%
      dplyr::mutate(end_influenza_date =  as.Date("2019-12-31")) %>% # before Covid-19 pandemic
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      compute()
    cohort <- cohort %>% 
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$death_date)) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$event)) & .data$cohort_end_date > .data$event, .data$event, .data$cohort_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$one_year_date)) & .data$cohort_end_date > .data$one_year_date, .data$one_year_date, .data$cohort_end_date))) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(
        !(is.na(.data$end_influenza_date)) & .data$cohort_end_date > .data$end_influenza_date, .data$end_influenza_date, .data$cohort_end_date))) %>%
      dplyr::mutate(follow_up_days = !!CDMConnector::datediff("cohort_start_date", "cohort_end_date")) %>% 
      dplyr::mutate(reason_censoring = ifelse(
        !(is.na(.data$event)) &
          cohort_end_date == .data$event, "COVID-19",
        ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
               ifelse(cohort_end_date == .data$one_year_date, "one year of follow_up",
                      ifelse(cohort_end_date == .data$end_influenza_date, "end of influenza cohort consideration (31-12-2019)",
                             ifelse(cohort_end_date == .data$observation_period_end_date,
                                    "end of data collection or exit from database",NA )))))) %>%
      compute()
  } else {
    # censor on observation_end, death, study end date, or covid (re)infection
    cohort <- cohort %>% dplyr::mutate(one_year_date = 
                                         !!CDMConnector::dateadd("cohort_start_date", 365)) %>%
      dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
      left_join(observation_death, by = c("subject_id")) %>%
      compute()
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
                                                                   "end of data collection or exit from database",NA ))))) %>% compute()
  }
  
  
  
  if(dataafter) {
    # exclude if follow-up < specified days
    excluded_followup <- cohort %>% dplyr::filter(.data$follow_up_days < after_look) %>%
      compute()
    reason_exclusion <- excluded_followup %>% dplyr::group_by(.data$reason_censoring) %>%
      tally() %>% collect()
    cohort <- cohort %>% dplyr::filter(.data$follow_up_days >= after_look) %>% 
      dplyr::select(-follow_up_days, -reason_censoring) %>% compute()
    
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = cohort %>%
                                       dplyr::tally()
                                     %>% dplyr::pull(), reason = paste0(
                                       "> ",after_look, " days follow-up")))
  }
  
  # get first or subsequent events
  cohort <- cohort %>% dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
    dplyr::mutate(seq = row_number()) %>% distinct() %>% ungroup() %>% compute()
  
  cohort <- cohort %>% dplyr::select(subject_id, cohort_definition_id, 
                                     cohort_start_date, cohort_end_date, seq) %>% 
    compute()
  
  first_event <- cohort %>% dplyr::filter(seq == 1) %>% dplyr::select(-seq) %>% compute()
  subs_events <- cohort %>% dplyr::filter(seq != 1) %>% dplyr::select(-seq) %>% compute()
  
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "First event only"))
  
  # Not for any "re-event" cohort
  if(covid_preid) {
    # No historical covid-19 infection
    first_event <- first_event %>% 
      addEvent(cdm, InitialCohortsName, 1, c(-covid_look,-1), order = "last") %>%
      compute()
    
    first_event <- first_event %>% dplyr::filter(is.na(.data$event)) %>% compute()
    attrition <- rbind(attrition, 
                       dplyr::tibble(number_observations = first_event %>% dplyr::tally()
                                     %>% dplyr::pull(), reason = "Historical COVID-19"))
    first_event <- first_event %>% dplyr::select(-c(event)) %>% compute()
  }
  
  return(list(first_event,subs_events,attrition,reason_exclusion))
}

do_overlap <- function(cdm, base_cohort_id, outcome_cohort_id, overlap_cohort_id, washout = TRUE, tableName, overlapTableName, first = FALSE, indexsymptom = FALSE, tableold) {
  base <- cdm[[BaseCohortsName]] %>% 
    dplyr::filter(cohort_definition_id == base_cohort_id) %>%
    compute()
  if(!tableget){
    outcome <- tableold %>% 
      dplyr::filter(cohort_definition_id == outcome_cohort_id) %>%
      compute()
  } else {
    outcome <- cdm[[tableName]] %>% 
      dplyr::filter(cohort_definition_id == outcome_cohort_id) %>%
      compute()
  }
  
  overlap <- base %>% 
    dplyr::inner_join(
      outcome %>% 
        dplyr::select(subject_id, outcome_date = cohort_start_date, 
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>% distinct() %>% compute()
  attrition <- dplyr::tibble(
    number_observations = overlap %>% dplyr::tally() %>% dplyr::pull(),
    reason = "Starting events"
  )
  overlap <- overlap %>%
    dplyr::mutate(cohort_definition_id = overlap_cohort_id) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(time_diff < -90 & time_diff > -366) %>%
    dplyr::select(-time_diff) %>% 
    compute()
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
    compute()
  # We are only asking for the first outcome event in the window of interest, per each index base event
  attrition <- rbind(attrition, 
                     dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                   %>% dplyr::pull(), reason = "Only first event"))
  if(washout) {
    overlap <- overlap %>% 
      addEvent( cdm, tableName, outcome_cohort_id, c(-180,-1), order = "last") %>% 
      compute()
    if("event" %in% colnames(overlap)) {
      overlap <- overlap %>% 
        dplyr::mutate(date_previous = !!CDMConnector::datediff("cohort_start_date", "event")) %>%
        dplyr::filter(is.na(.data$date_previous)) %>% dplyr::select(-c(event,date_previous)) %>% 
        compute()
      attrition <- rbind(attrition, 
                         dplyr::tibble(number_observations = overlap %>% dplyr::tally()
                                       %>% dplyr::pull(), reason = "180 days of washout"))
    }
  }
  write_csv(
    attrition,
    file = here::here(output_at, paste0("attrition_overlap_",overlapTableName,"_",overlap_cohort_id,".csv"))
  )
  if(indexsymptom) {
    overlap <- overlap %>% dplyr::select(subject_id,cohort_definition_id,outcome_date,outcome_end) %>%
      dplyr::rename("cohort_start_date" = "outcome_date") %>% dplyr::rename("cohort_end_date" = "outcome_end") %>%
      distinct() %>%
      compute()
  } else {
    overlap <- overlap %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      distinct() %>%
      compute()
  }
  
  if(first == TRUE) {
  } else {
    overlap <- dplyr::union_all(tableold, overlap) %>% compute()
  } 
  
  return(overlap)
}

create_outcome <- function(cdm, window, filter_start = TRUE, first_event = TRUE, end_outcome = TRUE, new_ids, tableName) {
  counter <- 1
  for(i in window){
    new_id <- new_ids[counter]
    name_cohort <- Initial_cohorts$cohort_name[i]
    current <- cdm[[InitialCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == i) %>% dplyr::select(
        "subject_id",
        "cohort_start_date"
      ) %>% compute() 
    attrition <- dplyr::tibble(
      number_observations = current %>% dplyr::tally() %>% dplyr::pull(),
      reason = "Starting events"
    )
    
    if(filter_start) {
      current <- current %>% dplyr::filter(cohort_start_date > study_start_date) %>%
        compute()
      attrition <- rbind(attrition, 
                         dplyr::tibble(number_observations = current %>% dplyr::tally()
                                       %>% dplyr::pull(), reason = paste0("Entry after ",
                                                                          study_start_date)))
    }
    
    if(first_event) {
      # Only get the first event
      current <- current %>% dplyr::group_by(subject_id) %>% 
        dbplyr::window_order(.data$cohort_start_date) %>%
        dplyr::mutate(seq = row_number()) %>% distinct() %>% 
        ungroup() %>% compute()
      current <- current %>% dplyr::filter(.data$seq == 1) %>% 
        dplyr::select(-seq) %>% compute()
      
      attrition <- rbind(attrition, 
                         dplyr::tibble(number_observations = current %>% dplyr::tally()
                                       %>% dplyr::pull(), reason = "Only first event"))
      
    }
    
    current <- current %>% dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = !!CDMConnector::asDate(ifelse(!is.na(.data$death_date) & .data$observation_period_end_date > .data$death_date, .data$death_date, .data$observation_period_end_date))) %>%
      compute()
    
    if(end_outcome) {
      current <- current %>% dplyr::mutate(cohort_end_date = .data$cohort_start_date) %>%
        compute()
    }
    
    current <- current %>% dplyr::mutate(cohort_definition_id = new_id) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    
    if(isTRUE(counter == 1)) {
      currenttable <- current %>% compute()
      
    } else {
      currenttable <- dplyr::union_all(currenttable, current) %>% compute()
    }
    write_csv(
      attrition,
      file = here::here(output_at, paste0("attrition_outcome_",tableName,"_",new_id,".csv"))
    )
    counter <- counter + 1
  }
  return(currenttable)
}
