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
    mutate(date_previous = !!CDMConnector::datediff(.data$cohort_start_date, .data$event)) %>%
    compute()
  
  cohort <- cohort %>%
    filter(is.na(.data$date_previous) | .data$date_previous < -washout) %>% compute()
  attrition <- rbind(attrition, 
                     dplyr::tibble(
                       number_observations = cohort %>% dplyr::tally() %>%
                         dplyr::pull(), reason = "Event washout"))
  cohort <- cohort %>% dplyr::select(-c(event,date_previous)) %>% compute()
  
  # Check the individuals are in observation at cohort entry
  cohort <- cohort %>% PatientProfiles::addInObservation(cdm) %>%
    filter(.data$in_observation == 1) %>% compute()
  attrition <- rbind(attrition,
                     dplyr::tibble(number_observations = cohort %>%
                                     dplyr::tally() %>% dplyr::pull(),
                                   reason = "In observation at cohort entry"))
  
  if(databefore) {
    # Prior history
    cohort <- cohort %>% PatientProfiles::addPriorHistory(cdm) %>% compute() 
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
      dplyr::mutate(last_flu = !!CDMConnector::datediff(.data$cohort_start_date, .data$event)) %>% 
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
                                  lubridate::as_date(.data$cohort_start_date + lubridate::days(365))) %>%
      dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      compute()
    
    cohort <- cohort %>% dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(
      .data$observation_period_end_date, 
      .data$death_date, 
      .data$event - lubridate::days(1), 
      .data$one_year_date,
      .data$end_covid_testing_date - lubridate::days(1),
      na.rm = F))) %>%
      dplyr::mutate(follow_up_days = .data$cohort_end_date - .data$cohort_start_date) %>% 
      dplyr::mutate(reason_censoring = ifelse(
        !(is.na(event)) &
          cohort_end_date == event - lubridate::days(1), "COVID-19",
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
                                  lubridate::as_date(.data$cohort_start_date + lubridate::days(365))) %>%
      dplyr::mutate(end_influenza_date =  as.Date("2019-12-31")) %>% # before Covid-19 pandemic
      dplyr::left_join(observation_death, by = c("subject_id")) %>%
      compute()
    cohort <- cohort %>% dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(
      .data$observation_period_end_date, 
      .data$death_date, 
      .data$event - lubridate::days(1), 
      .data$one_year_date,
      .data$end_influenza_date,
      na.rm = F))) %>%
      dplyr::mutate(follow_up_days = cohort_end_date - .data$cohort_start_date) %>% 
      dplyr::mutate(reason_censoring = ifelse(
        !(is.na(.data$event)) &
          cohort_end_date == .data$event - lubridate::days(1), "COVID-19",
        ifelse(!(is.na(.data$death_date)) & cohort_end_date == .data$death_date, "death",
               ifelse(cohort_end_date == .data$one_year_date, "one year of follow_up",
                      ifelse(cohort_end_date == .data$end_influenza_date, "end of influenza cohort consideration (31-12-2019)",
                             ifelse(cohort_end_date == .data$observation_period_end_date,
                                    "end of data collection or exit from database",NA )))))) %>%
      compute()
  } else {
    # censor on observation_end, death, study end date, or covid (re)infection
    cohort <- cohort %>%dplyr::mutate(one_year_date = 
                                  lubridate::as_date(.data$cohort_start_date + lubridate::days(365))) %>%
      dplyr::mutate(end_covid_testing_date =  as.Date(.env$covid_end_date)) %>% # asked in the CodeToRun file
      left_join(observation_death, by = c("subject_id")) %>%
      compute()
    cohort <- cohort %>% dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(
      .data$observation_period_end_date, 
      .data$death_date, 
      .data$one_year_date,
      .data$end_covid_testing_date - lubridate::days(1),
      na.rm = F))) %>%
      dplyr::mutate(follow_up_days = cohort_end_date - .data$cohort_start_date) %>% 
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
                       dplyr::tibble(number_observations = .data$first_event %>% dplyr::tally()
                       %>% dplyr::pull(), reason = "Historical COVID-19"))
    first_event <- first_event %>% dplyr::select(-c(event)) %>% compute()
  }

  return(list(first_event,subs_events,attrition,reason_exclusion))
}

do_overlap <- function(cdm, base_cohort_id, outcome_cohort_id, overlap_cohort_id, washout = TRUE, tableName, overlapTableName) {
  base <- cdm[[BaseCohortsName]] %>% 
    dplyr::filter(.data$cohort_definition_id == base_cohort_id)
  outcome <- cdm[[tableName]] %>% 
    dplyr::filter(.data$cohort_definition_id == outcome_cohort_id)
  overlap <- base %>% 
    dplyr::inner_join(
      outcome %>% 
        dplyr::select(subject_id, outcome_date = cohort_start_date, 
                      outcome_end = cohort_end_date),
      by = "subject_id"
    ) %>%
    dplyr::mutate(cohort_definition_id = overlap_cohort_id) %>%
    dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
    dplyr::filter(.data$time_diff < -90 & .data$time_diff > -366) %>%
    dplyr::select(- .data$time_diff) %>% 
    dplyr::group_by(subject_id,cohort_start_date) %>%
    dbplyr::window_order(.data$outcome_date) %>%
    dplyr::mutate(seq = row_number()) %>%
    dbplyr::window_order() %>% 
    distinct() %>% 
    ungroup() %>%
    dplyr::filter(.data$seq == 1) %>% 
    dplyr::mutate(cohort_end_date = min(.data$cohort_end_date, .data$outcome_date)) %>%
    compute()
  # We are only asking for the first outcome event in the window of interest, per each index base event
  
  if(washout) {
    overlap <- overlap %>% 
      addEvent( cdm, tableName, outcome_cohort_id, c(-180,-1), order = "last") %>% 
      compute()
    if("event" %in% colnames(overlap)) {
      overlap <- overlap %>% 
        dplyr::mutate(date_previous = !!CDMConnector::datediff("cohort_start_date", event)) %>%
      dplyr::filter(is.na(.data$date_previous)) %>% dplyr::select(-c(event,date_previous)) %>% 
        compute()
    }
  }

  overlap <- overlap %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  appendPermanent(overlap, name = overlapTableName,
                  schema = results_database_schema)
}

do_overlap_LCany <- function(cdm, bases_cohort_id, outcomes_cohort_id, overlaps_cohort_id) {
  # We build this cohort from the beginning, i.e. getting the initial instantiated symptom cohorts
  # and enforcing washout for each symptom with itself. Then we join all of them together
  message("outcome ", 1)
  first_outcome <- outcomes_cohort_id[1]
  outcome <- cdm[[InitialCohortsName]] %>%
    dplyr::filter(.data$cohort_definition_id == first_outcome) %>%
    dplyr::filter(.data$cohort_start_date > as.Date("2020-09-01")) %>%
    addEvent(cdm, InitialCohortsName, first_outcome, c(-180,-1), order = "last") %>% 
    compute()
  if("event" %in% colnames(outcome)) {
    outcome <- outcome %>% dplyr::mutate(date_previous = !!CDMConnector::datediff("cohort_start_date", event)) %>%
    dplyr::filter(is.na(.data$date_previous)) %>% dplyr::select(-c(event,date_previous)) %>% compute()
  }
  outcomes_cohort_id <- outcomes_cohort_id[-1]
  for(i in outcomes_cohort_id) {
    message("outcome ", i)
    outcome_next <- cdm[[InitialCohortsName]] %>% 
      dplyr::filter(.data$cohort_definition_id == i) %>%
      dplyr::filter(.data$cohort_start_date > as.Date("2020-09-01")) %>%
      addEvent(cdm, InitialCohortsName, i, c(-180,-1), order = "last") %>%
      compute()
    if("event" %in% colnames(outcome_next)) {
      outcome_next <- outcome_next %>% dplyr::mutate(date_previous = !!CDMConnector::datediff("cohort_start_date", event)) %>%
         filter(is.na(.data$date_previous)) %>% dplyr::select(-c(event,date_previous)) %>% compute()
     }
    outcome <- outcome %>% dplyr::full_join(outcome_next, by = c("subject_id", "cohort_definition_id", "cohort_start_date", "cohort_end_date"))
  }
  # As symptoms, end date equals start date
  outcome <- outcome %>% dplyr::mutate(cohort_end_date = .data$cohort_start_date)
  # The overlap cohorts consists of individuals both in the base and outcome cohorts
  # We do it for the four of them as the creation of the outcome cohort is very expensive
  for(j in bases_cohort_id) {
    overlap_cohort_id <- overlaps_cohort_id[j]
    
    base <- cdm[[BaseCohortsName]] %>% 
      dplyr::filter(cohort_definition_id == j)
    overlap <- base %>% 
      dplyr::inner_join(
        outcome %>% 
          dplyr::select(subject_id, outcome_date = cohort_start_date, 
                        outcome_end = cohort_end_date),
        by = "subject_id"
      ) %>%
      dplyr::mutate(cohort_definition_id = overlap_cohort_id) %>%
      dplyr::mutate(time_diff = !!CDMConnector::datediff("outcome_date","cohort_start_date")) %>%
      dplyr::filter(.data$time_diff < -90 & .data$time_diff > -366) %>%
      dplyr::select(- .data$time_diff) %>% 
      dplyr::group_by(subject_id,cohort_start_date) %>%
      dbplyr::window_order(.data$outcome_date) %>%
      dplyr::mutate(seq = row_number()) %>%
      dbplyr::window_order() %>% 
      distinct() %>% 
      ungroup() %>%
      dplyr::filter(.data$seq == 1) %>% 
      dplyr::mutate(cohort_end_date = min(.data$cohort_end_date, .data$outcome_date)) %>%
      compute()
    # We are only asking for the first outcome event in the window of interest, per each index base event
    
    overlap <- overlap %>% dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
      compute()
    if(j == 1) {
      computeQuery(overlap, name = OverlapCohortsCName,
                   schema = results_database_schema, overwrite = TRUE)
    } else {
      appendPermanent(overlap, name = OverlapCohortsCName,
                   schema = results_database_schema)
    }

  }
}

create_outcome <- function(cdm, window, filter_start = TRUE, first_event = TRUE, end_outcome = TRUE, new_ids, tableName) {
  counter <- 1
  for(i in window){
    new_id <- new_ids[counter]
    name_cohort <- initialCohortSet$cohort_name[i]
    current <- cdm[[InitialCohortsName]] %>% 
      dplyr::filter(.data$cohort_definition_id == i) %>% dplyr::select(
        "subject_id",
        "cohort_start_date"
      ) %>% compute() 
    
    if(filter_start) {
      current <- current %>% dplyr::filter(.data$cohort_start_date > study_start_date) %>%
        compute()
    }
    
    if(first_event) {
      # Only get the first event
      current <- current %>% dplyr::group_by(subject_id) %>% 
        dbplyr::window_order(.data$cohort_start_date) %>%
        dplyr::mutate(seq = row_number()) %>% distinct() %>% 
        ungroup() %>% compute()
      current <- current %>% dplyr::filter(.data$seq == 1) %>% 
        dplyr::select(-.data$seq) %>% compute()
    }
    
    current <- current %>% dplyr::left_join(observation_death, by = c("subject_id")) %>%
      dplyr::mutate(cohort_end_date = lubridate::as_date(pmin(
        .data$observation_period_end_date, .data$death_date))) %>% compute()

    if(end_outcome) {
      current <- current %>% dplyr::mutate(cohort_end_date = .data$cohort_start_date)
    }
    
    current <- current %>% dplyr::mutate(cohort_definition_id = new_id) %>%
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
    if(new_id == 1) {
      computeQuery(current, name = tableName,
                      schema = results_database_schema, overwrite = TRUE)
    } else {
      appendPermanent(current, name = tableName,
                      schema = results_database_schema)
    }
    counter <- counter + 1
   }
}

create_any_cohort <- function(cdm, window, cohort_id, LC = FALSE, tableName) {
  cohorts <- cdm[[tableName]] %>%
    dplyr::filter(.data$cohort_definition_id %in% window)
  # For the LC any cohort, we don't want only the first event, as we will do washout in the overlap ones
  if(LC) {
    any_cohort <- cohorts %>% dplyr::select(-.data$cohort_definition_id) %>%
      dplyr::mutate(cohort_definition_id = cohort_id) %>% 
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
    
  } else {
    any_cohort <- cohorts %>% dplyr::select(-.data$cohort_definition_id) %>%
      dplyr::group_by(subject_id) %>% dbplyr::window_order(.data$cohort_start_date) %>%
      dplyr::mutate(seq = row_number()) %>% distinct() %>% 
      dplyr::filter(.data$seq == 1) %>% ungroup() %>%
      dplyr::mutate(cohort_definition_id = cohort_id) %>% 
      dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date)
  }
  
  appendPermanent(any_cohort, name = tableName,  schema = results_database_schema)
}

do_overlap_vacc <- function(cdm, base_id, new_id, tableName) {
  vaccinated_cohort <- cdm[[tableName]] %>%
    dplyr::filter(.data$cohort_definition_id == base_id) %>% 
    left_join(cdm[[VaccCohortsName]] %>%
                dplyr::filter(.data$cohort_definition_id == 1) %>% 
                dplyr::select(subject_id, "vacc_date" = "cohort_start_date"),
              by = "subject_id") %>%
    dplyr::filter(.data$cohort_start_date > .data$vacc_date) %>% 
    dplyr::filter(!(is.na(.data$vacc_date))) %>%
    dplyr::mutate(cohort_definition_id = new_id) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()
  nonvaccinated_cohort <- cdm[[tableName]] %>%
    dplyr::filter(.data$cohort_definition_id == base_id) %>% 
    left_join(cdm[[VaccCohortsName]] %>%
                dplyr::filter(cohort_definition_id == 2) %>%
                dplyr::select(subject_id, "vacc_end_date" = "cohort_end_date"),
              by = "subject_id") %>%
    dplyr::filter(.data$vacc_end_date > .data$cohort_start_date) %>%
    dplyr::mutate(cohort_end_date = pmin(.data$cohort_end_date,.data$vacc_end_date)) %>%
    dplyr::mutate(cohort_definition_id = new_id+1) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date) %>%
    compute()

  appendPermanent(vaccinated_cohort, name = tableName,
                  schema = results_database_schema)
  appendPermanent(nonvaccinated_cohort, name = tableName,
                  schema = results_database_schema)
  }

do_strata_calendar <- function(cdm, base_id, new_id) {
  cohort_delta <- cdm[["studyathon_final_cohorts"]] %>%
    dplyr::filter(cohort_definition_id == base_id) %>%
    dplyr::filter(cohort_start_date < omicron_start_date) %>%
    mutate(cohort_definition_id = new_id) %>% compute()
  cohort_omicron <- cdm[["studyathon_final_cohorts"]] %>%
    dplyr::filter(cohort_definition_id == base_id) %>%
    dplyr::filter(cohort_start_date >= omicron_start_date) %>%
    mutate(cohort_definition_id = new_id+1) %>% compute()
  appendPermanent(cohort_delta, name = "studyathon_final_cohorts",
                  schema = results_database_schema)
  appendPermanent(cohort_omicron, name = "studyathon_final_cohorts",
                  schema = results_database_schema)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(cohort_definition_id = c(new_id,new_id+1), 
                                             cohort_name =c(paste0("Cohort ",i," Delta" ),
                                                           paste0("Cohort ",i," Omicron" ))))
  
}

do_sex_strata <- function(cohort_id, new_id, tableName) {
  sex_strata <- cdm[[tableName]] %>% dplyr::filter(.data$cohort_definition_id == cohort_id)
  females <- sex_strata %>% PatientProfiles::addSex(cdm) %>% dplyr::filter(sex == "Female") %>% 
    dplyr::mutate(cohort_definition_id = new_id) %>% compute()
  males <- sex_strata %>% PatientProfiles::addSex(cdm) %>% 
    dplyr::filter(sex == "Male") %>% dplyr::mutate(cohort_definition_id = new_id + 1) %>%
    compute()
  appendPermanent(females, name = tableName,  schema = results_database_schema)
  appendPermanent(males, name = tableName,  schema = results_database_schema)
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = tableName,
                                             cohort_definition_id = c(new_id,new_id+1), 
                                             cohort_name =c(paste0("Cohort ",i," females" ),paste0("Cohort ",i," males" ))))
}

do_age_strata <- function(cohort_id, new_id, tableName) {
  age_strata <- cdm[[tableName]] %>% dplyr::filter(.data$cohort_definition_id == cohort_id)
  age1 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(0:2)) %>% 
    dplyr::mutate(cohort_definition_id = new_id) %>% compute()
  age2 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(3:5)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+1) %>% compute()
  age3 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(6:9)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+2) %>% compute()
  age4 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(10:13)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+3) %>% compute()
  age5 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(14:17)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+4) %>% compute()
  age6 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(18:40)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+5) %>% compute()
  age7 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(41:64)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+6) %>% compute()
  age8 <- age_strata %>% PatientProfiles::addAge(cdm) %>% 
    dplyr::filter(.data$age %in% c(65:120)) %>% 
    dplyr::mutate(cohort_definition_id = new_id+7) %>% compute()
  
  appendPermanent(age1, name = tableName,  schema = results_database_schema)
  appendPermanent(age2, name = tableName,  schema = results_database_schema)
  appendPermanent(age3, name = tableName,  schema = results_database_schema)
  appendPermanent(age4, name = tableName,  schema = results_database_schema)
  appendPermanent(age5, name = tableName,  schema = results_database_schema)
  appendPermanent(age6, name = tableName,  schema = results_database_schema)
  appendPermanent(age7, name = tableName,  schema = results_database_schema)
  appendPermanent(age8, name = tableName,  schema = results_database_schema)
  
  names_final_cohorts <- rbind(names_final_cohorts,
                               dplyr::tibble(table_name = tableName,
                                             cohort_definition_id = c(new_id : new_id+7), 
                                             cohort_name =c(paste0("Cohort ",i," age (0,2)" ),
                                                            paste0("Cohort ",i," age (3,5)" ),
                                                            paste0("Cohort ",i," age (6,9)" ),
                                                            paste0("Cohort ",i," age (10,13)" ),
                                                            paste0("Cohort ",i," age (14,17)" ),
                                                            paste0("Cohort ",i," age (18,40)" ),
                                                            paste0("Cohort ",i," age (41,64)" ),
                                                            paste0("Cohort ",i," age (65,120)" ))))
  
}
