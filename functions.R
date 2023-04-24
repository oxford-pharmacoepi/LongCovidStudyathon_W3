addAge <- function(x, cdm, ageAt = "cohort_start_date", defaultMonth = 1, defaultDay = 1, imposeMonth = TRUE, imposeDay = TRUE, compute = TRUE) {
  defaultMonth <- as.integer(defaultMonth)
  defaultDay <- as.integer(defaultDay)
  
  person <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", dplyr::all_of(ageAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    )
  
  if (imposeMonth == TRUE) {
    person <- person %>%
      dplyr::mutate(month_of_birth = .env$defaultMonth)
  } else {
    person <- person %>%
      dplyr::mutate(month_of_birth = dplyr::if_else(
        is.na(.data$month_of_birth),
        .env$defaultMonth,
        .data$month_of_birth
      ))
  }
  
  if (imposeDay == TRUE) {
    person <- person %>%
      dplyr::mutate(day_of_birth = .env$defaultDay)
  } else {
    person <- person %>%
      dplyr::mutate(day_of_birth = dplyr::if_else(
        is.na(.data$day_of_birth),
        .env$defaultDay,
        .data$day_of_birth
      ))
  }
  
  person <- person %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(year_of_birth1 = as.character(as.integer(.data$year_of_birth))) %>%
    dplyr::mutate(month_of_birth1 = as.character(as.integer(.data$month_of_birth))) %>%
    dplyr::mutate(day_of_birth1 = as.character(as.integer(.data$day_of_birth))) %>%
    dplyr::mutate(birth_date = as.Date(paste0(
      .data$year_of_birth1, "-",
      .data$month_of_birth1, "-",
      .data$day_of_birth1
    ))) %>%
    dplyr::mutate(age = floor(dbplyr::sql(sqlGetAge(
      dialect = CDMConnector::dbms(cdm),
      dob = "birth_date",
      dateOfInterest = ageAt
    )))) %>%
    dplyr::select("subject_id", dplyr::all_of(ageAt), "age") %>%
    dplyr::right_join(x, by = c("subject_id", ageAt)) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "age")
  if (isTRUE(compute)) {
    person <- person %>% dplyr::compute()
  }
  return(person)
}

sqlGetAge <- function(dialect, dob, dateOfInterest) {
  SqlRender::translate(
    SqlRender::render("((YEAR(@date_of_interest) * 10000 + MONTH(@date_of_interest) * 100 +
                      DAY(@date_of_interest)-(YEAR(@dob)* 10000 + MONTH(@dob) * 100 + DAY(@dob))) / 10000)",
                      dob = dob,
                      date_of_interest = dateOfInterest
    ),
    targetDialect = dialect
  )
}

addPriorHistory <- function(x, cdm, priorHistoryAt = "cohort_start_date", compute = TRUE) {
  x <- cdm[["observation_period"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "observation_period_start_date"
    ) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", dplyr::all_of(priorHistoryAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(prior_history = CDMConnector::datediff(
      start = "observation_period_start_date",
      end = !!priorHistoryAt
    )) %>%
    dplyr::right_join(
      x,
      by = c("subject_id", priorHistoryAt)
    ) %>%
    dplyr::select(dplyr::all_of(colnames(x)), "prior_history")
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}

addSex <- function(x, cdm, compute = TRUE) {
  x <- cdm[["person"]] %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::inner_join(
      x %>% dplyr::select("subject_id") %>% dplyr::distinct(),
      by = c("subject_id")
    ) %>%
    dplyr::mutate(sex = dplyr::case_when(
      .data$gender_concept_id == 8507 ~ "Male",
      .data$gender_concept_id == 8532 ~ "Female",
      TRUE ~ as.character(NA)
    )) %>%
    dplyr::select("subject_id", "sex") %>%
    dplyr::right_join(x, by = "subject_id") %>%
    dplyr::select(dplyr::all_of(colnames(x)), "sex")
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
  return(x)
}

addNumberVisit <- function(x, cdm, id_visit, window = c(NA, NA), name = "number_visit", compute = TRUE) {
  x <- cdm[["visit_occurrence"]] %>%
    dplyr::select(
      "subject_id" = "person_id", "visit_concept_id", "visit_start_date"
    ) %>%
    dplyr::filter(visit_concept_id %in% id_visit) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "cohort_start_date") %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[1]) <=
        .data$visit_start_date
    ) %>%
    dplyr::filter(
      CDMConnector::dateadd("cohort_start_date", window[2]) >=
        .data$visit_start_date
    ) %>%
    dplyr::group_by(
      .data$subject_id, .data$cohort_start_date
    ) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::compute() %>%
    dplyr::right_join(
      x,
      by = c("subject_id", "cohort_start_date")
    ) %>%
    dplyr::mutate(!!name := dplyr::if_else(is.na(.data[[name]]), 0, .data[[name]])) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name)))
  if (isTRUE(compute)) {
    x <- x %>% dplyr::compute()
  }
}

addNumberEvent <- function(x, cdm, eventTableName, filter = NULL, window = c(NA, NA), name = "number_event", eventAt = "cohort_start_date", eventDate = "cohort_start_date", compute = TRUE) {
  events <- cdm[[eventTableName]]
  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      events <- events %>%
        dplyr::filter_at(
          dplyr::vars(dplyr::all_of(namesFilter[k])), 
          dplyr::any_vars(. == !!filter[[k]])
        )
    }
  }
  if ("person_id" %in% colnames(events)) {
    events <- events %>%
      dplyr::rename("subject_id" = "person_id")
  }
  events <- events %>%
    dplyr::select("subject_id","event_date" = dplyr::all_of(eventDate)) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "event_at" = dplyr::all_of(eventAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(date_dif = !!CDMConnector::datediff("event_at", "event_date"))
  if (!is.na(window[1])) {
    events <- events %>%
      dplyr::filter(.data$date_dif >= !!window[1])
  }
  if (!is.na(window[2])) {
    events <- events %>%
      dplyr::filter(.data$date_dif <= !!window[2])
  }
  events <- events %>%
    dplyr::group_by(.data$subject_id, .data$event_at) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!eventAt := "event_at") %>%
    dplyr::right_join(
      x,
      by = c("subject_id", eventAt)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(name), 
      ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name))) 
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
}

addNumberEvent_in <- function(x, cdm, eventTableName, filter = NULL, window = c(NA, NA), name = "number_event", eventAt = "cohort_start_date", eventDate = "cohort_start_date", compute = TRUE) {
  events <- cdm[[eventTableName]]
  if (!is.null(filter)) {
    namesFilter <- names(filter)
    for (k in 1:length(filter)) {
      events <- events %>%
        dplyr::filter_at(
          dplyr::vars(dplyr::all_of(namesFilter[k])), 
          dplyr::any_vars(. %in% !!filter[[k]])
        )
    }
  }
  if ("person_id" %in% colnames(events)) {
    events <- events %>%
      dplyr::rename("subject_id" = "person_id")
  }
  events <- events %>%
    dplyr::select("subject_id","event_date" = dplyr::all_of(eventDate)) %>%
    dplyr::inner_join(
      x %>%
        dplyr::select("subject_id", "event_at" = dplyr::all_of(eventAt)) %>%
        dplyr::distinct(),
      by = "subject_id"
    ) %>%
    dplyr::mutate(date_dif = !!CDMConnector::datediff("event_at", "event_date"))
  if (!is.na(window[1])) {
    events <- events %>%
      dplyr::filter(.data$date_dif >= !!window[1])
  }
  if (!is.na(window[2])) {
    events <- events %>%
      dplyr::filter(.data$date_dif <= !!window[2])
  }
  events <- events %>%
    dplyr::group_by(.data$subject_id, .data$event_at) %>%
    dplyr::summarise(!!name := dplyr::n(), .groups = "drop") %>%
    dplyr::rename(!!eventAt := "event_at") %>%
    dplyr::right_join(
      x,
      by = c("subject_id", eventAt)
    ) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(name), 
      ~ dplyr::if_else(is.na(.x), 0, .x)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name))) 
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
}

addEvent <- function(x, cdm, eventTableName, eventId = NULL, window = c(NA, NA), name = "event", eventDate = "cohort_start_date", eventAt = "cohort_start_date", order = "first", compute = TRUE) {
  eventTable <- cdm[[eventTableName]]
  if (!is.null(eventId)) {
    eventTable <- eventTable %>% dplyr::filter(.data$cohort_definition_id %in% .env$eventId)
  }
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", eventAt))))
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "all") {
    xx <- xx %>%
      dbplyr::window_order(.data$event_date) %>%
      dplyr::mutate(nam = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nam = paste0(.env$name, "_", .data$nam)) %>%
      tidyr::pivot_wider(names_from = "nam", values_from = "event_date")
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}

addVisit <- function(x, cdm, eventTableName, eventId = NULL, window = c(NA, NA), name = "event", eventDate = "cohort_start_date", eventAt = "cohort_start_date", order = "first", compute = TRUE) {
  eventTable <- cdm[[eventTableName]]
  if (!is.null(eventId)) {
    eventTable <- eventTable %>% dplyr::filter(.data$visit_concept_id %in% .env$eventId)
  }
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("subject_id", eventAt))))
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        !!name := max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "all") {
    xx <- xx %>%
      dbplyr::window_order(.data$event_date) %>%
      dplyr::mutate(nam = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(nam = paste0(.env$name, "_", .data$nam)) %>%
      tidyr::pivot_wider(names_from = "nam", values_from = "event_date")
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}

addOverlap <- function(x, cdm, cohortName, id, name, window = c(NA, 0)) {
  xx <- x %>%
    dplyr::select("subject_id", "cohort_start_date") %>%
    dplyr::inner_join(
      cdm[[cohortName]] %>%
        dplyr::filter(cohort_definition_id %in% !!id) %>%
        dplyr::inner_join(
          dplyr::tibble(
            cohort_definition_id = id,
            overlap_name = name
          ),
          by = "cohort_definition_id",
          copy = TRUE
        ) %>%
        dplyr::select(
          "subject_id", 
          "overlap_name", 
          "overlap_start" = "cohort_start_date", 
          "overlap_end" = "cohort_end_date"
        ),
      by = "subject_id"
    )
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(overlap_end >= CDMConnector::dateadd("cohort_start_date", window[1]))
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(overlap_start <= CDMConnector::dateadd("cohort_start_date", window[2]))
  }
  xx <- xx %>%
    dplyr::select("subject_id", "cohort_start_date", "overlap_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = 1) %>%
    tidyr::pivot_wider(names_from = "overlap_name", values_from = "value", values_fill = 0) %>%
    dplyr::right_join(x, by = c("subject_id", "cohort_start_date"))
  for (nam in name) {
    if(nam %in% colnames(xx)) {
      xx <- xx %>%
        dplyr::mutate(!!nam := if_else(is.na(.data[[nam]]), 0, .data[[nam]]))
    } else {
      xx <- xx %>%
        dplyr::mutate(!!nam := 0)
    }
  }
  return(xx %>% compute())
}

addMultipleEvent <- function(x, cdm, eventTableName, eventId = NULL, window = c(NA, NA), name = "event", eventDate = "cohort_start_date", eventAt = "cohort_start_date", order = "first", compute = TRUE) {
  eventTable <- cdm[[eventTableName]]
  if (!is.null(eventId)) {
    eventTable <- eventTable %>% dplyr::filter(.data$cohort_definition_id %in% .env$eventId)
  }
  if ("person_id" %in% colnames(eventTable)) {
    eventTable <- eventTable %>% dplyr::rename("subject_id" = "person_id")
  }
  xx <- x %>%
    dplyr::select(dplyr::all_of(c("subject_id", eventAt))) %>%
    dplyr::inner_join(
      eventTable %>%
        dplyr::select("event_id" = "cohort_definition_id", "subject_id", "event_date" = dplyr::all_of(eventDate)),
      by = "subject_id"
    ) %>%
    dplyr::mutate(dif_time = !!CDMConnector::datediff(eventAt, "event_date"))
  if (!is.na(window[1])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time >= !!window[1])
  }
  if (!is.na(window[2])) {
    xx <- xx %>%
      dplyr::filter(.data$dif_time <= !!window[2])
  }
  xx <- xx %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("event_id", "subject_id", eventAt))))
  if (order == "first") {
    xx <- xx %>%
      dplyr::summarise(
        event_date = min(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else if (order == "last") {
    xx <- xx %>%
      dplyr::summarise(
        event_date = max(.data$event_date, na.rm = TRUE),
        .groups = "drop"
      )
  }
  xx <- xx %>%
    dplyr::inner_join(
      dplyr::tibble(
        event_id = .env$eventId,
        event_name = .env$name
      ),
      by = "event_id",
      copy = TRUE
    ) %>%
    select(-"event_id") %>%
    tidyr::pivot_wider(names_from = "event_name", values_from = "event_date")
  for (nam in name) {
    if (!(nam %in% colnames(xx))) {
      xx <- xx %>% mutate(!!nam := as.Date(NA))
    }
  }
  xx <- x %>% dplyr::left_join(xx, by = c("subject_id", eventAt))
  if (isTRUE(compute)) {
    xx <- xx %>% dplyr::compute()
  }
  return(xx)
}

addSex <- function(x, cdm, name = "sex", tablePrefix = NULL) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  
  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }
  
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  
  checkmate::reportAssertions(collection = errorMessage)
  
  errorMessage <- checkmate::makeAssertCollection()
  
  PersonExists <- "person" %in% names(cdm)
  if (!isTRUE(PersonExists)) {
    errorMessage$push(
      "- `person` is not found in cdm"
    )
  }
  PersonCheck <- inherits(cdm$person, "tbl_dbi")
  if (!isTRUE(PersonCheck)) {
    errorMessage$push(
      "- table `person` is not of the right type"
    )
  }
  
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )
  
  checkmate::reportAssertions(collection = errorMessage)
  
  errorMessage <- checkmate::makeAssertCollection()
  
  columnCheck <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(columnCheck)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }
  
  column2Check <- "person_id" %in% colnames(cdm$person)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$person"
    )
  }
  
  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )
  
  checkmate::reportAssertions(collection = errorMessage)
  
  # Start code
  
  name <- rlang::enquo(name)
  if("subject_id" %in% colnames(x)){
    x <- cdm[["person"]] %>%
      dplyr::rename("subject_id" = "person_id") %>%
      dplyr::inner_join(
        x %>% dplyr::select("subject_id") %>% dplyr::distinct(),
        by = c("subject_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("subject_id", !!name) %>%
      dplyr::right_join(x, by = "subject_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  } else {
    x <- cdm[["person"]] %>%
      dplyr::inner_join(
        x %>% dplyr::select("person_id") %>% dplyr::distinct(),
        by = c("person_id")
      ) %>%
      dplyr::mutate(!!name := dplyr::case_when(
        .data$gender_concept_id == 8507 ~ "Male",
        .data$gender_concept_id == 8532 ~ "Female",
        TRUE ~ as.character(NA)
      )) %>%
      dplyr::select("person_id", !!name) %>%
      dplyr::right_join(x, by = "person_id") %>%
      dplyr::select(dplyr::all_of(colnames(x)), !!name)
  }
  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)
}

addInObservation <- function(x, cdm, observationAt = "cohort_start_date", name = "in_observation", tablePrefix = NULL) {
  
  ## check for standard types of user error
  
  errorMessage <- checkmate::makeAssertCollection()
  
  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }
  
  cdmCheck <- inherits(cdm, "cdm_reference")
  if (!isTRUE(cdmCheck)) {
    errorMessage$push(
      "- cdm must be a CDMConnector CDM reference object"
    )
  }
  
  checkmate::reportAssertions(collection = errorMessage)
  
  errorMessage <- checkmate::makeAssertCollection()
  
  ObsperiodExists <- "observation_period" %in% names(cdm)
  if (!isTRUE(ObsperiodExists)) {
    errorMessage$push(
      "- `observation_period` is not found in cdm"
    )
  }
  
  cdmObsPeriodCheck <- inherits(cdm$observation_period, "tbl_dbi")
  if (!isTRUE(cdmObsPeriodCheck)) {
    errorMessage$push(
      "- `observation_period` in cdm is not a table "
    )
  }
  checkmate::reportAssertions(collection = errorMessage)
  
  errorMessage <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(observationAt, len = 1,
                             add = errorMessage,
  )
  
  column1Check <- observationAt %in% colnames(x)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `observationAt` is not a column of x"
    )
  }
  
  column2Check <- ("subject_id" %in% colnames(x) || "person_id" %in% colnames(x))
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- neither `subject_id` nor `person_id` are columns of x"
    )
  }
  
  column3Check <- "person_id" %in% colnames(cdm$observation_period)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `person_id` is not a column of cdm$observation_period"
    )
  }
  
  column4Check <- "observation_period_start_date" %in% colnames(cdm$observation_period)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `observation_period_start_date` is not a column of cdm$observation_period"
    )
  }
  
  column5Check <- "observation_period_end_date" %in% colnames(cdm$observation_period)
  if (!isTRUE(column5Check)) {
    errorMessage$push(
      "- `observation_period_end_date` is not a column of cdm$observation_period"
    )
  }
  
  checkmate::assertCharacter(name, len = 1,
                             add = errorMessage,
  )
  
  checkmate::assertCharacter(
    tablePrefix, len = 1, null.ok = TRUE, add = errorMessage
  )
  
  checkmate::reportAssertions(collection = errorMessage)
  
  # Start code
  name = rlang::enquo(name)
  
  if("subject_id" %in% colnames(x)) {
    x <- x %>%
      dplyr::left_join(
        cdm$observation_period %>%
          dplyr::select(
            "subject_id" = "person_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = "subject_id"
      ) %>%
      dplyr::mutate(
        !!name := dplyr::if_else(
          .data[[observationAt]] >= .data$observation_period_start_date &
            .data[[observationAt]] <= .data$observation_period_end_date,
          1,
          0
        )
      ) %>%
      dplyr::select(
        -"observation_period_start_date", - "observation_period_end_date"
      )
  } else {
    x <- x %>%
      dplyr::left_join(
        cdm$observation_period %>%
          dplyr::select(
            "person_id",
            "observation_period_start_date",
            "observation_period_end_date"
          ),
        by = "person_id"
      ) %>%
      dplyr::mutate(
        !!name := dplyr::if_else(
          .data[[observationAt]] >= .data$observation_period_start_date &
            .data[[observationAt]] <= .data$observation_period_end_date,
          1,
          0
        )
      ) %>%
      dplyr::select(
        -"observation_period_start_date", - "observation_period_end_date"
      )
  }
  
  if(is.null(tablePrefix)){
    x <- x %>%
      CDMConnector::computeQuery()
  } else {
    x <- x %>%
      CDMConnector::computeQuery(name = paste0(tablePrefix,
                                               "_person_sample"),
                                 temporary = FALSE,
                                 schema = attr(cdm, "write_schema"),
                                 overwrite = TRUE)
  }
  return(x)
  
}