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

getLargeScaleCharacteristics <- function(cdm, targetCohortName, targetCohortId = NULL, temporalWindows = list( c(NA, -366), c(-365, -91), c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),c(31, 365), c(91, 365), c(366, NA)),
                                         tablesToCharacterize = c("condition_occurrence", "drug_era","procedure_occurrence", "measurement"),
                                         overlap = TRUE, minimumCellCount = 5) {
  get_start_date <- list(
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date"
  )
  
  get_end_date <- list(
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_end_date",
    "procedure_occurrence" = NULL,
    "device_exposure" = "device_exposure_end_date",
    "measurement" = NULL,
    "observation" = NULL,
    "drug_era" = "drug_era_end_date",
    "condition_era" = "condition_era_end_date",
    "specimen" = NULL
  )
  
  get_concept <- list(
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id"
  )
  
  errorMessage <- checkmate::makeAssertCollection()
  
  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)
  
  # check targetCohortName
  checkmate::assertCharacter(targetCohortName, len = 1, add = errorMessage)
  
  # check that targetCohortName point to a table that is a cohort
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[targetCohortName]])),
    add = errorMessage
  )
  
  
  #check input cohort cannot have missing in the following columns
  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_definition_id")),
    add = errorMessage
  )
  
  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("subject_id")),
    add = errorMessage
  )
  
  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_start_date")),
    add = errorMessage
  )
  
  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_end_date")),
    add = errorMessage
  )
  
  
  # check targetCohortId
  checkmate::assertIntegerish(
    targetCohortId,
    lower = 1,
    null.ok = TRUE,
    add = errorMessage
  )
  
  # check temporalWindows
  checkmate::assertList(temporalWindows, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(unlist(lapply(temporalWindows, length)) == 2),
    add = errorMessage
  )
  
  # check tablesToCharacterize
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% c(
      "visit_occurrence", "condition_occurrence", "drug_exposure",
      "procedure_occurrence", "device_exposure", "measurement", "observation",
      "drug_era", "condition_era", "specimen"
    )),
    add = errorMessage
  )
  
  # overlap
  checkmate::assertLogical(overlap, any.missing = FALSE, add = errorMessage)
  
  # minimumCellCount
  checkmate::assertCount(minimumCellCount, add = errorMessage)
  
  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)
  
  if (length(overlap) > 1) {
    if (length(overlap) != length(tablesToCharacterize)) {
      stop("If length(overlap)>1 then length(overlap) = length(tablesToCharacterize)")
    }
  } else {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }
  
  # write temporal windows tibble
  temporalWindows <- lapply(temporalWindows, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(
      window_start = x[1], window_end = x[2], window_name = nam
    )
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(window_id = dplyr::row_number()) %>%
    dplyr::select("window_id", "window_name", "window_start", "window_end")
  
  # filter the cohort and get the targetCohortId if not specified
  if (!is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  } else {
    targetCohort <- cdm[[targetCohortName]]
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }
  
  # get the distinct subjects with their observation period
  subjects <- targetCohort %>%
    dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "person_id"
    ) %>%
    dplyr::compute()
  
  # for each one of the windows we get which are the subjects contributing to it
  subjects_denominator <- subjects %>%
    dplyr::mutate(dif_start = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_start_date"
    ))) %>%
    dplyr::mutate(dif_end = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_end_date"
    ))) %>%
    dplyr::mutate(to_merge = 1) %>%
    dplyr::inner_join(
      temporalWindows %>%
        dplyr::mutate(to_merge = 1),
      by = "to_merge",
      copy = TRUE
    ) %>%
    dplyr::filter(
      is.na(.data$window_end) | .data$dif_start <= .data$window_end
    ) %>%
    dplyr::filter(
      is.na(.data$window_start) | .data$dif_end >= .data$window_start
    ) %>%
    dplyr::select(
      "person_id", "cohort_start_date", "cohort_end_date", "window_id"
    ) %>%
    dplyr::compute()
  
  # get the codes observed in each window for each one of the subjects, only
  # events in the observation window will be observed. The result is a
  # temporary table in the database
  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    overlap.k <- overlap[tablesToCharacterize == table_name]
    # get start date depending on the table
    start_date <- get_start_date[[table_name]]
    # get end date depending on the table
    end_date <- get_end_date[[table_name]]
    # get concept id depending on the table
    concept_id <- get_concept[[table_name]]
    # subset the table to the study subjects
    study_table <- cdm[[table_name]] %>%
      dplyr::inner_join(subjects, by = "person_id") %>%
      # rename start date
      dplyr::rename("start_date" = .env$start_date)
    # rename or create end date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      study_table <- study_table %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    study_table <- study_table %>%
      # rename concept id and get concept name
      dplyr::rename("concept_id" = .env$concept_id) %>%
      dplyr::left_join(
        cdm$concept %>%
          dplyr::select("concept_id", "concept_name"),
        by = "concept_id"
      ) %>%
      # obtain observations inside the observation period only
      dplyr::filter(.data$start_date <= .data$observation_period_end_date) %>%
      dplyr::filter(.data$end_date >= .data$observation_period_start_date) %>%
      # obtain the time difference between the start of the event and the
      # cohort start date
      dplyr::mutate(days_difference_start = dbplyr::sql(CDMConnector::datediff(
        start = "cohort_start_date",
        end = "start_date"
      )))
    # obtain the time difference between the end of the event and the cohort
    # start date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = .data$days_difference_start)
    } else {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = dbplyr::sql(CDMConnector::datediff(
          start = "cohort_start_date",
          end = "end_date"
        )))
    }
    study_table <- study_table %>%
      # merge the table that we want to characterize with all the temporal
      # windows
      dplyr::mutate(to_merge = 1) %>%
      dplyr::inner_join(
        temporalWindows %>%
          dplyr::mutate(to_merge = 1),
        by = "to_merge",
        copy = TRUE
      ) %>%
      # get only the events that start before the end of the window
      dplyr::filter(
        is.na(.data$window_end) |
          .data$days_difference_start <= .data$window_end
      ) %>%
      # get only events that end/start (depending if overlap = TRUE/FALSE) after
      # the start of the window
      dplyr::filter(
        is.na(.data$window_start) |
          .data$days_difference_end >= .data$window_start
      ) %>%
      # get only distinct events per window id
      dplyr::select(
        "person_id", "cohort_start_date", "cohort_end_date", "window_id",
        "concept_id", "concept_name"
      ) %>%
      dplyr::distinct() %>%
      dplyr::compute()
    
    return(study_table)
  })
  
  # union all the tables into a temporal table
  for (i in 1:length(characterizedTable)) {
    if (i == 1) {
      characterizedTables <- characterizedTable[[i]] %>%
        dplyr::mutate(table_id = .env$i)
    } else {
      characterizedTables <- characterizedTables %>%
        dplyr::union_all(
          characterizedTable[[i]] %>%
            dplyr::mutate(table_id = .env$i)
        )
    }
  }
  characterizedTables <- characterizedTables %>% dplyr::compute()
  
  
  
  # if we want to summarise the data we count the number of counts for each
  # event, window and table
  for (k in 1:length(targetCohortId)) {
    characterizedCohort <- targetCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::select(
        "person_id" = "subject_id", "cohort_start_date", "cohort_end_date"
      ) %>%
      dplyr::inner_join(
        characterizedTables,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$concept_id, .data$concept_name, .data$window_id, .data$table_id) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("concept_count" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])
    denominator <- targetCohort %>%
      dplyr::rename("person_id" = "subject_id") %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::inner_join(
        subjects_denominator,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$window_id) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("denominator_count" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])
    if (k == 1) {
      characterizedCohortk <- characterizedCohort
      denominatork <- denominator
    } else {
      characterizedCohortk <- characterizedCohortk %>%
        dplyr::union_all(characterizedCohort)
      denominatork <- denominatork %>%
        dplyr::union_all(denominator)
    }
  }
  characterizedTables <- characterizedCohortk %>%
    dplyr::mutate(obscured_counts = dplyr::if_else(
      .data$concept_count < .env$minimumCellCount, TRUE, FALSE
    )) %>%
    dplyr::relocate("cohort_definition_id", .before = "concept_id")
  
  #  denominatork <- denominatork %>%
  #    dplyr::select(-"cohort_definition_id") %>%
  #    dplyr::distinct()
  
  subjects_denominator <- denominatork %>%
    dplyr::mutate(obscured_in_observation = dplyr::if_else(
      .data$denominator_count < .env$minimumCellCount, TRUE, FALSE
    ))
  # %>%
  # dplyr::relocate("cohort_definition_id", .before = "window_id")
  
  tablesToCharacterize <- tibble::tibble(
    table_id = seq(length(tablesToCharacterize)),
    table_name = tablesToCharacterize,
    overlap = overlap
  )
  
  characterizedTables <- characterizedTables %>%
    dplyr::mutate(concept_count = dplyr::if_else(.data$obscured_counts,
                                                 paste0("<", minimumCellCount),
                                                 as.character(.data$concept_count)
    )) %>%
    dplyr::select(
      "cohort_definition_id", "table_id", "window_id", "concept_id",
      "concept_name", "concept_count"
    )
  
  subjects_denominator <- subjects_denominator %>%
    dplyr::mutate(denominator_count = dplyr::if_else(.data$obscured_in_observation,
                                                     paste0("<", minimumCellCount),
                                                     as.character(.data$denominator_count)
    )) %>%
    dplyr::select("window_id", "denominator_count", "cohort_definition_id")
  
  result <- characterizedTables %>%
    dplyr::left_join(tablesToCharacterize, by = "table_id") %>%
    dplyr::left_join(subjects_denominator,
                     by = c(
                       "window_id", "cohort_definition_id"
                     )
    ) %>%
    dplyr::left_join(temporalWindows, by = "window_id") %>%
    dplyr::select(
      "cohort_definition_id", "table_id", "table_name",
      "window_id", "window_name", "concept_id",
      "concept_name", "concept_count", "denominator_count",
      "overlap"
    ) %>%
    dplyr::mutate(concept_type = "Standard")
  
  return(result)
}

supressCount <- function(result,minimumCellCounts = 5,globalVariables = c("number_observations", "number_subjects"), estimatesToObscure = "count") {
  
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  column1Check <- c("cohort_definition_id") %in% colnames(result)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `cohort_definition_id` is not a column of result"
    )
  }
  column2Check <- c("variable") %in% colnames(result)
  if (!isTRUE(column2Check)) {
    errorMessage$push(
      "- `variable` is not a column of result"
    )
  }
  column3Check <- c("estimate") %in% colnames(result)
  if (!isTRUE(column3Check)) {
    errorMessage$push(
      "- `estimate` is not a column of result"
    )
  }
  column4Check <- c("value") %in% colnames(result)
  if (!isTRUE(column4Check)) {
    errorMessage$push(
      "- `value` is not a column of result"
    )
  }
  checkmate::assertIntegerish(minimumCellCounts, len = 1,
                              add = errorMessage,
  )
  checkmate::assertCharacter(globalVariables,
                             add = errorMessage,
  )
  checkmate::assertCharacter(estimatesToObscure,
                             add = errorMessage,
  )
  checkmate::reportAssertions(collection = errorMessage)
  
  # Start code
  values_to_osbcure <- suppressWarnings(as.numeric(result$value)) <
    minimumCellCounts &
    suppressWarnings(as.numeric(result$value)) > 0
  obscured_values <- result$estimate %in% estimatesToObscure & values_to_osbcure
  obscured_cohort <- unique(result$cohort_definition_id[
    result$estimate %in% estimatesToObscure &
      result$variable %in% globalVariables &
      values_to_osbcure
  ])
  result$value[obscured_values] <- paste0("<", minimumCellCounts)
  result$value[
    result$cohort_definition_id %in% obscured_cohort
  ] <- as.character(NA)
  result$value[
    result$cohort_definition_id %in% obscured_cohort &
      result$variable %in% globalVariables
  ] <- paste0("<", minimumCellCounts)
  return(result)
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