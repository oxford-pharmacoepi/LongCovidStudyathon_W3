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

addFlagEvent_in <- function(x, cdm, eventTableName, filter = NULL, window = c(NA, NA), name = "number_event", eventAt = "cohort_start_date", eventDate = "cohort_start_date", compute = TRUE) {
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
      ~ dplyr::if_else(is.na(.x), 0, 1)
    )) %>%
    dplyr::select(dplyr::all_of(c(colnames(x), name))) 
  if (isTRUE(compute)) {
    events <- events %>% dplyr::compute()
  }
  return(events)
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
