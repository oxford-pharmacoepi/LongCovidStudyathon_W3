do_vaccination_characterisation <- function(cohort_ids_interest, stem_name) {
  cohorts_interest <- cdm[["studyathon_final_cohorts"]] %>% 
    dplyr::filter(cohort_definition_id %in% cohort_ids_interest)
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 350,
                             window = c(NA,0),
                             value = "binary",
                             name = "first_dose") %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 351,
                             name = "second_dose",
                             window = c(NA,0),
                             value = "binary") %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 352,
                             name = "third_dose",
                             window = c(NA,0),
                             value = "binary") %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 103,
                             name = "last_dose",
                             value = "date",
                             window = c(NA,0), order = "last") %>% compute()
  
  cohorts_interest <- cohorts_interest %>% 
    mutate(dose = first_dose + second_dose + third_dose) %>%
    mutate(last_dose_days = CDMConnector::datediff("last_dose","cohort_start_date")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date,dose,last_dose_days) %>% 
    compute()
  
  # More output than this??
  vacc_counts <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id, dose) %>% tally()
  vacc_lastdose <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::summarise(mean_lastdose = mean(last_dose_days), 
                     var_lastdose = var(last_dose_days),
                     .groups = 'drop')
  
  write.csv(
    vacc_counts,
    file = here::here(output_lsc, paste0("Vaccination_doses_",stem_name,".csv")),
    row.names = FALSE
  )
  write.csv(
    vacc_lastdose,
    file = here::here(output_lsc, paste0("Vaccination_last_dose_",stem_name,".csv")),
    row.names = FALSE
  )
}

do_lsc <- function(cohort_ids_interest, stem_name) {
  charac <- LargeScaleCharacteristics::getLargeScaleCharacteristics(
    cdm, targetCohortName = "studyathon_final_cohorts", 
    targetCohortId = cohort_ids_interest)
  for(i in 1:length(charac)) {
    write.csv(
      charac[[i]],
      file = here::here(output_lsc, paste0("Characterisation_",stem_name,"_",names(charac)[i],".csv")),
      row.names = FALSE
    )
  }
}

do_hu <- function(cohort_ids_interest, stem_name) {
  cohorts_interest <- cdm[["studyathon_final_cohorts"]] %>% 
    dplyr::filter(cohort_definition_id %in% cohort_ids_interest)
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(NA,-366)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(-365,-91)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(-90,-1)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(-365,-31)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(-30,-1)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(0,0)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(1,30)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(1,90)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(31,365)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(91,365)) %>%
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number, time"), order = "last", 
                                       window = c(366,NA)) %>% compute()
  
  # Something else apart from number and time?
  # Do all windows!!
  HU_summary <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::summarise(mean_number_ICU = mean(number_lc_pasc_hucohorts_1), 
                     mean_number_ventilation = mean(number_lc_pasc_hucohorts_2),
                     mean_number_tracheostomy = mean(number_lc_pasc_hucohorts_3), 
                     mean_number_ECMO = mean(number_lc_pasc_hucohorts_4),
                     mean_time_ICU = mean(time_lc_pasc_hucohorts_1),
                     mean_time_ventilation = mean(time_lc_pasc_hucohorts_2),
                     mean_time_tracheostomy = mean(time_lc_pasc_hucohorts_3),
                     mean_time_ECMO = mean(time_lc_pasc_hucohorts_4),
                     var_number_ICU = var(number_lc_pasc_hucohorts_1), 
                     var_number_ventilation = var(number_lc_pasc_hucohorts_2),
                     var_number_tracheostomy = var(number_lc_pasc_hucohorts_3), 
                     var_number_ECMO = var(number_lc_pasc_hucohorts_4),
                     var_time_ICU = var(time_lc_pasc_hucohorts_1),
                     var_time_ventilation = var(time_lc_pasc_hucohorts_2),
                     var_time_tracheostomy = var(time_lc_pasc_hucohorts_3),
                     var_time_ECMO = var(time_lc_pasc_hucohorts_4),
                     .groups = 'drop')
  
  write.csv(
    HU_summary,
    file = here::here(output_lsc, paste0("Healthcare_Utilisation_",stem_name,".csv")),
    row.names = FALSE
  )
}

do_tp <- function(cohort_base_id, stem_name_strata) {
    dataSettings <- TreatmentPatterns::createDataSettings(
    OMOP_CDM = FALSE, 
    cohortLocation = here("4_Characterisation","TreatmentPatterns_cohorts",
                          paste0("input_cohorts_",stem_name_strata,".csv"))
  )
  
  cohortSettings <- TreatmentPatterns::createCohortSettings(
    cohortsToCreate_location = here("4_Characterisation","TreatmentPatterns_cohorts",
                                    paste0("cohorts_to_create",cohort_base_id,".csv")),
    cohortsFolder = here("4_Characterisation","TreatmentPatterns_cohorts")
  )
  pathwaySettings <- TreatmentPatterns::createPathwaySettings(
    pathwaySettings_location = here("4_Characterisation","TreatmentPatterns_cohorts",
                                    paste0("pathway_settings",cohort_base_id,".csv"))
  )
  saveSettings <- TreatmentPatterns::createSaveSettings(databaseName = db.name,
                                                        rootFolder = here(),
                                                        outputFolder = here(output_tp))
  
  TreatmentPatterns::executeTreatmentPatterns(
    dataSettings = dataSettings, cohortSettings = cohortSettings,
    pathwaySettings = pathwaySettings, saveSettings = saveSettings)
  
  #dTreatmentPatterns::launchResultsExplorer()
  
}

###########################################################
# Copied from TxPath
computeTreatmentPathways <- function(cdm,
                                     targetCohortTable,
                                     eventCohortTable,
                                     periodPriorToIndex = 0,
                                     minEraDuration = 0,
                                     eraCollapseSize = 1,
                                     verbose = TRUE,
                                     maxIterations = 1000) {
  
  # preprocess
  event_cohorts_filtered <- cdm[[eventCohortTable]] %>%
    dplyr::rename(event_id = .data$cohort_definition_id,
                  event_start_date = .data$cohort_start_date,
                  event_end_date = .data$cohort_end_date) %>%
    dplyr::inner_join(cdm[[targetCohortTable]], by = "subject_id") %>%
    dplyr::mutate(lookback_date = !!CDMConnector::dateadd("cohort_start_date", periodPriorToIndex)) %>%
    dplyr::filter(.data$event_start_date >= .data$lookback_date) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$lookback_date,
                  .data$event_id,
                  .data$event_start_date,
                  .data$event_end_date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cohort_definition_id,
                   .data$subject_id,
                   .data$cohort_start_date,
                   .data$cohort_end_date) %>%
    CDMConnector::computeQuery()
  
  # add duration, filter on minEraDuration,
  # add gap time and collapse if gap < eraCollapseSize
  trace <- event_cohorts_filtered %>%
    dplyr::mutate(duration = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
    dplyr::filter(.data$duration >= .env$minEraDuration) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$event_id) %>%
    dbplyr::window_order(.data$event_start_date, .data$event_start_date) %>%
    dplyr::mutate(lag_end_date = dplyr::lag(.data$event_end_date)) %>%
    dplyr::mutate(gap_days = !!CDMConnector::datediff("lag_end_date", "event_start_date")) %>%
    dplyr::mutate(new_event = dplyr::if_else(is.na(.data$gap_days) || .data$gap_days > .env$eraCollapseSize, 1L, 0L)) %>%
    dplyr::mutate(event_counter = cumsum(.data$new_event)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$event_id, .data$event_counter) %>%
    dplyr::mutate(new_event_start_date = min(.data$event_start_date, na.rm = TRUE),
                  new_event_end_date = max(.data$event_end_date, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # TODO give user option to inspect the trace
  preprocessed <- trace %>%
    dplyr::mutate(event_id = as.character(as.integer(.data$event_id))) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$event_id,
                  event_start_date = .data$new_event_start_date,
                  event_end_date = .data$new_event_end_date) %>%
    dplyr::distinct() %>%
    CDMConnector::computeQuery()
  
  
  # iteratively collapse ----------
  x <- preprocessed
  for (i in seq_len(maxIterations)) {
    currentRowcount <- dplyr::tally(x) %>% dplyr::pull(.data$n)
    if (verbose) {
      print(glue::glue("Iteration {i} rowcount: {currentRowcount}"))
    }
    x <- collapseEras(x, minEraDuration = minEraDuration)
    newRowcount <- dplyr::tally(x) %>% dplyr::pull(.data$n)
    if (currentRowcount == newRowcount) {
      # TODO parameterize output table name?
      cdm[["treatment_patterns"]] <- x
      return(cdm)
    }
  }
  rlang::abort("Error with collapse function. Set verbose to TRUE or increase maxIterations.")
}

# Internal function to collapse eras
collapseEras <- function(preprocessed, minEraDuration = 0) {
  
  # TODO add persistent table option
  
  # identify the first pair of records that overlap for each person. Flag them with filter_flag.
  qry <- preprocessed %>%
    # mutate(event_id = as.character(as.integer(event_id))) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::mutate(lag_event_end_date = dplyr::lag(.data$event_end_date,
                                                  order_by = c(.data$event_start_date, .data$event_end_date))) %>%
    dplyr::mutate(overlap_flag = dplyr::case_when(.data$event_start_date <= .data$lag_event_end_date ~ 1L, TRUE ~ 0L)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id, .data$overlap_flag) %>%
    dbplyr::window_order(.data$cohort_definition_id,
                         .data$subject_id,
                         .data$event_start_date,
                         .data$event_end_date) %>%
    dplyr::mutate(row_flag = dplyr::if_else((.data$overlap_flag * dplyr::row_number()) == 1L, 1L, 0L)) %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    dplyr::mutate(filter_flag = dplyr::case_when(
      .data$row_flag == 1L ~ 1L,
      dplyr::lead(.data$row_flag, order_by = c(.data$event_start_date, .data$event_end_date)) == 1L ~ 1L,
      TRUE ~ 0L)) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$subject_id, .data$event_start_date, .data$event_end_date) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  .data$event_id,
                  .data$event_start_date,
                  .data$event_end_date,
                  .data$filter_flag) %>%
    dplyr::ungroup() %>%
    CDMConnector::computeQuery()
  
  # quality check
  check <- qry %>%
    dplyr::filter(.data$filter_flag == 1L) %>%
    dplyr::count(.data$cohort_definition_id, .data$subject_id, name = "n_rows_per_person") %>%
    dplyr::count(.data$n_rows_per_person, name = "n_persons") %>%
    dplyr::collect()
  
  if (nrow(check) == 0) {
    # no overlaps to process
    return(preprocessed)
  } else if (!all(check$n_rows_per_person == 2)) {
    print(check)
    rlang::abort("There should be two rows per person in the collapse step! Error with era collapse algorithm!")
  }
  
  # TODO add combinationWindow
  # combinationWindow should be greater than min era length
  # stopifnot(combinationWindow >= minEraDuration)
  
  combos <- qry %>%
    dplyr::filter(.data$filter_flag == 1) %>%
    dplyr::select(-"filter_flag") %>%
    dplyr::group_by(.data$cohort_definition_id, .data$subject_id) %>%
    # collapse to one row per person
    dplyr::mutate(
      event_id_a = .data$event_id,
      event_id_b = dplyr::lead(.data$event_id,
                               order_by = c(.data$event_start_date, .data$event_end_date)),
      start_a = .data$event_start_date, end_a = .data$event_end_date,
      start_overlap = max(.data$event_start_date, na.rm = TRUE),
      end_overlap = min(.data$event_end_date, na.rm = TRUE),
      start_b = dplyr::lead(.data$event_start_date,
                            order_by = c(.data$event_start_date, .data$event_end_date)),
      end_b = dplyr::lead(.data$event_end_date,
                          order_by = c(.data$event_start_date, .data$event_end_date))) %>%
    dplyr::filter(!is.na(.data$event_id_b)) %>%
    dplyr::select(.data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  dplyr::matches("_(a|b|overlap)$")) %>%
    dplyr::mutate(overlap = !!CDMConnector::datediff("start_overlap", "end_overlap"),
                  event_id_overlap = paste(.data$event_id_a, .data$event_id_b, sep = "-")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(start_overlap_minus1 = !!CDMConnector::dateadd("start_overlap", -1L),
                  end_overlap_plus1 = !!CDMConnector::dateadd("end_overlap", 1L),
                  end_a_plus1 = !!CDMConnector::dateadd("end_a", 1L),
                  start_b_minus1 = !!CDMConnector::dateadd("start_b", -1L)) %>%
    dplyr::mutate(
      new_start_a = dplyr::case_when(
        .data$start_a == .data$start_b ~ NA, # no era A before overlap
        TRUE ~ .data$start_a),
      new_end_a = dplyr::case_when(
        .data$start_a == .data$start_b ~ NA, # no era A before overlap
        .data$overlap < .env$minEraDuration ~ .data$start_b_minus1, # overlap is not long enough to be counted as a combo
        TRUE ~ .data$start_overlap_minus1 # there is an overlap after era A
      ),
      # need to adjust start and end dates of A and B
      # start and end of overlap are unchanged
      new_start_b = dplyr::case_when(
        .data$end_a == .data$end_b ~ NA, # no era B after overlap
        .data$overlap < .env$minEraDuration ~ .data$start_b, # overlap is not long enough to be a combo
        TRUE ~ .data$end_overlap_plus1
      ),
      new_end_b = dplyr::case_when(
        .data$end_a == .data$end_b ~ NA, # no era B after overlap
        .data$end_b > .data$end_overlap ~ .data$end_b,
        .data$end_a > .data$end_overlap ~ .data$end_a, # TODO in this case do we need to adjust the event_b_id?
        TRUE ~ NA
      )
    ) %>%
    dplyr::arrange(.data$cohort_definition_id, .data$subject_id) %>%
    CDMConnector::computeQuery()
  
  # combos should no longer contain overlaps
  
  output <- list(
    dplyr::filter(qry, .data$filter_flag == 0L) %>% dplyr::select(-"filter_flag"),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_a,
                  event_start_date = .data$new_start_a,
                  event_end_date = .data$new_end_a) %>%
      dplyr::mutate(difftime = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
      dplyr::filter(difftime >= .env$minEraDuration) %>% dplyr::select(-difftime),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_b,
                  event_start_date = .data$new_start_b,
                  event_end_date = .data$new_end_b) %>%
      dplyr::mutate(difftime = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
      dplyr::filter(difftime >= .env$minEraDuration) %>% dplyr::select(-difftime),
    dplyr::select(combos,
                  .data$cohort_definition_id,
                  .data$subject_id,
                  .data$cohort_start_date,
                  .data$cohort_end_date,
                  event_id = .data$event_id_overlap,
                  event_start_date = .data$start_overlap,
                  event_end_date = .data$end_overlap) %>%
      dplyr::mutate(difftime = !!CDMConnector::datediff("event_start_date", "event_end_date")) %>%
      dplyr::filter(difftime >= .env$minEraDuration) %>% dplyr::select(-difftime)
  ) %>%
    purrr::reduce(dplyr::union) %>%
    dplyr::filter(!is.na(.data$event_start_date) && !is.na(.data$event_end_date)) %>%
    dplyr::filter(!!CDMConnector::datediff("event_start_date", "event_end_date") >= .env$minEraDuration) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$cohort_definition_id,
                   .data$subject_id,
                   .data$event_start_date,
                   .data$event_end_date) %>%
    CDMConnector::computeQuery()
  
  return(output)
}


patternAttrition <- function(th,
                             minNumPatterns) {
  tp <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name)
  #get number of individuals
  numPersons <- nrow(tp)
  
  #get number of patterns
  tp <- tp %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n")
  
  
  numPathways <- nrow(tp)
  
  tp <- tp %>%
    dplyr::filter(n >= minNumPatterns)
  
  numPersons2 <- sum(tp$n)
  numPathways2 <- nrow(tp)
  
  df <- tibble::tibble(
    'StartingNumberOfPersons' = numPersons,
    'StartingNumberOfPathways' = numPathways,
    'minPatternCount' = minNumPatterns,
    'FilteredNumberOfPersons' = numPersons2,
    'FilteredNumberOfPathways' = numPathways2
  )
  
  return(df)
  
}

prepSankey <- function(treatment_pathways) {
  
  links <- treatment_pathways %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = c(-row, -n),
                        names_to = 'column', values_to = 'source') %>%
    dplyr::mutate(column = match(column, names(treatment_pathways))) %>%
    tidyr::drop_na(source) %>%
    dplyr::mutate(source = paste0(source, '__', column)) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(target = dplyr::lead(source, order_by = column)) %>%
    tidyr::drop_na(target, source) %>%
    dplyr::group_by(source, target) %>%
    dplyr::summarise(value = sum(n), .groups = 'drop') %>%
    dplyr::arrange(desc(value))
  
  nodes <- data.frame(name = unique(c(links$source, links$target)))
  nodes <- data.table::data.table(nodes)
  links <- data.table::data.table(links)
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  nodes$name <- sub('__[0-9]+$', '', nodes$name)
  links$type <- sub(' .*', '',
                    as.data.frame(nodes)[links$source + 1, 'name'])
  data.table::setkey(links, type)
  data.table::setorder(links, cols = - "value")
  
  res <- list(
    'links' = links,
    'nodes' = nodes
  )
  
  return(res)
  
}


findPatterns <- function(th, minNumPatterns = 30) {
  
  #get attrition
  attrition <- patternAttrition(th, minNumPatterns = minNumPatterns)
  
  # get patterns
  treatment_pathways <- th %>%
    tidyr::pivot_wider(id_cols = person_id,
                       names_from = event_seq,
                       names_prefix = "event_cohort_name",
                       values_from = event_cohort_name) %>%
    dplyr::count(dplyr::across(tidyselect::starts_with("event_cohort_name"))) %>%
    dplyr::mutate(End = "end", .before = "n") %>%
    dplyr::filter(n >= minNumPatterns)
  
  #prep sankey
  sankeyDat <- prepSankey(treatment_pathways)
  
  #clean patterns
  clean <- treatment_pathways %>%
    dplyr::select(-End) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::mutate(seq = paste(event_cohort_name1, event_cohort_name2, event_cohort_name3)) %>%
    dplyr::mutate(seq = gsub(" NA", "", seq),
                  seq = gsub(" ", " -> ", seq)) %>%
    dplyr::select(seq, n)
  
  
  ll <- structure(
    list(
      'sankey' = sankeyDat,
      'treatmentPatterns' = clean,
      'attrition' = attrition
    ), class = "treatmentPatterns"
  )
  
  return(ll)
}


plot_patterns <- function(treatmentPatterns) {
  
  links <- treatmentPatterns$sankey$links
  nodes <- treatmentPatterns$sankey$nodes
  
  label <- unique(links$type)
  label2 <- paste0("'", paste(label, collapse = "','"), "',", "'end'")
  
  
  martin_colors <- unname(colorBlindness::paletteMartin)[-1]
  
  
  col <- martin_colors[seq_along(label)]
  col2 <- paste0("'", paste(col, collapse = "','"), "',", "'#1B1919FF'")
  
  
  
  myCol <- glue::glue('d3.scaleOrdinal() .domain([{label2}]) .range([{col2}])')
  
  #plot sankeyNetwork
  sankey <- networkD3::sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    fontSize = 11,
    colourScale = myCol
  )
  return(sankey)
  
  
}

