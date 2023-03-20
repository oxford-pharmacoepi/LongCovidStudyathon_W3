do_vaccination_characterisation <- function(cohort_ids_interest, stem_name, tableName) {
  cohorts_interest <- cdm[[tableName]] %>% 
    dplyr::filter(.data$cohort_definition_id %in% cohort_ids_interest) %>%
    compute()
  cohorts_interest <- cohorts_interest %>% 
    addOverlap(cdm, VaccCohortsName, 3, "first_dose") %>% 
    addOverlap(cdm, VaccCohortsName, 4, "second_dose") %>% 
    addOverlap(cdm, VaccCohortsName, 5, "third_dose") %>% 
    addEvent(cdm, VaccCohortsName, 1, c(Na,0), "last_dose", order = "last") %>% 
    compute()
  
  cohorts_interest <- cohorts_interest %>% 
    dplyr::mutate(dose = first_dose + second_dose + third_dose) %>%
    dplyr::mutate(last_dose_days = CDMConnector::datediff("last_dose","cohort_start_date")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date,dose,last_dose_days) %>% 
    compute()
  
  vacc_counts <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id, dose) %>% tally() %>%
    compute()
  vacc_lastdose <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::summarise(mean_lastdose = mean(last_dose_days), 
                     var_lastdose = var(last_dose_days),
                     .groups = 'drop') %>%
    compute()
  
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

do_lsc <- function(cohort_ids_interest, stem_name, tableName) {
  # Check if charac is list or just table, because then no [[x]]
  charac <- LargeScaleCharacteristics::getLargeScaleCharacteristics(
    cdm, targetCohortName = tableName, 
    targetCohortId = cohort_ids_interest,
    tablesToCharacterize = "condition_occurrence",
    temporalWindows = list(
      c(NA, -366), c(-365, -181),
      c(-180, -91), c(-90, -31), c(-30, -1),
      c(0, 0)
    ))
  
 # Like this maybe? 
  write.csv(
    charac,
    file = here::here(output_lsc, paste0("Characterisation_",stem_name,".csv")),
    row.names = FALSE
  )
  
#  for(i in 1:length(charac)) {
#    write.csv(
#      charac[[i]],
#      file = here::here(output_lsc, paste0("Characterisation_",stem_name,"_",names(charac)[i],".csv")),
#      row.names = FALSE
#    )
#  }
}

do_du <- function(cohort_ids_interest, stem_name, tableName) {
  charac <- LargeScaleCharacteristics::getLargeScaleCharacteristics(
    cdm, targetCohortName = tableName, 
    targetCohortId = cohort_ids_interest,
    tablesToCharacterize = "drug_exposure",
    temporalWindows = list(
      c(-365, -181), c(-180, -91), c(-90, -31), c(-30, -1),
      c(0, 0), c(1, 30), c(31, 90), c(91, 180), c(181, 365)
    ))
  
  # Like this maybe? 
  write.csv(
    charac,
    file = here::here(output_lsc, paste0("DrugUtilisation_",stem_name,".csv")),
    row.names = FALSE
  )
  
#  for(i in 1:length(charac)) {
#    write.csv(
#      charac[[i]],
#      file = here::here(output_du, paste0("DrugUtilisation_",stem_name,"_",names(charac)[i],".csv")),
#      row.names = FALSE
#    )
#  }
  
}

do_hu <- function(cohort_ids_interest, stem_name, tableName) {
  cohorts_interest <- cdm[[tableName]] %>% 
    dplyr::filter(.data$cohort_definition_id %in% cohort_ids_interest) %>%
    compute()
  cohorts_interest <- cohorts_interest %>% 
    addEvent(cdm, HUCohortsName, 1, c(-365,-1), "last_icu", order = "last") %>%
    addEvent(cdm, HUCohortsName, 2, c(-365,-1), "last_vent", order = "last") %>%
    addEvent(cdm, HUCohortsName, 3, c(-365,-1), "last_trach", order = "last") %>%
    addEvent(cdm, HUCohortsName, 4, c(-365,-1), "last_ecmo", order = "last") %>%
    addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 1), c(-365,-1), "number_icu") %>%
    addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 2), c(-365,-1), "number_vent") %>%
    addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 3), c(-365,-1), "number_trach") %>%
    addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 4), c(-365,-1), "number_ecmo") %>%
    addNumberVisit(cdm, 9202, c(-365,-1)) %>% 
    addEvent(cdm, "visit_occurrence", 9202, c(-365,-1), "last_GP", eventDate = "visit_start_date", order = "last") %>%
    compute()
  
  HU_summary <- cohorts_interest %>%
    dplyr::rename("number_GP" = "number_visit") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::mutate(time_GP = !!CDMConnector::datediff("last_GP", "cohort_start_date")) %>%
    dplyr::mutate(time_icu = !!CDMConnector::datediff("last_icu", "cohort_start_date")) %>%
    dplyr::mutate(time_vent = !!CDMConnector::datediff("last_vent", "cohort_start_date")) %>%
    dplyr::mutate(time_trach = !!CDMConnector::datediff("last_trach", "cohort_start_date")) %>%
    dplyr::mutate(time_ecmo = !!CDMConnector::datediff("last_ecmo", "cohort_start_date")) %>%
    dplyr::select(dplyr::starts_with(c("number","time"))) %>%
    dplyr::summarise(across(everything(), list(mean = mean, var = var, sum = sum))) %>%
    dplyr::arrange(cohort_definition_id) %>%
    compute()
  
  write.csv(
    HU_summary,
    file = here::here(output_lsc, paste0("Healthcare_Utilisation_noHosp_",stem_name,".csv")),
    row.names = FALSE
  )
  
  # Gte hospitalisation codes
  ip.codes <- c(9201, 262)
  # add all descendents
  ip.codes.w.desc <- cdm$concept_ancestor %>%
    filter(ancestor_concept_id  %in% ip.codes ) %>% 
    collect() %>% 
    select(descendant_concept_id) %>% 
    distinct() %>% 
    pull()
  
  cohorts_interest <- cohorts_interest %>% dplyr::select(-dplyr::contains(c("time", "number", "last"))) %>%
    addNumberVisit(cdm, ip.codes.w.desc, c(-365,-1)) %>% 
    addEvent(cdm, "visit_occurrence", ip.codes.w.desc, c(-365,-1), "last_hosp", eventDate = "visit_start_date", order = "last") %>%
    compute()

  if(all(c("number_visit", "last_hosp") %in% colnames(cohorts_interest))) {
    HU_hosp_summary <- cohorts_interest %>%
      dplyr::rename("number_hosp" = "number_visit") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(cohort_definition_id) %>%
      mutate(time_visit = !!CDMConnector::datediff("last_hosp", "cohort_start_date")) %>%
      dplyr::select(dplyr::starts_with(c("number","time"))) %>%
      dplyr::summarise(across(everything(), list(mean = mean, var = var, sum = sum))) %>%
      dplyr::arrange(cohort_definition_id) %>%
      compute()
    # K is it a cbind?
    HU_summary_final <- cbind(HU_summary,HU_hosp_summary)
  } else {
    HU_summary_final <- HU_summary
  }
  
  write.csv(
    HU_summary_final,
    file = here::here(output_lsc, paste0("Healthcare_Utilisation_",stem_name,".csv")),
    row.names = FALSE
  )
  
  }

do_tp <- function(cohort_base_id, tp_ids, tableName) {
    dataSettings <- TreatmentPatterns::createDataSettings(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdm_database_schema,
    resultSchema = results_database_schema,
    cohortTable = tableName
  )
    targetname <- names_final_cohorts %>% 
      dplyr::filter(table_name == tableName) %>%
      dplyr::filter(cohort_definition_id == cohort_base_id) %>%
      dplyr::select(cohort_name) %>% pull()
    eventnames <- names_final_cohorts %>% 
      dplyr::filter(table_name == tableName) %>%
      dplyr::filter(cohort_definition_id %in% tp_ids) %>%
      dplyr::select(cohort_name) %>% pull()
      
  # Check if this works
    targetCohort <- data.frame(cohortName = targetname, cohortId = cohort_base_id)
    eventCohorts <- data.frame(cohortName = eventnames, cohortId = tp_ids)

    cohortscreate <- tibble::tibble(
      cohortId = c(cohort_base_id,tp_ids), 
      cohortName = c(targetname,eventnames), 
      cohortType = c("target",rep("event",32)))
    
    # Create settings first (the folder)
    write_csv(file = here(output_tp,"settings","cohorts_to_create.csv"),cohortscreate)
    
  cohortSettings <- TreatmentPatterns::createCohortSettings(
    targetCohorts = targetCohort,
    eventCohorts = eventCohorts
  )
  
  pathwaySettings <- TreatmentPatterns::createPathwaySettings(
    cohortSettings = cohortSettings,
    studyName = targetname
  )
  saveSettings <- TreatmentPatterns::createSaveSettings(databaseName = db.name,
                                                        rootFolder = here(),
                                                        outputFolder = here(output_tp))
  TreatmentPatterns::constructPathways(
    dataSettings = dataSettings,
    pathwaySettings = pathwaySettings,
    saveSettings = saveSettings)
  
  TreatmentPatterns::generateOutput(saveSettings = saveSettings)
}
