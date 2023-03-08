do_vaccination_characterisation <- function(cohort_ids_interest, stem_name) {
  cohorts_interest <- cdm[["studyathon_final_cohorts"]] %>% 
    dplyr::filter(cohort_definition_id %in% cohort_ids_interest)
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 350,
                             window = c(NA,0),
                             value = "binary") %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 351,
                             value = "binary",
                             window = c(NA,0)) %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 352,
                             window = c(NA,0),
                             value = "binary") %>% compute()
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, "studyathon_final_cohorts", 
                             cohortId = 103,
                             value = "date",
                             window = c(NA,0), order = "last") %>% compute()
  
  cohorts_interest <- cohorts_interest %>% 
    mutate(dose = binary_studyathon_final_cohorts_350 + binary_studyathon_final_cohorts_351 + binary_studyathon_final_cohorts_352) %>%
    mutate(last_dose_days = CDMConnector::datediff("date_studyathon_final_cohorts_103","cohort_start_date")) %>%
    dplyr::select(subject_id,cohort_definition_id,cohort_start_date,cohort_end_date,dose,last_dose_days) %>% 
    compute()
  
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
    targetCohortId = cohort_ids_interest,
    tablesToCharacterize = "condition_occurrence",
    temporalWindows = list(
      c(NA, -366), c(-365, -181),
      c(-180, -91), c(-90, -31), c(-30, -1),
      c(0, 0)
    ))
  
  for(i in 1:length(charac)) {
    write.csv(
      charac[[i]],
      file = here::here(output_lsc, paste0("Characterisation_",stem_name,"_",names(charac)[i],".csv")),
      row.names = FALSE
    )
  }
}

do_du <- function(cohort_ids_interest, stem_name) {
  charac <- LargeScaleCharacteristics::getLargeScaleCharacteristics(
    cdm, targetCohortName = "studyathon_final_cohorts", 
    targetCohortId = cohort_ids_interest,
    tablesToCharacterize = "drug_exposure",
    temporalWindows = list(
      c(-365, -181), c(-180, -91), c(-90, -31), c(-30, -1),
      c(0, 0), c(1, 30), c(31, 90), c(91, 180), c(181, 365)
    ))
  
  for(i in 1:length(charac)) {
    write.csv(
      charac[[i]],
      file = here::here(output_lsc, paste0("DrugUtilisation_",stem_name,"_",names(charac)[i],".csv")),
      row.names = FALSE
    )
  }
}

do_hu <- function(cohort_ids_interest, stem_name) {
  cohorts_interest <- cdm[["studyathon_final_cohorts"]] %>% 
    dplyr::filter(cohort_definition_id %in% cohort_ids_interest)
  cohorts_interest <- cohorts_interest %>% 
    CohortProfiles::addCohortIntersect(cdm, cohortTableName = "lc_pasc_hucohorts", 
                                       value = c("number", "time"), order = "last", 
                                       window = c(-365,-1)) %>%
    CohortProfiles::addTableIntersect(cdm, tableName = "visit_occurrence", 
                                      value = c("number", "time"), order = "last",
                                      window = c(-365, -1))
  
  HU_summary <- cohorts_interest %>% 
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::summarise(mean_number_ICU = mean(number_lc_pasc_hucohorts_1), 
                     mean_number_ventilation = mean(number_lc_pasc_hucohorts_2),
                     mean_number_tracheostomy = mean(number_lc_pasc_hucohorts_3), 
                     mean_number_ECMO = mean(number_lc_pasc_hucohorts_4),
                     mean_number_GP = mean("number_visit_occurrence_(-365,-1)"),
                     sum_ICU = sum(number_lc_pasc_hucohorts_1), 
                     sum_ventilation = sum(number_lc_pasc_hucohorts_2),
                     sum_tracheostomy = sum(number_lc_pasc_hucohorts_3), 
                     sum_ECMO = sum(number_lc_pasc_hucohorts_4),
                     sum_GP = sum("number_visit_occurrence_(-365,-1)"),
                     mean_time_ICU = mean(time_lc_pasc_hucohorts_1),
                     mean_time_ventilation = mean(time_lc_pasc_hucohorts_2),
                     mean_time_tracheostomy = mean(time_lc_pasc_hucohorts_3),
                     mean_time_ECMO = mean(time_lc_pasc_hucohorts_4),
                     mean_time_GP = mean("time_visit_occurrence_(-365,-1)"),
                     var_number_ICU = var(number_lc_pasc_hucohorts_1), 
                     var_number_ventilation = var(number_lc_pasc_hucohorts_2),
                     var_number_tracheostomy = var(number_lc_pasc_hucohorts_3), 
                     var_number_ECMO = var(number_lc_pasc_hucohorts_4),
                     var_number_GP = var("number_visit_occurrence_(-365,-1)"),
                     var_time_ICU = var(time_lc_pasc_hucohorts_1),
                     var_time_ventilation = var(time_lc_pasc_hucohorts_2),
                     var_time_tracheostomy = var(time_lc_pasc_hucohorts_3),
                     var_time_ECMO = var(time_lc_pasc_hucohorts_4),
                     var_time_GP = var("time_visit_occurrence_(-365,-1)"),
                     .groups = 'drop')
  
  
  # Add hospitalisation and sick leave when available too
  
  write.csv(
    HU_summary,
    file = here::here(output_lsc, paste0("Healthcare_Utilisation_",stem_name,".csv")),
    row.names = FALSE
  )
}

do_tp <- function(cohort_base_id) {
    dataSettings <- TreatmentPatterns::createDataSettings(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdm_database_schema,
    resultSchema = results_database_schema,
    cohortTable = "studyathon_final_cohorts"
  )
  
    # Change c(65:99) to the to-be ids of the drugs instantiated in the beginning and put into "studyathon_final_cohorts"
    targetCohort <- data.frame(cohortName = names_final_cohorts[cohort_base_id], cohortId = cohort_base_id)
    eventCohorts <- data.frame(cohortName = names_final_cohorts$cohortName[65:99], cohortId = c(65:99))

    cohortscreate <- tibble::tibble(
      cohortId = c(cohort_base_id,65:99), 
      cohortName = c(names_final_cohorts[cohort_base_id],names_final_cohorts$cohortName[65:99]), 
      cohortType = c("target",rep("event",21))) # Or whichever
    #K
    # Create settings first (the folder)
    write_csv(file = here(output_tp,"settings","cohorts_to_create.csv"),cohortscreate)
    
  cohortSettings <- TreatmentPatterns::createCohortSettings(
    targetCohorts = targetCohort,
    eventCohorts = eventCohorts
  )
  
  pathwaySettings <- TreatmentPatterns::createPathwaySettings(
    cohortSettings = cohortSettings,
    studyName = names_final_cohorts[cohort_base_id]
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
