# WP1: Incidence and Prevalence

# no outcome washout
# no complete dataset or full contribution
# repeated events no
# look back 0

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts"))

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
  if (!file.exists(output_ip)){
    dir.create(output_ip, recursive = TRUE)}

# ----------------------------------------------------------------
# 1a: PASC, LC and MC on base cohorts
info(logger, '-- Calculating incidence and prevalence for outcomes in base cohorts')

calculate_IP <- function(base_id,outcome_id) {
  message("Calculating IP for base ", base_id," and outcome ",outcome_id)
  
  base_name <- ifelse(base_id == 1, "Inf",
                      ifelse(base_id == 2, "Reinf", 
                             ifelse(base_id == 3, "Neg", 
                                    ifelse(base_id == 4, "Flu", NA))))
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = base_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability),
    sex = c("Male", "Female", "Both")
  )
  for(i in outcome_id) {
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
    dplyr::filter(cohort_definition_id == i)

  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"),
    repeatedEvents = FALSE, completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)

  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0(base_name,"_",i,"_AllandSex"),
    outputFolder=output_ip) 
  }
  
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = base_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability),
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )

  for(i in outcome_id) {
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
    dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"),repeatedEvents = FALSE,
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0(base_name,"_",i,"_Age"),
    outputFolder=output_ip) 
  }
  
  vacc_id <- ifelse(base_id == 1, 117,
                    ifelse(base_id == 2, 119,
                           ifelse(base_id == 3, 121, 
                                  ifelse(base_id == 4, 123, NA))))
  nonvacc_id <- ifelse(base_id == 1, 118, 
                       ifelse(base_id == 2, 120, 
                              ifelse(base_id == 3, 122,
                                     ifelse(base_id == 4, 124, NA))))
  
  message("- Vaccination strata")
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = vacc_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability)
  )
  for(i in outcome_id) {
    cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"),repeatedEvents = FALSE,
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0(base_name,"_",i,"_Vacc"),
    outputFolder=output_ip) 
  }
  
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = nonvacc_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability) 
  )
 for(i in outcome_id) {
   cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
     dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome",
    interval = c("years","months"), repeatedEvents = FALSE, 
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0(base_name,"_",i,"_NonVacc"),
    outputFolder=output_ip) 
 }
}

base_cohorts_id <- c(1:4)
outcome_cohorts_id <- c(5:59,100:102) # only outcomes
IP_1a1 <- purrr::map2(1,outcome_cohorts_id,calculate_IP)
IP_1a2 <- purrr::map2(2,outcome_cohorts_id,calculate_IP)
IP_1a3 <- purrr::map2(3,outcome_cohorts_id,calculate_IP)
IP_1a4 <- purrr::map2(4,outcome_cohorts_id,calculate_IP)

# ----------------------------------------------------------------
# 1b: PASC, LC, MC, base cohorts on source population

info(logger, '-- Calculating incidence and prevalence for outcomes and base cohorts in source population')

calculate_IP_allpop <- function(outcome_id) {
  
  message("Calculating IP for outcome ",outcome_id)
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability),
    sex = c("Male", "Female", "Both")
  )
  for(i in outcome_id) {
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
    dplyr::filter(cohort_definition_id == i)
  
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"), repeatedEvents = FALSE, 
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0("Allpop_",i,"_AllandSex"), 
    outputFolder=output_ip) 
  }
  
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability),
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )
  
  for(i in outcome_id) {
    cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"),repeatedEvents = FALSE, 
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0("Allpop_",i,"_Age"), 
    outputFolder=output_ip) 
  }
  message("- Vaccination strata")
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = 103,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability) 
  )
  for(i in outcome_id) {
    cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
    interval = c("years","months"),repeatedEvents = FALSE, 
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0("Allpop_",i,"_Vacc"), 
    outputFolder=output_ip) 
  }
  
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = 104,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date(latest_data_availability)
  )
  for(i in outcome_id) {
    cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>%
      dplyr::filter(cohort_definition_id == i)
  inc <- IncidencePrevalence::estimateIncidence(
    cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome",
    interval = c("years","months"), repeatedEvents = FALSE, 
    completeDatabaseIntervals = FALSE, minCellCount = 5)

  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
    cdm=cdm, resultList=list(inc), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(
    result=study_results, zipName=paste0("Allpop_",i,"NonVacc"), 
    outputFolder=output_ip) 
  }
}

all_cohorts_id <- c(1:4,105:116,205:259,405:459,605:659,805:859) 
# base and outcome+base overlap.
IP_1b <- calculate_IP_allpop(all_cohorts_id)
