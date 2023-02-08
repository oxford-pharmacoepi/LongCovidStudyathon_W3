# WP1: Incidence and Prevalence

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema, cohortTables = c(cohort_table_name,"studyathon_final_cohorts"))

# Output folder for WP1
output_ip <- here::here(output.folder,"IP")
  if (!file.exists(output_ip)){
    dir.create(output_ip, recursive = TRUE)}

# ----------------------------------------------------------------
# 1a: PASC, LC and MC on base cohorts
# calendar_strata <- c("2020-09-01", "2021-12-01", "2022-12-01") # not needed here as we calculate IP monthly, etc.
# stratification all together, or all different?

calculate_IP <- function(base_id,outcome_id) {
  message("Calculating IP for base ", base_id," and outcome ",outcome_id)
  
  base_name <- ifelse(base_id == 1, "Inf", ifelse(base_id == 2, "Reinf", ifelse(base_id == 3, "Neg", ifelse(base_id == 4, "Flu", NA))))
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = base_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    sex = c("Male", "Female", "Both")
  )
  message("- Denominator")
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  message("- Outcome")
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
    repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  message("- Incidence")
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
    timePoint = "start",minCellCount = 5)
  message("- Point prevalence")
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
    completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  message("- Period prevalence")
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  message("- Study results gathered")
  
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_id,"_AllandSex"), outputFolder=output_ip) 
  message("- Study results exported")
  
  
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = base_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    ageGroup = list(c(0,2),c(3,5),c(6,9),c(10,13),c(14,17),c(18,40),
                    c(41,64),c(65,120))
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_id,"_Age"), outputFolder=output_ip) 
  
  vacc_id <- ifelse(base_id == 1, 116, ifelse(base_id == 2, 118, ifelse(base_id == 3, 120, ifelse(base_id == 4, 122, NA))))
  nonvacc_id <- ifelse(base_id == 1, 117, ifelse(base_id == 2, 119, ifelse(base_id == 3, 121, ifelse(base_id == 4, 123, NA))))
  
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = vacc_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_name,"_Vacc"), outputFolder=output_ip) 
  
  
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = nonvacc_id,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_name,"NonVacc"), outputFolder=output_ip) 
  
}

base_cohorts_id <- c(1:4)
outcome_cohorts_id <- c(5:59,100:102,104:107,112:115) # both only outcome and outcome+base overlap
IP_1a <- Map(calculate_IP,base_cohorts_id,outcome_cohorts_id)


# ----------------------------------------------------------------
# 1b: PASC, LC, MC, base cohorts on source population
calculate_IP_allpop <- function(outcome_id) {
  
  message("Calculating IP for outcome ",outcome_id)
  
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    sex = c("Male", "Female", "Both")
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"_AllandSex"), outputFolder=output_ip) 
  
  
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    ageGroup = list(c(0,2),c(3,5),c(6,9),c(10,13),c(14,17),c(18,40),
                    c(41,64),c(65,120))
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_id,"_Age"), outputFolder=output_ip) 
  
  
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = 103,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"_Vacc"), outputFolder=output_ip) 
  
  
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "studyathon_final_cohorts",
    strataCohortId = 104,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- IncidencePrevalence::estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- IncidencePrevalence::estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- IncidencePrevalence::estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  IncidencePrevalence::exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"NonVacc"), outputFolder=output_ip) 
  
}

all_cohorts_id <- c(1:4,5:59,100:102,104:107,112:115) # base, only outcome and outcome+base overlap
IP_1b <- lapply(FUN = calculate_IP,all_cohorts_id)


