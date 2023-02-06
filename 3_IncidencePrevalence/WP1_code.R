# WP1: Incidence and Prevalence

# Output folder for WP1
output_ip <- here::here(output.folder,"IP")
  if (!file.exists(output_ip)){
    dir.create(output_ip, recursive = TRUE)}

# QUESTION What is the strata for calendar period (relating to VOC)?

# ----------------------------------------------------------------
# 1a: PASC, LC and MC on base cohorts
# calendar_strata <- c("2020-09-01", "2021-12-01", "2022-12-01") # not needed here as we calculate IP monthly, etc.
# stratification all together, or all different?

calculate_IP <- function(base_id,outcome_id) {
  
  base_name <- ifelse(base_id == 1, "Inf", ifelse(base_id == 2, "Reinf", ifelse(base_id == 3, "Neg", ifelse(base_id == 4, "Flu", NA))))
  
  # No strata and sex strata
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    sex = c("Male", "Female", "Both")
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
    repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
    timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
    completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_name,"_AllandSex"), outputFolder=output_ip) 
  
  
  # Age strata
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    ageGroup = list(c(0,2),c(3,5),c(6,9),c(10,13),c(14,17),c(18,40),
                    c(41,64),c(65,120))
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_id,"_Age"), outputFolder=output_ip) 
  
  
  # Vaccination strata
  vaccinated_cohort <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id) %>% 
    left_join(cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == 103) %>% select(person_id, "vacc_date" = "cohort_start_date"), by = "person_id") %>%
    filter(cohort_start_date > vacc_date) %>% filter(!(is.na(vacc_date))) %>% select(person_id,cohort_start_date,cohort_end_date)
  
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id) %>% 
      right_join(vaccinated_cohort, by="person_id"),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_name,"_Vacc"), outputFolder=output_ip) 
  
  
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =   cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == base_id) %>% 
      anti_join(vaccinated_cohort, by="person_id"),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0(base_name,"_",outcome_name,"NonVacc"), outputFolder=output_ip) 
  
}

base_cohorts_id <- c(1:4)
outcome_cohorts_id <- c(5:59,100:102,104:107,112:115) # both only outcome and outcome+base overlap
IP_1a <- lapply(calculate_IP,base_cohorts_id,outcome_cohorts_id)


# ----------------------------------------------------------------
# 1b: PASC, LC, MC, base cohorts on source population
calculate_IP_allpop <- function(outcome_id) {
  
  # No strata and sex strata
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    sex = c("Male", "Female", "Both")
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"_AllandSex"), outputFolder=output_ip) 
  
  
  # Age strata
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01"), # latest data availability
    ageGroup = list(c(0,2),c(3,5),c(6,9),c(10,13),c(14,17),c(18,40),
                    c(41,64),c(65,120))
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_id,"_Age"), outputFolder=output_ip) 
  
  
  # Vaccination strata
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =  cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == 103) %>% 
      right_join(vaccinated_cohort, by="person_id"),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"_Vacc"), outputFolder=output_ip) 
  
  
  cdm$denominator <- generateDenominatorCohortSet(
    cdm =   cdm %>% 
      anti_join(cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == 103), by="person_id"),
    startDate = as.Date("2020-09-01"),
    endDate = as.Date("2022-12-01") # latest data availability
  )
  cdm$outcome <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == outcome_id)
  
  inc <- estimateIncidence(cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                           repeatedEvents = TRUE, outcomeWashout = 180, completeDatabaseIntervals = TRUE, minCellCount = 5)
  
  prev_point <- estimatePointPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome", interval = c("years","months"),
                                        timePoint = "start",minCellCount = 5)
  
  prev_period <- estimatePeriodPrevalence(cdm = cdm,denominatorTable = "denominator", outcomeTable = "outcome",interval = c("years","months"),
                                          completeDatabaseIntervals = TRUE, fullContribution = TRUE,minCellCount = 5)
  
  study_results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList=list(inc, prev_point, prev_period), databaseName = db.name)
  exportIncidencePrevalenceResults(result=study_results, zipName=paste0("Allpop_",outcome_name,"NonVacc"), outputFolder=output_ip) 
  
}

all_cohorts_id <- c(1:4,5:59,100:102,104:107,112:115) # base, only outcome and outcome+base overlap
IP_1b <- lapply(calculate_IP,all_cohorts_id)



