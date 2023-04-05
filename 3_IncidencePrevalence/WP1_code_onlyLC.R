# WP1: Incidence and Prevalence

# no outcome washout
# no complete dataset or full contribution
# repeated events no
# look back 0

if((doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
}

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
if (!file.exists(output_ip)){
  dir.create(output_ip, recursive = TRUE)}

# ----------------------------------------------------------------
# 1a: PASC, LC and MC on base cohorts
info(logger, '-- Calculating incidence and prevalence for outcomes in base cohorts')

calculate_IP <- function(base_id, outcome_id, tableBase, tableOutcome, stem) {
  message("Calculating IP for base ", base_id," and outcome ",outcome_id)
  
  base_name <- ifelse(base_id == 1, "Inf",
                      ifelse(base_id == 2, "Reinf", 
                             ifelse(base_id == 3, "Neg", 
                                    ifelse(base_id == 4, "Flu", NA))))
  
  date_to_consider <- if_else(base_id %in% c(1,2,3), as.Date("2020-09-01"), as.Date("2017-01-01"))
  date_to_end <- if_else(base_id %in% c(1,2,3), as.Date(latest_data_availability), as.Date("2019-12-31"))
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = base_id,
    startDate = date_to_consider,
    endDate = date_to_end,
    sex = c("Male", "Female", "Both")
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"),
          repeatedEvents = FALSE, completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0(base_name,"_",stem,"_",i,"_AllandSex"),
          outputFolder=output_ip) 
      }
    }
  }
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = base_id,
    startDate = date_to_consider,
    endDate = date_to_end,
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"),repeatedEvents = FALSE,
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0(base_name,"_",stem,"_",i,"_Age"),
          outputFolder=output_ip) 
      }
    }
  }
  
  vacc_id <- ifelse(base_id == 1, 23,
                    ifelse(base_id == 2, 25, NA))
  nonvacc_id <- ifelse(base_id == 1, 24, 
                       ifelse(base_id == 2, 26, NA))
  
  message("- Vaccination strata")
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = vacc_id,
    startDate = date_to_consider,
    endDate = date_to_end,
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"),repeatedEvents = FALSE,
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0(base_name,"_",stem,"_",i,"_Vacc"),
          outputFolder=output_ip) 
      }
    }
  }
  
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    strataTable = tableBase,
    strataCohortId = nonvacc_id,
    startDate = date_to_consider,
    endDate = date_to_end,
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome",
          interval = c("years","months"), repeatedEvents = FALSE, 
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0(base_name,"_",stem,"_",i,"_NonVacc"),
          outputFolder=output_ip) 
      }
    }
  }
}

base_cohorts_id <- c(1:2)
outcome_cohorts_id <- c(1:27) # only outcomes
for(i in base_cohorts_id) {
  calculate_IP(i, outcome_cohorts_id, BaseCohortsName, LongCovidCohortsName, "LC")
}

# ----------------------------------------------------------------
# 1b: PASC, LC, MC, base cohorts on source population

info(logger, '-- Calculating incidence and prevalence for outcomes and base cohorts in source population')

calculate_IP_allpop <- function(outcome_id, date_to_consider, date_to_end, tableOutcome, stem) {
  
  message("Calculating IP for outcome ",outcome_id)
  
  message("- No strata and sex strata")
  # No strata and sex strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = date_to_consider,
    endDate = date_to_end,
    sex = c("Male", "Female", "Both")
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"), repeatedEvents = FALSE, 
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0("Allpop_",stem,"_",i,"_AllandSex"), 
          outputFolder=output_ip) 
      }
    }
  }
  
  message("- Age strata")
  # Age strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm =  cdm,
    startDate = date_to_consider,
    endDate = date_to_end,
    ageGroup = list(c(0,6),c(7,11),c(12,18),c(19,40),c(41,64),c(65,120))
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"),repeatedEvents = FALSE, 
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0("Allpop_",stem,"_",i,"_Age"), 
          outputFolder=output_ip) 
      }
    }
  }
  message("- Vaccination strata")
  # Vaccination strata
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = VaccCohortsName,
    strataCohortId = 1,
    startDate = date_to_consider,
    endDate = date_to_end,
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome", 
          interval = c("years","months"),repeatedEvents = FALSE, 
          completeDatabaseIntervals = FALSE, minCellCount = 5) 
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0("Allpop_",stem,"_",i,"_Vacc"), 
          outputFolder=output_ip) 
      }
    }
  }
  cdm$denominator <- IncidencePrevalence::generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = VaccCohortsName,
    strataCohortId = 2,
    startDate = date_to_consider,
    endDate = date_to_end,
  )
  if(cdm$denominator %>% tally() %>% pull() != 0) {
    for(i in outcome_id) {
      cdm$outcome <- cdm[[tableOutcome]] %>%
        dplyr::filter(.data$cohort_definition_id == i)
      if(cdm$outcome %>% tally() %>% pull() != 0) {
        inc <- IncidencePrevalence::estimateIncidence(
          cdm = cdm, denominatorTable = "denominator", outcomeTable = "outcome",
          interval = c("years","months"), repeatedEvents = FALSE, 
          completeDatabaseIntervals = FALSE, minCellCount = 5)
        
        study_results <- IncidencePrevalence::gatherIncidencePrevalenceResults(
          cdm=cdm, resultList=list(inc))
        IncidencePrevalence::exportIncidencePrevalenceResults(
          result=study_results, zipName=paste0("Allpop_",stem,"_",i,"_NonVacc"), 
          outputFolder=output_ip) 
      }
    }
  }
}

# base and outcome+base overlap.
calculate_IP_allpop(c(1:2), as.Date("2020-01-01"), as.Date(latest_data_availability), BaseCohortsName, "base")
calculate_IP_allpop(c(1:4), as.Date("2020-01-01"), as.Date(latest_data_availability), OverlapCohortsCName, "overlap_any")
calculate_IP_allpop(c(1:50), as.Date("2020-01-01"), as.Date(latest_data_availability), OverlapCohortsIPName, "overlap")

