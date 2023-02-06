# Instantiate initial cohorts

info(logger, "- getting initial cohort definitions")

Initial_cohorts <- CDMConnector::readCohortSet(here::here("1_InitialCohorts","Jsons")) %>%
  dplyr::mutate(cohortName = substr(cohortName, 5, nchar(cohortName)))

info(logger, "- getting initial cohorts")

cdm <- CDMConnector::generateCohortSet(cdm, Initial_cohorts,
                                       cohortTableName = cohort_table_name,
                                       overwrite = TRUE)

info(logger, "- got initial cohorts")