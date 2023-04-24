# Instantiate initial cohorts

info(logger, "- getting initial cohort definitions")

if(instantiate_diff) {
  Initial_cohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts","Jsons_diff")) %>%
    dplyr::mutate(cohort_name = substr(cohort_name, 5, nchar(cohort_name)))
} else {
  Initial_cohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts","Jsons")) %>%
    dplyr::mutate(cohort_name = substr(cohort_name, 5, nchar(cohort_name)))
}

info(logger, "- getting initial cohorts")

cdm <- CDMConnector::generateCohortSet(cdm, Initial_cohorts,
                                       name = InitialCohortsName,
                                       overwrite = TRUE)

info(logger, "- got initial cohorts")