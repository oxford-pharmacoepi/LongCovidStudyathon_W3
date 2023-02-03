# Instantiate initial cohorts
# Get no start date nor any other attrition, will do that myself here
# Ask Annika about PCR or diagnose in Covid
# Ask England associated care site id
# for 180d, 120d, in observation? what about death?

# New infection: 18 yo, 180d prior, no influenza 42d before, no covi 42d before
# Tested negative all: 18yo, 180d prior, no prior covid (ONLY here diagnosis), no influenza 42d before, no negative covid test 42d before
# PCR censoring (DIAGNOSE here yes)
# All PASC events have 90d washout
# Not LC symptoms

info(logger, "- getting initial cohort definitions")

Initial_cohorts <- CDMConnector::readCohortSet(here::here("1_InitialCohorts","Jsons")) %>%
  dplyr::mutate(cohortName = substr(cohortName, 5, nchar(cohortName)))

info(logger, "- getting initial cohorts")

cdm <- CDMConnector::generateCohortSet(cdm, Initial_cohorts,
                                       cohortTableName = cohort_table_name,
                                       overwrite = TRUE)

info(logger, "- got initial cohorts")