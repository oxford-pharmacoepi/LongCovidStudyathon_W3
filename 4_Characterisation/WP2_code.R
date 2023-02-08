# Large-scale characterisation part
# Only for base cohorts and for LC any / LC code / PASC any in overlap

# Output folder for WP2
output_lsc <- here::here(output.folder,"Large-scale Characterisation")
if (!file.exists(output_lsc)){
  dir.create(output_lsc, recursive = TRUE)}

# Should choose which temporalWindows, tables to Characterize, etc.
# Default temporalWindows =
# c(NA, -366), c(-365, -91),
# c(-365, -31), c(-90, -1), c(-30, -1),
# c(0, 0), c(1, 30), c(1, 90),
# c(31, 365), c(91, 365), c(366, NA)

# tablesToCharacterize = 
#  "condition_occurrence", "drug_era",
#  "procedure_occurrence", "measurement"

# -------------------------------------------------------------------
cohort_ids_interest <-  c(1:4,104:115)

# Characterisation without strata
charac <- CohortProfiles::largeScaleCharacterization(cdm,"studyathon_final_cohorts", targetCohortId = cohort_ids_interest)
write.csv(
  charac,
  file = here::here(output_lsc, "Characterisation_all"),
  row.names = FALSE
)

# Characterisation sex strata
do_sex_strata <- function(cohort_id,new_id) {
  sex_strata <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == cohort_id)
  females <- sex_strata %>% CohortProfiles::addSex(cdm) %>% dplyr::filter(sex == "Female") %>% mutate(cohort_definition_id = new_id)
  males <- sex_strata %>% CohortProfiles::addSex(cdm) %>% dplyr::filter(sex == "Male") %>% mutate(cohort_definition_id = new_id + 1)
  appendPermanent(females, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(males, name = "studyathon_final_cohorts",  schema = results_database_schema)
}
id_new_sex <- c(seq(125,155,2)) # only odd numbers!!!
lapply(cohort_ids_interest,id_new_sex,do_sex_strata)

charac_sex <- CohortProfiles::largeScaleCharacterization(cdm,"studyathon_final_cohorts", targetCohortId = c(125:156))
write.csv(
  charac_sex,
  file = here::here(output_lsc, "Characterisation_sex"),
  row.names = FALSE
)

# Characterisation age strata
do_age_strata <- function(cohort_id,new_id) {
  age_strata <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id == cohort_id)
  age1 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(0:2)) %>% mutate(cohort_definition_id = new_id)
  age2 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(3:5)) %>% mutate(cohort_definition_id = new_id+1)
  age3 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(6:9)) %>% mutate(cohort_definition_id = new_id+2)
  age4 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(10:13)) %>% mutate(cohort_definition_id = new_id+3)
  age5 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(14:17)) %>% mutate(cohort_definition_id = new_id+4)
  age6 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(18:40)) %>% mutate(cohort_definition_id = new_id+5)
  age7 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(41:64)) %>% mutate(cohort_definition_id = new_id+6)
  age8 <- age_strata %>% CohortProfiles::addAge(cdm) %>% dplyr::filter(age %in% c(65:120)) %>% mutate(cohort_definition_id = new_id+7)
  
  appendPermanent(age1, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age2, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age3, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age4, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age5, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age6, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age7, name = "studyathon_final_cohorts",  schema = results_database_schema)
  appendPermanent(age8, name = "studyathon_final_cohorts",  schema = results_database_schema)
}
id_new_age <- c(seq(157,277,8))
lapply(cohort_ids_interest,id_new_age,do_age_strata)

charac_age <- CohortProfiles::largeScaleCharacterization(cdm,"studyathon_final_cohorts", targetCohortId = c(157:284))
write.csv(
  charac_age,
  file = here::here(output_lsc, "Characterisation_age"),
  row.names = FALSE
)


# Characterisation calendar period strata



# Characterisation vaccination strata
