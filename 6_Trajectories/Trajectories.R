# Run trajectories part
names_in_cdm <- CohortNames[CohortNames %in% names(cdm)]
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = names_in_cdm)

output_traj <- file.path(tempDir,"Trajectories")
if (!file.exists(output_traj)){
  dir.create(output_traj, recursive = TRUE)}

# Save the "any LC symptom + infection" or "infection + LC code" cohort in a table which can be read by Trajectories package
traj_table <- cdm[[OverlapCohortsCName]] %>%
  dplyr::filter(cohort_definition_id %in% c(1,5)) %>%
  dplyr::mutate(cohort_definition_id = 1)
computeQuery(traj_table, name = TrajCohortsName,  
             temporary = FALSE,
             schema = results_database_schema, overwrite = TRUE)

names_in_cdm <- CohortNames[CohortNames %in% names(cdm)]
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = names_in_cdm)

# Setting local system & database parameters - CHANGE ACCORDING TO YOUR SYSTEM & DATABASE:
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema="",
                                                               prefixForResultTableNames = paste0(table_stem,"_traj"), 
                                                               cdmDatabaseSchema = cdm_database_schema,
                                                               vocabDatabaseSchema =vocabulary_database_schema,
                                                               resultsSchema = results_database_schema,
                                                               sqlRole = F, 
                                                               inputFolder=paste0(here::here("6_Trajectories")), 
                                                               mainOutputFolder=output_traj, 
                                                               databaseHumanReadableName='LCPASC')


# ##################################################
# RUN DISCOVERY ANALYSIS
# ##################################################

Trajectories::discover(connection,
                       trajectoryLocalArgs,
                       createCohort=F,
                       validationSetSize=0, #set to 0 if you are you going to validate the results in another databaase anyways
                       createEventPairsTable=T,
                       runDiscoveryAnalysis=T,
                       forceRecalculationOfAnalysis=F, #used only if runDiscoveryAnalysis=T
                       createFilteredFullgraphs=T,
                       runTrajectoryAnalysis=T,
                       selfValidate=F, #set to F if you are you going to validate the results in another databaase anyways
                       cleanup=F)

# ##################################################
# DISCONNECT FROM DATABASE
# ##################################################

DatabaseConnector::disconnect(connection)

# ##################################################
