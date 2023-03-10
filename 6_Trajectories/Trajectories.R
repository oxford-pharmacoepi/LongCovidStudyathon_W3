connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

output_traj <- file.path(tempDir,"Trajectories")
if (!file.exists(output_traj)){
  dir.create(output_traj, recursive = TRUE)}

# Save the "any LC symptom + infection" or "infection + LC code" cohort in a table which can be read by Trajectories package
traj_table <- cdm$studyathon_final_cohorts %>%
  dplyr::filter(cohort_definition_id %in% c(105,109)) %>%
  mutate(cohort_definition_id = 1)
computeQuery(new_infection,
             name = "studyathon_trajcohort",
             temporary = FALSE,
             schema = write_database_schema,
             overwrite = TRUE)
#computePermanent(traj_table, name = "studyathon_trajcohort",  
#                 schema = results_database_schema, overwrite = TRUE)


cdm <- cdmFromCon(
  db, cdm_database_schema, writeSchema = results_database_schema, 
  cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts","studyathon_trajcohort"))

# Setting local system & database parameters - CHANGE ACCORDING TO YOUR SYSTEM & DATABASE:
trajectoryLocalArgs <- Trajectories::createTrajectoryLocalArgs(oracleTempSchema="",
                                                               prefixForResultTableNames = "studyathon_traj", 
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
