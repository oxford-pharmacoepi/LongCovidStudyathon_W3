# Run trajectories part

connection <- DatabaseConnector::connect(connectionDetails)
on.exit(DatabaseConnector::disconnect(connection)) #Close db connection on error or exit

names_in_cdm <- CohortNames[CohortNames %in% names(cdm)]
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = names_in_cdm)

output_traj <- file.path(tempDir,"Trajectories")
if (!file.exists(output_traj)){
  dir.create(output_traj, recursive = TRUE)}

if((doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName, TrajCohortsName))
} else if ((doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     HUCohortsName))
} else if (!(doCharacterisation || doClustering) && doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName,
                                     TrajCohortsName))
} else if (!(doCharacterisation || doClustering) && !doTrajectories) {
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,LongCovidCohortsName,
                                     PascCohortsName,MedCondCohortsName,VaccCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
}

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
