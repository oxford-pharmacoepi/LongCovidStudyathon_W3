# ---------------------------------------------------------------------
# COHORT DIAGNOSTICS
# Export folder
output_cd <- file.path(tempDir,"Cohort Diagnostics")
if (!file.exists(output_cd)){
  dir.create(output_cd, recursive = TRUE)}

# Run diagnostics of the base cohorts and the LC code cohort (pre-inclusion and exclusion criteria)
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c("studyathon_lcpasc","studyathon_final_cohorts"))

# Get cohort definition files
cohortJsonFiles <- list.files(here("1_InitialCohorts", "Jsons"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
id_jsons_interest <- c(1,2,3,4,64)
for(i in c(1:5)){
  id <- id_jsons_interest[i]
  working.json<-here("1_InitialCohorts", "Jsons",
                     cohortJsonFiles[id])
  cohortJson <- readChar(working.json, file.info(working.json)$size)
  cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
  sql <- buildCohortQuery(cohortExpression, 
                          options = CirceR::createGenerateOptions(generateStats = TRUE))
  
  cohortDefinitionSet[[i]]<-tibble(atlasId = i,
                                   cohortId = id,
                                   cohortName = str_replace(cohortJsonFiles[id],".json",""),
                                   json=cohortJson,
                                   sql=sql,
                                   logicDescription = NA,
                                   generateStats=TRUE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "studyathon_lcpasc")

CohortGenerator::createCohortTables(connectionDetails= connectionDetails,
                                    cohortDatabaseSchema = results_database_schema,
                                    cohortTableNames = cohortTableNames)
# Tables in the database already created
# Generate the cohort set
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# Base cohorts instantiated as json or inclusion/exclusion tables afterwards?
# Cohorts 1, 2, 3, 4 and 64
if(tableCohortDiagnostics) {
  CohortDiagnostics::createCountsTable()
}
CohortDiagnostics::executeDiagnostics(cohortDefinitionSet,
                                      connectionDetails = connectionDetails,
                                      cohortTable = "studyathon_lcpasc",
                                      cohortIds = c(1,2,3,4,64),
                                      cohortDatabaseSchema = results_database_schema,
                                      cdmDatabaseSchema = cdm_database_schema,
                                      exportFolder = output_cd,
                                      databaseId = db.name,
                                      minCellCount = 5,
                                      runInclusionStatistics = FALSE, 
                                      runOrphanConcepts = TRUE,
                                      runTimeDistributions = FALSE, 
                                      runVisitContext = FALSE,
                                      runBreakdownIndexEvents = TRUE, 
                                      runIncidenceRate = FALSE, 
                                      runTimeSeries = FALSE, 
                                      runCohortOverlap = FALSE, 
                                      runCohortCharacterization = FALSE,
                                      runTemporalCohortCharacterization = FALSE)


