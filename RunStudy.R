# Create zip file
zipName <- paste0(db.name,"_Results")
tempDir <- zipName
tempDirCreated <- FALSE
if (!dir.exists(tempDir)) {
  dir.create(tempDir)
  tempDirCreated <- TRUE
}

start <- Sys.time()

# Start log
log_file <- paste0(tempDir, "/log.txt")
# Think what to do if already there. Overwrite, append?
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Create table names to use throughout the study
InitialCohortsName <- paste0(table_stem,"_initialcohorts")
BaseCohortsName <- paste0(table_stem,"_basecohorts")
LongCovidCohortsName <- paste0(table_stem,"_lccohorts")
OverlapCohortsName <- paste0(table_stem,"_overlapcohorts")
VaccCohortsName <- paste0(table_stem,"_vacccohorts")
HUCohortsName <- paste0(table_stem,"_hucohorts")
clusterCohortName <- paste0(table_stem,"_clustercohort")

# Create vector with all names
CohortNames <- c(InitialCohortsName, BaseCohortsName, LongCovidCohortsName,
                 OverlapCohortsName, HUCohortsName, VaccCohortsName, clusterCohortName)

# get functions used throughout this study
source(here::here("functions.R"))

# Read initial cohorts
if (readInitialCohorts){
  info(logger, 'INSTANTIATING INITIAL COHORTS')
  cdm <- cdmFromCon(db, cdm_database_schema, 
                    writeSchema = results_database_schema)
  source(here("1_InitialCohorts", "InstantiateStudyCohorts.R"), local=TRUE)
  info(logger, 'GOT INITIAL COHORTS')
} else {
  info(logger, 'INITIAL COHORTS ALREADY INSTANTIATED')
  Initial_cohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts", "Jsons")) %>%
    dplyr::mutate(cohort_name = substr(cohort_name, 5, nchar(cohort_name)))
  cdm <- cdmFromCon(
    db, cdm_database_schema, writeSchema = results_database_schema,
    cohortTables = InitialCohortsName)
  info(logger, 'INITIAL COHORTS READ')
}

# Instantiate study cohorts
if(getStudyCohorts) {
  info(logger, 'GETTING STUDY COHORTS')
  source(here("2_StudyCohorts","GetStudyCohorts.R"), local = TRUE)
  info(logger, 'GOT STUDY COHORTS')
}

# Objective 3a: Clustering
if(doClustering) {
  info(logger, 'PERFORMING LCA CLUSTERING')
  source(here("3_Clustering","WP3_code.R"), local = TRUE)
  info(logger, 'FINISHED LCA CLUSTERING')
}

zip::zip(zipfile = file.path(output.folder, paste0(zipName, ".zip")),
         files = list.files(tempDir, full.names = TRUE))
if (tempDirCreated) {
  unlink(tempDir, recursive = TRUE)
}
info(logger, 'SAVED RESULTS IN THE OUTPUT FOLDER')

print("Done!")
print("If all has worked, there should now be a zip file with your results
      in the output folder to share")
print("Thank you for running the study!")
Sys.time() - start
readLines(log_file)
