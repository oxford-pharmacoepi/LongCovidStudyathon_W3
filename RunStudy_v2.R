# Create zip file
zipName <- paste0(db.name,"_Results_v2")
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
PascCohortsName <- paste0(table_stem,"_pasccohorts")
MedCondCohortsName <- paste0(table_stem,"_mccohorts")
OverlapCohortsCName <- paste0(table_stem,"_overlapccohorts")
OverlapCohortsIPName <- paste0(table_stem,"_overlapipcohorts")
HUCohortsName <- paste0(table_stem,"_hucohorts")
TrajCohortsName <- paste0(table_stem,"_trajcohort")
VaccCohortsName <- paste0(table_stem,"_vacccohorts")

# Create vector with all names
CohortNames <- c(InitialCohortsName, BaseCohortsName, LongCovidCohortsName,
                 PascCohortsName, MedCondCohortsName, OverlapCohortsCName,
                 OverlapCohortsIPName, HUCohortsName, TrajCohortsName,
                 VaccCohortsName)

# get functions used throughout this study
source(here::here("functions.R"))

# Read initial cohorts
  info(logger, 'INITIAL COHORTS ALREADY INSTANTIATED')
  Initial_cohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts", "Jsons")) %>%
    dplyr::mutate(cohort_name = substr(cohort_name, 5, nchar(cohort_name)))
  cdm <- cdmFromCon(
    db, cdm_database_schema, writeSchema = results_database_schema,
    cohortTables = InitialCohortsName)

# Do extra characterisation analyses
  source(here::here("8_Extra", "extraAnalyses_v2.R"))

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
