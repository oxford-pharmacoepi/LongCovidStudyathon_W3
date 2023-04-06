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

# Get TreatmentPatterns code
devtools::load_all(here::here("TreatmentPatterns_code"))

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
  if(onlyLC) {
    source(here("2_StudyCohorts","GetStudyCohorts_onlyLC.R"), local = TRUE)
  } else if(sql_server) {
    source(here("2_StudyCohorts","GetStudyCohorts_sql.R"), local = TRUE)
  } else {
    source(here("2_StudyCohorts","GetStudyCohorts.R"), local = TRUE)
  }
  info(logger, 'GOT STUDY COHORTS')
}

# Objective 1: Incidence and Prevalence
if(doIncidencePrevalence) {
  info(logger, 'GETTING INCIDENCE AND PREVALENCE')
  if(onlyLC) {
    source(here("3_IncidencePrevalence","WP1_code_onlyLC.R"), local = TRUE)
  } else {
    source(here("3_IncidencePrevalence","WP1_code.R"), local = TRUE)
  }
  info(logger, 'GOT INCIDENCE AND PREVALENCE')
}

# Objective 2a: Characterisation
if(doCharacterisation || doDrugUtilisation || doTreatmentPatterns) {
  info(logger, 'DOING LARGE-SCALE CHARACTERISATION, DRUG UTILISATION AND/OR TREATMENT PATTERNS')
  if(onlyLC) {
    source(here("4_Characterisation","WP2_code_onlyLC.R"), local = TRUE)
  } else if(sql_server) {
    source(here("4_Characterisation","WP2_code_sql.R"), local = TRUE)
  } else {
    source(here("4_Characterisation","WP2_code.R"), local = TRUE)
  }
  info(logger, 'FINISHED LARGE-SCALE CHARACTERISATION, DRUG UTILISATION AND/OR TREATMENT PATTERNS')
}

# Objective 3a: Clustering
if(doClustering) {
  info(logger, 'PERFORMING LCA CLUSTERING AND NETWORK ANALYSIS')
  if(sql_server) {
    source(here("5_Clustering","WP3_code_sql.R"), local = TRUE)
  } else {
    source(here("5_Clustering","WP3_code.R"), local = TRUE)
  }
  info(logger, 'FINISHED LCA CLUSTERING AND NETWORK ANALYSIS')
}

# Objective 3c: Trajectories
if(doTrajectories) {
  info(logger, 'STUDYING TRAJECTORIES')
  source(here("6_Trajectories","Trajectories.R"), local = TRUE)
  info(logger, 'FINISHED TRAJECTORIES')
}

# IP plots
if(doIncidencePrevalence) {
  info(logger, 'PLOTTING RESULTS')
  if(onlyLC) {
    source(here("7_Plots","plots_onlyLC.R"), local = TRUE)
  } else {
    source(here("7_Plots","plots.R"), local = TRUE)
  }
  info(logger, 'RESULTS PLOTTED')
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
