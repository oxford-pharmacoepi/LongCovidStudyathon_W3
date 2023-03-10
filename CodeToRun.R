# This is the only code the user should interact with
# Runs the Studyathon Long Covid and PASC Project

# Required packages :
library("DBI")
library("dplyr")
library("dbplyr")
library("CirceR")
library("CDMConnector")
library("DatabaseConnector")
library("here")
library("log4r")
library("zip")
library("poLCA")
library("igraph")
library("psych")
library("Trajectories")
library("MatchIt")
library("purrr")
library("MCL")
library("ggplot2")
library("colorBlindness")
library("networkD3")

# get with remotes
library(IncidencePrevalence)
library(CohortProfiles)
library(LargeScaleCharacteristics)
library(TreatmentPatterns)

# Database name or acronym (e.g. for CPRD AURUM use "CPRUAurum")
db.name <- "..."

# Name of the output folder to save the results. Change to "output" or any other
# desired path
output.folder <- here("Results",db.name)

# Change the following parameters with your own database information
user <- Sys.getenv("...")
password <- Sys.getenv("...")
port <- Sys.getenv("...")
host <- Sys.getenv("...")
server_dbi <- Sys.getenv("...")

# Create database connection
# We use the DBI package to create a cdm_reference
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Name of the schema with the patient-level data
cdm_database_schema <- "..."

# Name of the schema with the vocabulary, usually the same as the patient-level
# data schema
vocabulary_database_schema <- cdm_database_schema

# Name of the schema where the result table will be created
results_database_schema <- "..."

# Study start date, should not change this
study_start_date <- as.Date("...")

# Covid end date, country specific, when testing ended
covid_end_date <- as.Date("...")

# Latest data availability, to know until when to calculate incidences
latest_data_availability <- as.Date("...") 

# Decide which parts of the study you want to run 
readInitialCohorts <- TRUE
getStudyCohorts <- TRUE
doCohortDiagnostics <- TRUE
doIncidencePrevalence <- TRUE
doCharacterisation <- TRUE
doDrugUtilisation <- TRUE
doTreatmentPatterns <- TRUE
doClustering <- TRUE
doTrajectories <- TRUE

# Set to true or false for the following information for your database
vaccine_data <- TRUE # Set to FALSE if you have no information on vaccination 
# whatsoever - and thus cannot stratify by it
vaccine_brand <- TRUE # Set to FALSE if you do have information on vaccination,
# but not on vaccine brand

# Create counts table of Cohort Diagnostics
tableCohortDiagnostics <- FALSE

# Run the study
source(here("RunStudy.R"))

# After this is run you should have a zip file in your output folder to share