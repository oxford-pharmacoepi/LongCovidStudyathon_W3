# This is the only code the user should interact with
# Runs extra characterisation studies

# Required packages from CRAN:
library(RPostgres)
library(tools)
library(DBI)
library(dplyr)
library(dbplyr)
library(CDMConnector)
library(here)
library(log4r)
library(zip)
library(poLCA)
library(igraph)
library(psych)
library(MatchIt)
library(purrr)
library(ggplot2)
library(IncidencePrevalence)
library(lubridate)
library(tibble)
library(reshape2)
library(readr)

# Database name or acronym (e.g. for CPRD AURUM use "CPRUAurum")
db.name <- "..."

# Name of the output folder to save the results. Change to "output" or any other
# desired path
output.folder <- here::here()

# Stem to use for the cohort tables in the database
table_stem <- "..."

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

# sql dialect used with the OHDSI SqlRender package
targetDialect <- "..." 

# Create connection details "OHDSI way" for some packages
server <- Sys.getenCopyOfv("...")
connectionDetails <- DatabaseConnector::downloadJdbcDrivers(targetDialect, here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = targetDialect,
  server = server,
  user = user,
  password = password,
  port = port,
  pathToDriver = here::here())

# Name of the schema with the patient-level data
cdm_database_schema <- "..."

# Name of the schema with the vocabulary, usually the same as the patient-level
# data schema
vocabulary_database_schema <- cdm_database_schema

# Name of the schema where the result table will be created
results_database_schema <- "..."

# Study start date, should not change this
study_start_date <- as.Date("2020-09-01")

# Covid end date, country specific, when testing ended
# Might not be applicable, otherwise set as latest_data_availability
covid_end_date <- as.Date("...")

# Start date of omicron variant, country specific
omicron_start_date <- as.Date("...")

# Latest data availability, to know until when to calculate incidences
latest_data_availability <- as.Date("...") 

# Set to true or false for the following information for your database
vaccine_data <- TRUE # Set to FALSE if you have no information on vaccination 
# whatsoever - and thus cannot stratify by it
vaccine_brand <- TRUE # Set to FALSE if you do have information on vaccination,
# but not on vaccine brand

# If you can only run Long Covid related analyses
onlyLC <- FALSE

# If your database engine is SQL Server
sql_server <- FALSE

# Run the study
source(here("RunStudy_v2.R"))

# After this is run you should have a zip file in your output folder to share