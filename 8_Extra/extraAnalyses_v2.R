# Read all cdm cohorts
  cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                    cohortTables = c(InitialCohortsName,BaseCohortsName,
                                     OverlapCohortsCName,OverlapCohortsIPName))
  
  # Output folder for Attrition
  output_at <- file.path(tempDir,"Attrition")
  if (!file.exists(output_at)){
    dir.create(output_at, recursive = TRUE)}
  
################################################################################
# Clustering with 4 classes everywhere
  
  if(!onlyLC) {
  names_final_cohorts <- read.csv(file.path(paste0(db.name,"_Results/"),paste0(db.name,"_cohorts.csv")))
  
  # Output folders for WP3
  output_clustering <- file.path(tempDir,"Clustering")
  if (!file.exists(output_clustering)){
    dir.create(output_clustering, recursive = TRUE)}

  info(logger, 'Calculating LCA clustering 4 classes')
  
  # First get all people with LC symptoms (overlap with infection cohort at base)
  symptoms_LC <- cdm[[OverlapCohortsIPName]] %>% 
    dplyr::filter(.data$cohort_definition_id %in% c(1:25))
  symptoms_LC <- symptoms_LC %>%
    dplyr::full_join(
      cdm[[OverlapCohortsCName]] %>% 
        dplyr::filter(.data$cohort_definition_id == 5) %>%
        dplyr::mutate(cohort_definition_id = 27)
    )
  
  names_symptoms <- names_final_cohorts %>% 
    dplyr::filter(.data$table_name == LongCovidCohortsName) %>%
    dplyr::filter(.data$cohort_definition_id %in% c(1:25,27)) %>%
    dplyr::select(cohort_definition_id, cohort_name) %>% compute()
  
  symptoms_LC <- symptoms_LC %>% addAge(cdm) %>% 
    addSex(cdm) %>% collect()
  symptoms_LC <- symptoms_LC %>% 
    dplyr::left_join(names_symptoms, 
                     by = c("cohort_definition_id")) %>% 
    dplyr::select(cohort_name,subject_id,age,sex)
  
  # Get the names of the symptoms or LC code
  names_symptoms <- symptoms_LC %>% dplyr::select(cohort_name) %>% distinct() %>% 
    pull()
  
  write.csv(names_symptoms, here::here(tempDir, paste0("names_symptoms_",db.name,".csv")))
  
  # Create table with columns of symptoms, 1 if individual has it, 0 otherwise
  i = 1
  working_name <- names_symptoms[i]
  working_name <- enquo(working_name)
  data_LCA <- symptoms_LC %>% dplyr::filter(cohort_name == !!working_name) %>% 
    dplyr::mutate(!!working_name := as.integer(1)) %>% 
    dplyr::select(subject_id,!!working_name)
  
  for(i in 2:length(names_symptoms)) {
    working_name <- names_symptoms[i]
    working_name <- enquo(working_name)
    data_LCA <- data_LCA %>% full_join(symptoms_LC %>% 
                                         dplyr::filter(cohort_name == !!working_name) %>% 
                                         dplyr::mutate(!!working_name := as.integer(1)) %>% 
                                         dplyr::select(subject_id,!!working_name), 
                                       by = c("subject_id"))
    
  }
  data_LCA[is.na(data_LCA)] <- 0
  data_LCA <- data_LCA %>% distinct()
  data_LCA <- data_LCA %>% dplyr::left_join(symptoms_LC %>% 
                                              dplyr::select(subject_id,age,sex), 
                                            by = "subject_id") %>%
    distinct()
  
  # Use package polCA
  # Fit latent class model
  mydata <- data_LCA 
  mydata[,2:(ncol(mydata)-2)] <- mydata[,2:(ncol(mydata)-2)] + 1 
  # needed for poLCA, now 0 (no symptom) is 1, and 1 (symptom) is 2. Not do that in age or sex or subject_id
  # the following is just to create variables that can be read by the formula of LCA
  n_s <- names_symptoms
  for(i in 1:length(names_symptoms)) {
    n_s[i] = paste0("LC_",i)
  }
  n_s <- append(n_s,"age")
  n_s <- append(n_s,"sex")
  colnames(mydata) <- c("subject_id",n_s)
  
  # Get the expression of the function for LCA depending on the number of variables available in the dataset
  cols <- "cbind("
  for(i in 1:length(names_symptoms)) {
    cols <- paste0(cols,colnames(mydata)[i+1],",")
  }
  cols <- substr(cols, 1, nchar(cols)-1)
  cols <- paste0(cols,")")
  x_vars <- c("1", "age", "sex")
  
  f <- with(mydata, as.formula(sprintf("%s ~ %s", cols, paste(x_vars, collapse = " + "))))
  
lc <- poLCA(f, mydata, nclass=4, maxiter=2000, graphs = TRUE, tol=1e-5, na.rm=FALSE,  
nrep=100, verbose=TRUE, calc.se=TRUE)
 	
for(i in 1:(length(lc)-1)) {
  write.csv(
    lc[[i]],
    file = here::here(output_clustering, paste0("Clustering_LCA_model4_",names(lc)[i],".csv")),
    row.names = FALSE
  )
}

lcmodel <- reshape2::melt(lc$probs, level=2)
lcmodel$L2 <- stringr::str_to_title(lcmodel$L2)
for(i in 1:length(names_symptoms)) {
  lcmodel$L2[lcmodel$L2 == paste0("Lc_",i)] <- names_symptoms[i]
  
}

# Get nice plot of class membership
zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(Var1 ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Blues",labels = c("No symptom", "Symptom")) +theme_bw()
zp1 <- zp1 + labs(x = "Symptoms",y=paste0("Prevalence syptoms in ", db.name), fill ="Response categories")	#x = "Questionnaire items"
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank(),
                    axis.text.x=element_text(size=10, angle = 90, vjust = 0.5, hjust=1))
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))

# Include horizontal lines with Prevalence of each symptom in all patients
# List prevalence values (range from 0 to 1)
# Transform back (1,2) to (0,1) binary
mydata[,2:(ncol(mydata)-2)] <- mydata[,2:(ncol(mydata)-2)] -1 
factors <- (colSums(mydata[,2:(ncol(mydata)-2)]))/(length(mydata[[1]]))	
factors <- as.matrix(factors)
rownames(factors) <- names_symptoms
factors <- factors[order(rownames(factors)),]

# Include prevalence average lines
for (i in 1:length(factors)) {
  zp1 <- zp1 + geom_segment(x = (i - 0.5), y = factors[[i]], xend = (i + 0.5), yend = factors[[i]])
}
ggsave(here::here(output_clustering, "Clustering_LCA.jpg"))

}

################################################################################
# Drug Utilisation and Treatment Patterns

if(sql_server) {
  source(here("2_StudyCohorts","functions_getCohorts_sql.R"))
} else {
  source(here("2_StudyCohorts","functions_getCohorts.R"))
}

cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,
                                   LongCovidCohortsName, PascCohortsName,
                                   OverlapCohortsCName))

# EXTRA COHORT: Infection + Any LC symptom with index date at symptom event
info(logger, 'STARTING TO GET EXTRA COHORTS')
if(onlyLC) {
  do_overlap_LCany(cdm, c(1:2), c(5:29), c(1:2), indexsymptom = TRUE)
  
  # LC code + Infection
  do_overlap(cdm, 1, 27, 3, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # LC code + Reinfection
  do_overlap(cdm, 2, 27, 4, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
} else {
  do_overlap_LCany(cdm, c(1:4), c(5:29), c(1:4), indexsymptom = TRUE)

  # LC code + Infection
  do_overlap(cdm, 1, 27, 5, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # LC code + Reinfection
  do_overlap(cdm, 2, 27, 6, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # LC code + Test negative
  do_overlap(cdm, 3, 27, 7, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # LC code + Influenza
  do_overlap(cdm, 4, 27, 8, washout = FALSE, tableName = LongCovidCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # PASC any symptom + Infection
  do_overlap(cdm, 1, 11, 9, washout = FALSE, tableName = PascCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # PASC any symptom + Reinfection
  do_overlap(cdm, 2, 11, 10, washout = FALSE, tableName = PascCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # PASC any symptom + Test negative
  do_overlap(cdm, 3, 11, 11, washout = FALSE, tableName = PascCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
  # PASC any symptom + Influenza
  do_overlap(cdm, 4, 11, 12, washout = FALSE, tableName = PascCohortsName,
             overlapTableName = Extrav2CohortsName, indexsymptom = TRUE)
  
#  create_outcome(cdm, window = c(68:99), filter_start = FALSE, first_event = FALSE,
#                 new_ids = c(13:44), tableName = BaseCohortsName)
}

# Output folders
output_lsc <- file.path(tempDir,"Large-scale Characterisation")
if (!file.exists(output_lsc)){
  dir.create(output_lsc, recursive = TRUE)}

if(!onlyLC) {
  output_du <- file.path(tempDir,"Drug Utilisation")
  if (!file.exists(output_du)){
    dir.create(output_du, recursive = TRUE)}
}

info(logger, 'EXTRA LC ANY COHORTS CREATED')


cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = c(InitialCohortsName,BaseCohortsName,
                                   OverlapCohortsCName, Extrav2CohortsName))

# Repeat some LSC and DU calculations
info(logger, 'START EXTRA ANALYSES')
if (onlyLC) {
  source(here::here("4_Characterisation","functions_characterisation.R"))
  do_lsc(c(1:30), "all_base", BaseCohortsName, any = FALSE)
  do_lsc(c(1:60), "all_any", OverlapCohortsCName, any = FALSE)
  
} else if (sql_server) {
  source(here::here("4_Characterisation","functions_characterisation_sql.R"))
  if(vaccine_data) {
    cohorts_interest_base <- c(1:60)
    cohorts_interest_any <- c(1:180)
    cohorts_extra_any <- c(1:12)
  } else {
    cohorts_interest_base <- c(1:44, 53:60)
    cohorts_interest_any <- c(1:132, 157:180)
    cohorts_extra_any <- c(1:12)
  }
  
  do_lsc(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_lsc(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  info(logger, 'LSC FINISHED')
  
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_du(cohorts_interest_any, "all_any", OverlapCohortsCName)
  do_du(cohorts_extra_any, "all_any_extra", Extrav2CohortsName, any = FALSE)
  
#  for(i in cohorts_extra_any) {
#    do_tp(i, c(13:44), Extrav2CohortsName)
#  }
  
} else {
  source(here::here("4_Characterisation","functions_characterisation.R"))
  if(vaccine_data) {
    cohorts_interest_base <- c(1:60)
    cohorts_interest_any <- c(1:180)
    cohorts_extra_any <- c(1:12)
    
  } else {
    cohorts_interest_base <- c(1:44, 53:60)
    cohorts_interest_any <- c(1:132, 157:180)
    cohorts_extra_any <- c(1:12)
    
  }
  
  do_lsc(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_lsc(cohorts_interest_any, "all_any", OverlapCohortsCName)
  
  info(logger, 'LSC FINISHED')
  
  cdm[["drug_exposure"]] <- cdm[["drug_exposure"]] %>%
    dplyr::filter(lubridate::year(.data$drug_exposure_start_date) >= 2016) %>%
    dplyr::compute()
  
  do_du(cohorts_interest_base, "all_base", BaseCohortsName, any = FALSE)
  do_du(cohorts_interest_any, "all_any", OverlapCohortsCName)
  do_du(cohorts_extra_any, "all_any_extra", Extrav2CohortsName, any = FALSE)
  
#  for(i in cohorts_extra_any) {
#    do_tp(i, c(13:44), Extrav2CohortsName)
#  }
  
}

info(logger, 'ALL DONE!')
