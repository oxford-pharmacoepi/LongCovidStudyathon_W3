# Clustering 

# Do the same with LC code when available? Or introduce it here as another variable?? Yes, I think.

# -------------------------------------------------------------------------------------------
# LCA clustering

# Convert the table to a "binary" format required by polCA
# First get all people with LC symptoms (overlap with infection cohort at base)
symptoms_LC <- cdm[["studyathon_final_cohorts"]] %>% dplyr::filter(cohort_definition_id %in% c(205:229))
symptoms_LC <- symptoms_LC %>% CohortProfiles::addAge(.,cdm) %>% CohortProfiles::addSex(.,cdm) %>% collect()
# FC_cohorts <- CDMConnector::readCohortSet(here("JSONS","FC_time_cohorts"))
# symptoms_LC <- symptoms_LC %>% mutate(cohort_definition_id = as.integer(cohort_definition_id))
# symptoms_LC <- as_tibble(symptoms_LC)
symptoms_LC <- symptoms_LC %>% mutate(cohort_definition_id = cohort_definition_id - 200) %>% 
  left_join(Initial_cohorts %>% dplyr::select(cohortId,cohortName) %>% mutate(cohort_definition_id = as.numeric(cohortId)), by = c("cohort_definition_id")) %>% 
  dplyr::select(cohortName,subject_id,age,sex)

# Get the names of the symptoms
names_symptoms <- symptoms_LC %>% dplyr::select(cohortName) %>% distinct() %>% pull()

# Create table with columns of symptoms, 1 if individual has it, 0 otherwise
i = 1
working_name <- names_symptoms[i]
working_name <- enquo(working_name)
data_LCA <- symptoms_LC %>% filter(cohortName == !!working_name) %>% mutate(!!working_name := as.integer(1)) %>% dplyr::select(subject_id,!!working_name)

for(i in 2:length(names_symptoms)) {
  working_name <- names_symptoms[i]
  working_name <- enquo(working_name)
  data_LCA <- data_LCA %>% full_join(symptoms_LC %>% filter(cohortName == !!working_name) %>% mutate(!!working_name := as.integer(1)) %>% dplyr::select(subject_id,!!working_name), by = c("subject_id"))
  
}
data_LCA[is.na(data_LCA)] <- 0
data_LCA <- data_LCA %>% distinct()
data_LCA <- data_LCA %>% left_join(symptoms_LC %>% dplyr::select(subject_id,age,sex), by = "subject_id")

# Use package polCA
# Fit latent class model
mydata <- data_LCA %>% dplyr::select(- subject_id)
mydata[,1:(ncol(mydata)-2)] <- mydata[,1:(ncol(mydata)-2)] + 1 # needed for poLCA, now 0 (no symptom) is 1, and 1 (symptom) is 2. Not do that in age or sex
# the following is just to create variables that can be read by the formula of LCA
n_s <- names_symptoms
for(i in 1:length(names_symptoms)) {
  n_s[i] = paste0("LC_",i)
}
n_s <- append(n_s,"age")
n_s <- append(n_s,"sex")
colnames(mydata) <- n_s
# Only 23 as two symptoms don't appear, should do recursively depending on data partners
f <- with(mydata, cbind(LC_1, LC_2, LC_3, LC_4, LC_5, LC_6,  LC_7,  LC_8,  LC_9,  LC_10, LC_11, LC_12, LC_13, LC_14, LC_15, LC_16, LC_17, LC_18, LC_19, LC_20, LC_21, LC_22, LC_23)~ 1 + age + sex)
#------ run a sequence of models with 1-10 classes and print out the model with the lowest BIC
# Tune these I guess???
max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f, mydata, nclass=i, maxiter=1000, graphs = TRUE,
              tol=1e-5, na.rm=FALSE,  
              nrep=5, verbose=TRUE, calc.se=TRUE) # Think of these parameters
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model

# visualisation of the model??????? Some plots at least


# -------------------------------------------------------------------------------------------
# Network clustering

# Number of people with each symptom
number_people <- data_LCA %>% dplyr::select(-c(subject_id,age,sex)) %>% summarise(across(,sum)) %>%  unlist(., use.names=FALSE)

# HERE I DO PHI COEFFICIENT (MAKES SENSE FOR BINATY VARIABLES), EVEN THOUGH WE CANNOT ACCOUNT FOR OTHER COVARIATES AT THE SAME TIME
# Could also use other distance measures for dichotomous variables like https://docs.scipy.org/doc/scipy/reference/spatial.distance.html
phi_matrix <- matrix(NA,nrow = length(names_symptoms), ncol = length(names_symptoms))
for(i in 1:length(names_symptoms)) {
  for(j in 1:length(names_symptoms)) {
    if(i == j) phi_matrix[i,j] = 1
    else {
      phi_matrix[i,j] = mydata[,c(i,j)] %>% table() %>% phi()
      phi_matrix[j,i] = phi_matrix[i,j]
      # mydata[,c(i,j)] %>% spicy::distance_measure_of_choice()
    }
  }
}

phi_test <- phi_matrix
phi_test <- abs(phi_test) # only positive (think!)
phi_test[phi_test < 0.05] <- 0 # Think of which number I use as a threshold
diag(phi_test) <- 0 # We don't care about obvious correlation of each variable with itself

# build network with nodes symptoms, edges phi coefficient using igraph package
ig <- graph.adjacency(phi_test, mode="undirected", weighted=TRUE)
# Tune this in a way that is visually beautiful and related to the value of people affected / strength of the association
V(ig)$size <- log(number_people)
E(ig)$width <- 10*E(ig)$weight

# visualise: CHOOSE HOW!!
# https://kateto.net/netscix2016.html
plot(ig)

# community detection: MCA?








