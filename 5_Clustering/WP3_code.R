# Clustering 

names_in_cdm <- CohortNames[CohortNames %in% names(cdm)]
cdm <- cdmFromCon(db, cdm_database_schema, writeSchema = results_database_schema,
                  cohortTables = names_in_cdm)

# Output folders for WP3
output_clustering <- file.path(tempDir,"Clustering")
if (!file.exists(output_clustering)){
  dir.create(output_clustering, recursive = TRUE)}

# -------------------------------------------------------------------------------------------
# LCA CLUSTERING
# Helpful: https://statistics.ohlsen-web.de/latent-class-analysis-polca/
# Read this: https://www.john-uebersax.com/stat/faq.htm#work

info(logger, '-- Calculating LCA clustering')

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

symptoms_LC <- symptoms_LC %>% CohortProfiles::addAge(cdm) %>% 
  CohortProfiles::addSex(cdm) %>% collect()
symptoms_LC <- symptoms_LC %>% 
  dplyr::left_join(names_symptoms, 
            by = c("cohort_definition_id")) %>% 
  dplyr::select(cohort_name,subject_id,age,sex)

# Get the names of the symptoms or LC code
names_symptoms <- symptoms_LC %>% dplyr::select(cohort_name) %>% distinct() %>% 
  pull()

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
                                   by = "subject_id")

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

# Run a sequence of models with 2-7 classes and print out the model with the lowest BIC
LCA_models <- list()
for(i in 2:7){
  lc <- poLCA(f, mydata, nclass=i, maxiter=2000, graphs = TRUE,
              tol=1e-5, na.rm=FALSE,  
              nrep=100, verbose=TRUE, calc.se=TRUE)
    LCA_models[[i-1]] <- lc
}    	
# nrep, maxiter or the number of classes can be tuned later on if for some databases convergence or IC output does not look right

# Create table with information criteria for the models
results <- tibble::tibble(Model= 2,
                      log_likelihood=LCA_models[[1]]$llik,
                      df = LCA_models[[1]]$resid.df,
                      BIC=LCA_models[[1]]$bic,
                      ABIC=  (-2*LCA_models[[1]]$llik) + ((log((LCA_models[[1]]$N + 2)/24)) * LCA_models[[1]]$npar),
                      CAIC = (-2*LCA_models[[1]]$llik) + LCA_models[[1]]$npar * (1 + log(LCA_models[[1]]$N)), 
                      likelihood_ratio=LCA_models[[1]]$Gsq)

add_model_info <- function(results,model,name) {
  results <- rbind(results,
                   tibble::tibble(
                     Model= name,
                     log_likelihood=model$llik,
                     df = model$resid.df,
                     BIC=model$bic,
                     ABIC=  (-2*model$llik) + ((log((model$N + 2)/24)) * model$npar),
                     CAIC = (-2*model$llik) + model$npar * (1 + log(model$N)), 
                     likelihood_ratio=model$Gsq)
                   )
  return(results)
}

# Populate with model information
results <- add_model_info(results,LCA_models[[2]], 3)
results <- add_model_info(results,LCA_models[[3]], 4)
results <- add_model_info(results,LCA_models[[4]], 5)
results <- add_model_info(results,LCA_models[[5]], 6)
results <- add_model_info(results,LCA_models[[6]], 7)

# Add entropy
entropy <- function (p) sum(-p*log(p))
entropy_numbers <- c(rep(NA,6))
for(i in 2:7) {
  error_prior <- entropy(LCA_models[[i-1]]$P) # class proportions model 2
  error_post <- mean(apply(LCA_models[[i-1]]$posterior,1, entropy),na.rm = TRUE)
  entropy_numbers[i-1] <- round(((error_prior-error_post) / error_prior),3)
  results <- results %>% dplyr::mutate(entropy = entropy_numbers)
}

results2 <- tidyr::gather(results,Criteria,Value,4:7)
results2

fit.plot<-ggplot(results2) + 
  geom_point(aes(x=Model,y=Value),size=3) +
  geom_line(aes(Model, Value, group = 1)) +
  theme_bw()+
  labs(x = "", y="", title = "") + 
  facet_grid(Criteria ~. ,scales = "free") +
  theme_bw(base_size = 16, base_family = "") +   
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(colour="grey", size=0.5),
        legend.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text=  element_text(size=16),
        axis.line = element_line(colour = "black")) 

# Save plot and table with IC information
write.csv(
  results,
  file = here::here(output_clustering, paste0("Information_criteria.csv")),
  row.names = FALSE
)
ggsave(here::here(output_clustering, "LCA_IC.jpg"))

# Inspect the mean posterior probability, should be > 50% for 2 clusters, >66% for two, >75% for three, etc., for a good distinction of clusters
mean_posterior <-list()
for(i in 1:6) {
  df <- LCA_models[[i]]$posterior
  df[] <- t(apply(df, 1, function(x) replace(x, x != max(x, na.rm = TRUE), NA)))
  numerator <- apply(df,2,sum,na.rm=T)
  denominator <- ifelse(is.na(df),0,1)
  denominator <- apply(denominator,2,sum,na.rm=T)
  mean_posterior[[i]] <- numerator/denominator*100
}
for(i in length(mean_posterior)) {
  write.csv(
    mean_posterior[[i]],
    file = here::here(output_clustering, paste0("Mean_posterior",i,".csv")),
                      row.names = FALSE
    )
}

# Save all model outputs
for(k in 2:7) {
  lc <- LCA_models[[k-1]]
  for(i in 1:(length(lc)-1)) {
    write.csv(
      lc[[i]],
      file = here::here(output_clustering, paste0("Clustering_LCA_model",k,"_",names(lc)[i],".csv")),
      row.names = FALSE
    )
  }
}

# Pick the best model
number_best_model <- results %>% dplyr::filter(BIC == min(BIC)) %>% 
  dplyr::select(Model) %>% pull()
lc <- LCA_models[[number_best_model -1]] 
# selected according to previous plot, for now we get the one with the lowest bic. Should do it consciously in April

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

# Look at characterisation of clusters: age and sex
mydata <- mydata %>% mutate(cluster_assignment = lc$predclass)
clusters_age <- mydata %>%
  dplyr::group_by(cluster_assignment) %>%
  dplyr::summarise(mean_age = mean(age),
                   sd_age = sd(age))
clusters_sex <- mydata %>%
  dplyr::mutate(sex_male = ifelse(sex == "Male", 1, 0)) %>%
  dplyr::mutate(sex_female = ifelse(sex == "Female", 1, 0)) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(cluster_assignment) %>%
  dplyr::mutate(denom = sum(count)) %>%
  dplyr::summarise(percent_male = 100*sum(sex_male)/denom,
                   percent_female = 100*sum(sex_female)/denom) %>%
  dplyr::distinct()

write.csv(
  clusters_age,
  file = here::here(output_clustering, paste0("Cluster_age.csv")),
  row.names = FALSE
)

write.csv(
  clusters_sex,
  file = here::here(output_clustering, paste0("Cluster_sex.csv")),
  row.names = FALSE
)

# Look at healthcare utilisation outcomes

# Gte hospitalisation codes
ip.codes <- c(9201, 262)
# add all descendents
ip.codes.w.desc <- cdm$concept_ancestor %>%
  filter(ancestor_concept_id  %in% ip.codes ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

cohort_LC <- cdm[[OverlapCohortsIPName]] %>% 
  dplyr::filter(.data$cohort_definition_id %in% c(1:25))
cohort_LC <- cohort_LC %>%
  dplyr::full_join(
    cdm[[OverlapCohortsCName]] %>% 
      dplyr::filter(.data$cohort_definition_id == 5) %>%
      dplyr::mutate(cohort_definition_id = 27))

cohort_LC <- cohort_LC %>% 
  addEvent(cdm, HUCohortsName, 1, c(-365,-1), "last_icu", order = "last") %>%
  addEvent(cdm, HUCohortsName, 2, c(-365,-1), "last_vent", order = "last") %>%
  addEvent(cdm, HUCohortsName, 3, c(-365,-1), "last_trach", order = "last") %>%
  addEvent(cdm, HUCohortsName, 4, c(-365,-1), "last_ecmo", order = "last") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 1), c(-365,-1), "number_icu") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 2), c(-365,-1), "number_vent") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 3), c(-365,-1), "number_trach") %>%
  addNumberEvent(cdm, HUCohortsName, list(cohort_definition_id = 4), c(-365,-1), "number_ecmo") %>%
  addNumberVisit(cdm, 9202, c(-365,-1)) %>% 
  addEvent(cdm, "visit_occurrence", 9202, c(-365,-1), "last_GP", eventDate = "visit_start_date", order = "last") %>%
  compute()

cohort_LC <- cohort_LC %>% 
  dplyr::left_join(mydata %>% dplyr::select("subject_id", "cluster_assignment"), 
                   by = "subject_id", copy = TRUE) %>%
  compute()

HU_summary <- cohort_LC %>%
  dplyr::rename("number_GP" = "number_visit") %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cohort_definition_id) %>%
  dplyr::mutate(time_GP = !!CDMConnector::datediff("last_GP", "cohort_start_date")) %>%
  dplyr::mutate(time_icu = !!CDMConnector::datediff("last_icu", "cohort_start_date")) %>%
  dplyr::mutate(time_vent = !!CDMConnector::datediff("last_vent", "cohort_start_date")) %>%
  dplyr::mutate(time_trach = !!CDMConnector::datediff("last_trach", "cohort_start_date")) %>%
  dplyr::mutate(time_ecmo = !!CDMConnector::datediff("last_ecmo", "cohort_start_date")) %>%
  dplyr::select(dplyr::starts_with(c("number","time"))) %>%
  dplyr::summarise(across(everything(), list(mean = mean, var = var, sum = sum))) %>%
  dplyr::arrange(cohort_definition_id) %>%
  compute()

write.csv(
  HU_summary,
  file = here::here(output_clustering, paste0("Cluster_healthcare_Utilisation_noHosp.csv")),
  row.names = FALSE
)

cohort_LC <- cohort_LC %>% dplyr::select(-dplyr::contains(c("time", "number", "last"))) %>%
  addNumberVisit(cdm, ip.codes.w.desc, c(-365,-1)) %>% 
  addEvent(cdm, "visit_occurrence", ip.codes.w.desc, c(-365,-1), "last_hosp", eventDate = "visit_start_date", order = "last") %>%
  compute()

cohort_LC <- cohort_LC %>% 
  dplyr::left_join(mydata %>% dplyr::select(subject_id, cluster_assignment), 
                   by = "subject_id", copy = TRUE) %>%
  compute()

if(all(c("number_visit", "last_hosp") %in% colnames(cohort_LC))) {
  HU_hosp_summary <- cohort_LC %>%
    dplyr::rename("number_hosp" = "number_visit") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cohort_definition_id) %>%
    mutate(time_visit = !!CDMConnector::datediff("last_hosp", "cohort_start_date")) %>%
    dplyr::select(dplyr::starts_with(c("number","time"))) %>%
    dplyr::summarise(across(everything(), list(mean = mean, var = var, sum = sum))) %>%
    dplyr::arrange(cohort_definition_id) %>%
    compute()
  # K is it a cbind?
  HU_summary_final <- cbind(HU_summary,HU_hosp_summary)
} else {
  HU_summary_final <- HU_summary
}

write.csv(
  HU_summary_final,
  file = here::here(output_clustering, paste0("Cluster_healthcare_Utilisation.csv")),
  row.names = FALSE
)


# Get nice plots for all models too
for(k in c(2:7)) {
  lc <- LCA_models[[k-1]] 
  # selected according to previous plot, for now we get the one with the lowest bic. Should do it consciously in April
  
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
  
  # Include prevalence average lines
  for (i in 1:length(factors)) {
    zp1 <- zp1 + geom_segment(x = (i - 0.5), y = factors[[i]], xend = (i + 0.5), yend = factors[[i]])
  }
  ggsave(here::here(output_clustering, paste0("Clustering_LCA_",k,"_clusters.jpg")))
  
}

# -------------------------------------------------------------------------------------------
# NETWORK VISUALISATION AND COMMUNITY DETECTION
info(logger, '-- Visualising network and looking at community detection')

# Number of people with each symptom
number_people <- data_LCA %>% dplyr::select(-c(subject_id,age,sex)) %>% 
  summarise(across(,sum)) %>%  unlist(., use.names=FALSE)

# HERE I DO PHI COEFFICIENT (MAKES SENSE FOR BINATY VARIABLES), EVEN THOUGH WE CANNOT ACCOUNT FOR OTHER COVARIATES AT THE SAME TIME
# Could also use other distance measures for dichotomous variables like https://docs.scipy.org/doc/scipy/reference/spatial.distance.html
data_network <- mydata[,2:(ncol(mydata)-3)]
phi_matrix <- matrix(NA,nrow = length(names_symptoms), ncol = length(names_symptoms))
colnames(phi_matrix) <- names_symptoms
rownames(phi_matrix) <- names_symptoms
for(i in 1:length(names_symptoms)) {
  for(j in 1:length(names_symptoms)) {
    if(i == j) phi_matrix[i,j] = 1
    else {
      phi_matrix[i,j] = data_network[,c(i,j)] %>% table() %>% 
        psych::phi(digits = 3)
      phi_matrix[j,i] = phi_matrix[i,j]
      # mydata[,c(i,j)] %>% spicy::distance_measure_of_choice()
    }
  }
}

# Define tolerance under which neglect correlation
# It is quite common to look at (-0,2,0.2) for significant correlation, can be tuned for better visualisation
tol <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

for(t in tol) {
  phi_test <- phi_matrix
  phi_test <- abs(phi_test) 
  phi_test[phi_test < t] <- 0 
  diag(phi_test) <- 0 # We don't care about obvious correlation of each variable with itself
  
  # Build network with nodes symptoms, edges phi coefficient using igraph package
  ig <- graph.adjacency(phi_test, mode="undirected", weighted=TRUE)
  
  # Tune this in a way that is visually beautiful and related to the value of people affected / strength of the association
  V(ig)$size <- log(number_people)
  E(ig)$width <- 10*E(ig)$weight
  
  # https://kateto.net/netscix2016.html # Visualisation ideas
  plot(ig, vertex.label.dist = 2, vertex.label.cex = 0.8)
  # layout_in_circle(ig, vertex.label.dist = 2, vertex.label.cex = 0.8)
  
  png(here::here(output_clustering, paste0("Network_",t,".png")), res = 150, height = 800, width = 1100)
  plot(ig, vertex.label.dist = 2, vertex.label.cex = 0.8)
  dev.off()
}

# Community detection: can also tune the tolerance in the future
phi_CM <- abs(phi_matrix)
phi_CM[phi_CM < 0.05] <- 0
diag(phi_CM) <- 0

# Which one to keep?
ig_full <- graph.adjacency(phi_CM, mode="undirected", weighted=TRUE)

# Random walk based
c1 <- igraph::cluster_walktrap(ig_full, weights = E(ig_full)$weight)
c2 <- igraph::cluster_walktrap(ig_full, weights = E(ig_full)$weight, steps = 2)
c3 <- igraph::cluster_walktrap(ig_full, weights = E(ig_full)$weight, steps = 10)
# Modularity fast greedy
c4 <- igraph::cluster_fast_greedy(ig_full, weights = E(ig_full)$weight)
# Infomap
c5 <- igraph::cluster_infomap(ig_full, e.weights = E(ig_full)$weight, v.weights = number_people)
c6 <- igraph::cluster_infomap(ig_full, e.weights = E(ig_full)$weight)
# Louvain, with resolution = 1 DEFAULT it is Modularity typical
c7 <- igraph::cluster_louvain(ig_full, weights = E(ig_full)$weight)
c8 <- igraph::cluster_louvain(ig_full, weights = E(ig_full)$weight, resolution = 1.4)
c9 <- igraph::cluster_louvain(ig_full, weights = E(ig_full)$weight, resolution = 0.6)

# Choose which ones to look at together, or to output (after GOLD)
plot(c1,ig_full)
plot(c2,ig_full)
plot(c3,ig_full)
plot(c4,ig_full)
plot(c5,ig_full)
plot(c6,ig_full)
plot(c7,ig_full)
plot(c8,ig_full)
plot(c9,ig_full)

# Difference in methods
differences_cm <- matrix(rep(NA,9*9), ncol = 9)
colnames(differences_cm) <- c("RW_4", "RW_2","RW_10","FastGreedy","Infomap","Infomap_noweightsnodes","Modularity","Louvain_1.4","Louvain_0.6")
rownames(differences_cm) <- colnames(differences_cm)
for (i in 1:9) {
  for(j in 1:9) {
    model1 <- paste0("c",i)
    model2 <- paste0("c",j)
    differences_cm[i,j] <- igraph::compare(eval(as.symbol(model1)),eval(as.symbol(model2)), method = "nmi")
  }
}
modularity <- c(rep(NA,9))
for(i in 1:9) {
  model <- paste0("c",i)
  modularity[i] <- igraph::modularity(eval(as.symbol(model)))
}

# Let's focus on c7, typical modularity community algorithm
membership(c7)
communities(c7)

png(here::here(output_clustering, "Modularity_CM.png"), res = 150, height = 800, width = 1100)
plot(c7,ig_full)
dev.off()

capture.output(communities(c7), file = here::here(output_clustering, "Communities_CM.txt"))



