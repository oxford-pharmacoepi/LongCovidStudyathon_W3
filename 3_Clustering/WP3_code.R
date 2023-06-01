# Clustering 

  cdm <- cdm_from_con(db, cdm_database_schema, write_schema = results_database_schema,
                    cohort_tables = c(clusterCohortName),
                    cdm_name = db.name)


names_final_cohorts <- read.csv(file.path(tempDir,paste0(db.name,"_cohorts.csv")))

# Output folders for WP3
output_clustering <- file.path(tempDir,"Clustering")
if (!file.exists(output_clustering)){
  dir.create(output_clustering, recursive = TRUE)}

# -------------------------------------------------------------------------------------------
# LCA CLUSTERING
# Helpful: https://statistics.ohlsen-web.de/latent-class-analysis-polca/
# Read this: https://www.john-uebersax.com/stat/faq.htm#work

info(logger, '-- Calculating LCA clustering')

names_symptoms <- Initial_cohorts$cohort_name[c(2:26,32)]
cohort_set <- tibble(
  ancestor_concept_id = c(
    434621, 317009, 443392, 201820, 321588, 316866, 4030518, 255573, 4182210
  ),
  cohort_definition_id = c(1:9),
  cohort_name = c(
    "autoimmune_disease", "asthma", "malignant_neoplastic_disease",
    "diabetes_mellitus", "heart_disease", "hypertensive_disorder",
    "renal_impairment", "copd", "dementia"
  )
)

# Use package polCA
# Fit latent class model
mydata <- cdm[[clusterCohortName]] %>%
  dplyr::select(subject_id, dplyr::all_of(names_symptoms), age, sex) %>% 
  dplyr::collect()
mydata[,2:(ncol(mydata)-2)] <- mydata[,2:(ncol(mydata)-2)] + 1 
# needed for poLCA, now 0 (no symptom) is 1, and 1 (symptom) is 2. Not do that in age or sex or subject_id

x_vars <- c("1", "age", "sex")

cols <- paste0("cbind(", paste(names_symptoms, collapse = ","), ")")

f <- with(mydata, as.formula(sprintf("%s ~ %s", cols, paste(x_vars, collapse = " + "))))

entropy <- function(p) sum(-p*log(p))

mydata$sumrow <- rowSums(mydata %>% dplyr::select(dplyr::all_of(names_symptoms)), na.rm = TRUE) - 26
results <- list()

# Run a sequence of models with 2-7 classes and print out the model with the lowest BIC
run_clustering <- function(numclust, numsymp, counter) {
  
  output_clustering_w <- file.path(tempDir,"Clustering",paste0(numclust,"_clust_",numsymp,"_symp"))
  if (!file.exists(output_clustering_w)){
    dir.create(output_clustering_w, recursive = TRUE)}
  
  
  # Get people with more than the required number of clusters
  working_data <- mydata %>% 
    dplyr::filter(sumrow >= numsymp) %>%
    computeQuery()
    
  if(working_data %>% tally() > 499) {
    lc <- poLCA_oxf(f, working_data, nclass=numclust, maxiter=2000, graphs = FALSE,
                tol=1e-5, na.rm=FALSE,  
                nrep=50, verbose=TRUE, calc.se=FALSE)
    
    # nrep, maxiter or the number of classes can be tuned later on if for some databases convergence or IC output does not look right
    
    # Create table with information criteria for the model
    error_prior <- entropy(lc$P) 
    error_post <- mean(apply(lc$posterior,1, entropy),na.rm = TRUE)
    
    df <- lc$posterior
    df[] <- t(apply(df, 1, function(x) replace(x, x != max(x, na.rm = TRUE), NA)))
    numerator <- apply(df,2,sum,na.rm=T)
    denominator <- ifelse(is.na(df),0,1)
    denominator <- apply(denominator,2,sum,na.rm=T)
    
    results[[counter]] <- tibble::tibble(Clust = numclust,
                                         Symp = numsymp,
                                         log_likelihood= lc$llik,
                                         df = lc$resid.df,
                                         BIC= lc$bic,
                                         ABIC=  (-2*lc$llik) + ((log((lc$N + 2)/24)) * lc$npar),
                                         CAIC = (-2*lc$llik) + lc$npar * (1 + log(lc$N)), 
                                         entropy = round(((error_prior-error_post) / error_prior),3),
                                         mean_posterior = numerator/denominator*100)
    
    
    write.csv(
      lc[["probs"]],
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_probs.csv")),
      row.names = FALSE
    )
    write.csv(
      lc[["P"]],
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_pclust.csv")),
      row.names = FALSE
    )
    write.csv(
      lc[["N"]],
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_N.csv")),
      row.names = FALSE
    )
    
    lcmodel <- reshape2::melt(lc$probs, level=2)
    lcmodel$L2 <- stringr::str_to_title(lcmodel$L2)
    
    # Get nice plot of class membership
    zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
    zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
    zp1 <- zp1 + facet_grid(Var1 ~ .) 
    zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Blues",labels = c("No symptom", "Symptom")) +theme_bw()
    zp1 <- zp1 + labs(x = "Symptoms",y=paste0("Prevalence symptoms in ", db.name), fill ="Response categories")	
    zp1 <- zp1 + theme( axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),                    
                        panel.grid.major.y=element_blank(),
                        axis.text.x=element_text(size=10, angle = 90, vjust = 0.5, hjust=1))
    zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
    
    # Include horizontal lines with Prevalence of each symptom in all patients
    # List prevalence values (range from 0 to 1)
    # Transform back (1,2) to (0,1) binary
    working_data[,2:(ncol(working_data)-3)] <- working_data[,2:(ncol(working_data)-3)] -1 
    factors <- (colSums(working_data[,2:(ncol(working_data)-3)]))/(length(working_data[[1]]))	
    factors <- as.matrix(factors)
    rownames(factors) <- names_symptoms
    factors <- factors[order(rownames(factors)),]
    
    write.csv(
      factors,
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_average.csv")),
      row.names = TRUE
    )
    
    factors <- factors[factors != 0]
    
    # Include prevalence average lines
    for (i in 1:length(factors)) {
      zp1 <- zp1 + geom_segment(x = (i - 0.5), y = factors[[i]], xend = (i + 0.5), yend = factors[[i]])
    }
    ggsave(here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_figure.jpg")))
    
    # Characterise clusters
    # Look at characterisation of clusters: age and sex
    working_data <- working_data %>% mutate(cluster_assignment = lc$predclass)
    clusters_age <- working_data %>%
      dplyr::group_by(cluster_assignment) %>%
      dplyr::summarise(median_age = median(age),
                       sd_age = sd(age))
    clusters_sex <- working_data %>%
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
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_age.csv")),
      row.names = FALSE
    )
    
    write.csv(
      clusters_sex,
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_sex.csv")),
      row.names = FALSE
    )
    
    # Look at number of people with symptom per cluster
    number_people <- working_data %>% dplyr::select(-c(age,sex,subject_id,sumrow)) 
    for(i in 1:numclust) {
      clust <- apply(number_people %>% dplyr::filter(cluster_assignment == i) %>% 
                       dplyr::select(-cluster_assignment), 2, sum)
      clust[clust < 5] <- NA

      write.csv(
        clust,
        file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_clust_",i,".csv")),
        row.names = TRUE
      )
    }

    working_data <- working_data %>%
      dplyr::left_join(cdm[[clusterCohortName]], by =c("subject_id"), copy = TRUE)
    
    # Healthcare utilisation outcomes  
    HU_summary <- working_data %>%
      dplyr::group_by(cluster_assignment) %>%
      dplyr::select(dplyr::starts_with(c("number"))) %>%
      dplyr::summarise(across(everything(), list(median = median, var = var, sum = sum), na.rm = TRUE)) %>%
      dplyr::arrange(cluster_assignment) %>%
      computeQuery()
    
    HU_sum1 <- HU_summary %>% 
      dplyr::select(!dplyr::ends_with("sum"))
    HU_sum2 <- HU_summary %>% 
      dplyr::select(dplyr::ends_with("sum"))
    HU_sum2[HU_sum2 < 5] <- NA
    HU_summary <- dplyr::tibble(
      HU_sum1,
      HU_sum2
    )
    
    write.csv(
      HU_summary,
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_HU.csv")),
      row.names = FALSE
    )
    
    # Comorbidities
    com_summary <- working_data %>%
      dplyr::select(cohort_set$cohort_name) %>% collect()
    clust_com <- working_data %>% 
      dplyr::select(cluster_assignment) %>% collect()
    com_summary[com_summary > 0] <- 1
    
    com_summary <- cbind(com_summary, clust_com) %>%
      group_by(cluster_assignment) %>%
      dplyr::summarise(across(everything(), list(sum = sum), na.rm = TRUE)) %>%
      dplyr::arrange(cluster_assignment) %>%
      computeQuery()
    
    com_sum1 <- com_summary %>% 
      dplyr::select(!dplyr::ends_with("sum"))
    com_sum2 <- com_summary %>% 
      dplyr::select(dplyr::ends_with("sum"))
    com_sum2[com_sum2 < 5] <- NA
    com_summary <- dplyr::tibble(
      com_sum1,
      com_sum2
    )
    
    write.csv(
      com_summary,
      file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_comorbidities.csv")),
      row.names = FALSE
    )
    
    # Vaccination status
    if(vaccine_data) {
      vacc_char <- working_data %>% 
        dplyr::mutate(dose = first_dose + second_dose + third_dose) %>%
        dplyr::select(subject_id,cohort_start_date,cohort_end_date,dose, cluster_assignment) %>% 
        dplyr::mutate(n = as.numeric(n)) %>% 
        dplyr::mutate(n = if_else(is.na(n), 0, n)) %>%
        dplyr::mutate(n = ifelse(n <= 5, NA, n)) %>% 
        computeQuery()
      
      vacc_counts <- vacc_char %>% 
        dplyr::group_by(cluster_assignment, dose) %>% tally() %>%
        computeQuery()
      
      write.csv(
        vacc_counts,
        file = here::here(output_clustering_w, paste0("Clustering_LCA_clust_",numclust,"_symp_",numsymp,"_vaccination.csv"))
      )
    }

  }
  return(results)
}

counter <- 1
for(nc in c(2:7)) { # 2 to 7 clusters
  for(ns in c(1:3)) { # 1 to 3 number of symptoms
    results <- run_clustering(nc,ns,counter)
    counter <- counter + 1
  }
}

results <- bind_rows(results)
results2 <- tidyr::gather(results,Criteria,Value,5:9)

# Plots with information criteria results for all models tried (one per # symptoms)
for(i in c(1:3)) {
  result_working <- results2 %>% dplyr::filter(Symp == i)
  
  fit.plot<-ggplot(result_working) + 
    geom_point(aes(x=Clust,y=Value),size=3) +
    geom_line(aes(Clust, Value, group = 1)) +
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
  
  ggsave(here::here(output_clustering, paste0("Clustering_LCA_symp_",i,"_IC.jpg")))
  
}

# Save table with all IC information
write.csv(
  results,
  file = here::here(output_clustering, paste0("Information_criteria.csv")),
  row.names = FALSE
)
