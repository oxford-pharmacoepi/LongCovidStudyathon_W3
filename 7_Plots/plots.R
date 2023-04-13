################################################################################
# PROVISIONAL PLOTS FOR CHECKS PRIOR TO THE TIDYING UP IN THE STUDYATHON
################################################################################

# Think how to group better later on

# Output folder for WP1
output_ip <- file.path(tempDir,"IP")
if (!file.exists(output_ip)){
  dir.create(output_ip, recursive = TRUE)}

# Output folder for plots
output_plots <- file.path(tempDir,"Plots_IP")
if (!file.exists(output_plots)){
  dir.create(output_plots, recursive = TRUE)}

# Names of bases and groups of outcomes for WP1a
base_names_before <- c("Inf", "Reinf", "Neg", "Flu")
outcome_names_before <- c("LC", "PASC", "MC")

# All files in the IP output folder
IP_file_names <- list.files(path = here::here(output_ip))
IP_file_names_full <- list.files(path = here::here(output_ip), full.names = TRUE)

base_names <- c(0)
outcome_names <- c(0)

# Erase base_names and outcome_names that do not appear in file_names
for(name in base_names_before) {
  if(length(grep(name,IP_file_names)) != 0) base_names <- c(base_names, name)
}

for(name in outcome_names_before) {
  if(length(grep(name,IP_file_names)) != 0) outcome_names <- c(outcome_names, name)
}

base_names <- base_names[-1]
outcome_names <- outcome_names[-1]

################################################################################
# Start plot making

# Incidence: whole population, no strata
working_names <- list()
working_names[[1]] <- IP_file_names_full[grep("^Allpop_base.*\\AllandSex.zip$",IP_file_names)]
working_names[[2]] <- IP_file_names_full[grep("^Allpop_overlap_any.*\\AllandSex.zip$",IP_file_names)]
working_names[[3]] <- IP_file_names_full[grep("^Allpop_overlap_.*\\AllandSex.zip$",IP_file_names)]
working_names[[3]] <- setdiff(working_names[[3]],working_names[[2]])
# Possibly will have to break this in smaller bits

name_plot <- c("Allpop_base", "Allpop_overlap_any", "Allpop_overlap")

for(i in c(1:3)) {
  name_file_inc <- list.files(path = here::here(output_ip, working_names[[i]][1]), pattern = "incidence_estimates")
  unzip(working_names[[i]][1], exdir = output_plots)
  file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
  df <- read.csv(file_inc)
  unlink(here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), recursive=TRUE)
  
  for(j in c(1:length(working_names[[i]])-1)) {
    unzip(working_names[[i]][j+1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- rbind(df, read.csv(file_inc))
    unlink(here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), recursive=TRUE)
  }
  
  df <- as_tibble(df)
  df <- df %>% 
    dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
  
  inc_mnths_plot <- df %>%
    dplyr::filter(denominator_sex == "Both" &
             analysis_interval == "months") %>%
    as.data.frame()
  
  # Put names instead of ids for outcomes
  plotAll <- inc_mnths_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calendar year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  plotname <- paste0(name_plot[i], "_",db.name,"_month.pdf")
  
  pdf(here::here(output_plots, plotname),
      width = 7, height = 5)
  print(plotAll, newpage = FALSE)
  dev.off()
  
  inc_yrs_plot <- df %>%
    dplyr::filter(denominator_sex == "Both" &
                    analysis_interval == "years") %>%
    as.data.frame()
  
  # Put names instead of ids for outcomes
  plotAll <- inc_yrs_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calender year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  plotname <- paste0(name_plot[i], "_",db.name,"_year.pdf")
  
  pdf(here::here(output_plots, plotname),
      width = 7, height = 5)
  print(plotAll, newpage = FALSE)
  dev.off()
  
}

# Incidence: different base cohorts, no strata
for(b in base_names) {
  for(o in outcome_names) {
    working_names <- IP_file_names_full[grep(paste0("^",b,"_",o,".*\\AllandSex.zip$"),IP_file_names)]
    
    name_file_inc <- list.files(path = here::here(output_ip, working_names[1]), pattern = "incidence_estimates")
    unzip(working_names[1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), recursive=TRUE)
    
      for(j in c(1:length(working_names)-1)) {
        name_file_inc <- list.files(path = here::here(output_ip, working_names[j+1]), pattern = "incidence_estimates")
        unzip(working_names[j+1], exdir = output_plots)
        file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
        df <- rbind(df,read.csv(file_inc))
        unlink(here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), recursive=TRUE)
      }
    
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
      
      inc_mnths_plot <- df %>%
        dplyr::filter(denominator_sex == "Both" &
                        analysis_interval == "months") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotAll <- inc_mnths_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years")+
        labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_",db.name,"_month.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotAll, newpage = FALSE)
      dev.off()
      
      inc_yrs_plot <- df %>%
        dplyr::filter(denominator_sex == "Both" &
                        analysis_interval == "years") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotAll <- inc_yrs_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years") +
       labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_",db.name,"_year.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotAll, newpage = FALSE)
      dev.off()
      
    }
}

# Incidence: whole population, sex strata
working_names <- list()
working_names[[1]] <- IP_file_names_full[grep("^Allpop_base.*\\AllandSex.zip$",IP_file_names)]
working_names[[2]] <- IP_file_names_full[grep("^Allpop_overlap_any.*\\AllandSex.zip$",IP_file_names)]
working_names[[3]] <- IP_file_names_full[grep("^Allpop_overlap_.*\\AllandSex.zip$",IP_file_names)]
working_names[[3]] <- setdiff(working_names[[3]],working_names[[2]])
# Possibly will have to break this in smaller bits

name_plot <- c("Allpop_base_sex", "Allpop_overlap_any_sex", "Allpop_overlap_sex")

for(i in c(1:3)) {
  name_file_inc <- list.files(path = here::here(output_ip, working_names[[i]][1]), pattern = "incidence_estimates")
  unzip(working_names[[i]][1], exdir = output_plots)
  file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
  df <- read.csv(file_inc)
  unlink(here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), recursive=TRUE)
  
  for(j in c(1:length(working_names[[i]])-1)) {
    unzip(working_names[[i]][j+1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- rbind(df, read.csv(file_inc))
    unlink(here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), recursive=TRUE)
  }
  
  df <- as_tibble(df)
  df <- df %>% 
    dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
  
  inc_mnths_plot <- df %>%
    dplyr::filter(denominator_sex != "Both" &
                    analysis_interval == "months") %>%
    as.data.frame()
  
  # Put names instead of ids for outcomes
  plotSex <- inc_mnths_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calendar year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  plotSex <- plotSex + facet_wrap(~denominator_sex, scales="free_y") +
    theme(strip.background = element_rect(colour="black", fill=NA),
          panel.border = element_rect(fill = NA, color = "black"))
  
  plotname <- paste0(name_plot[i], "_sex_", db.name,"_month.pdf")
  
  pdf(here(output_plots, plotname),
      width = 7, height = 5)
  print(plotSex, newpage = FALSE)
  dev.off()
  
  inc_yrs_plot <- df %>%
    dplyr::filter(denominator_sex != "Both" &
                    analysis_interval == "years") %>%
    as.data.frame()
  
  plotSex <- inc_yrs_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calender year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  plotSex <- plotSex + facet_wrap(~denominator_sex, scales="free_y") +
    theme(strip.background = element_rect(colour="black", fill=NA),
          panel.border = element_rect(fill = NA, color = "black"))
  
  plotname <- paste0(name_plot[i], "_sex_", db.name,"_year.pdf")
  
  pdf(here(output_plots, plotname),
      width = 7, height = 5)
  print(plotSex, newpage = FALSE)
  dev.off()
  
}

# Incidence: different base cohorts, sex strata
for(b in base_names) {
  for(o in outcome_names) {
    working_names <- IP_file_names_full[grep(paste0("^",b,"_",o,".*\\AllandSex.zip$"),IP_file_names)]
    
    name_file_inc <- list.files(path = here::here(output_ip, working_names[1]), pattern = "incidence_estimates")
    unzip(working_names[1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), recursive=TRUE)
    
    for(j in c(1:length(working_names)-1)) {
      name_file_inc <- list.files(path = here::here(output_ip, working_names[j+1]), pattern = "incidence_estimates")
      unzip(working_names[j+1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- rbind(df,read.csv(file_inc))
      unlink(here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), recursive=TRUE)
    }
    
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
    
    inc_mnths_plot <- df %>%
      dplyr::filter(denominator_sex != "Both" &
                      analysis_interval == "months") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotSex <- inc_mnths_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotSex <- plotSex + facet_wrap(~denominator_sex, scales="free_y") +
      theme(strip.background = element_rect(colour="black", fill=NA),
            panel.border = element_rect(fill = NA, color = "black"))
    
    plotname <- paste0(b,"_",o,"_sex_",db.name,"_month.pdf")
    
    pdf(here::here(output_plots, plotname),
        width = 7, height = 5)
    print(plotSex, newpage = FALSE)
    dev.off()
    
    inc_yrs_plot <- df %>%
      dplyr::filter(denominator_sex != "Both" &
                      analysis_interval == "years") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotSex <- inc_yrs_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotSex <- plotSex + facet_wrap(~denominator_sex, scales="free_y") +
      theme(strip.background = element_rect(colour="black", fill=NA),
            panel.border = element_rect(fill = NA, color = "black"))
    
    plotname <- paste0(b,"_",o,"_sex_",db.name,"_year.pdf")
    
    pdf(here::here(output_plots, plotname),
        width = 7, height = 5)
    print(plotSex, newpage = FALSE)
    dev.off()
    
  }
}

# Incidence: whole population, age strata
working_names <- list()
working_names[[1]] <- IP_file_names_full[grep("^Allpop_base.*\\Age.zip$",IP_file_names)]
working_names[[2]] <- IP_file_names_full[grep("^Allpop_overlap_any.*\\Age.zip$",IP_file_names)]
working_names[[3]] <- IP_file_names_full[grep("^Allpop_overlap_.*\\Age.zip$",IP_file_names)]
working_names[[3]] <- setdiff(working_names[[3]],working_names[[2]])
# Possibly will have to break this in smaller bits

name_plot <- c("Allpop_base", "Allpop_overlap_any", "Allpop_overlap")

for(i in c(1:3)) {
  name_file_inc <- list.files(path = here::here(output_ip, working_names[[i]][1]), pattern = "incidence_estimates")
  unzip(working_names[[i]][1], exdir = output_plots)
  file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
  df <- read.csv(file_inc)
  unlink(here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), recursive=TRUE)
  
  for(j in c(1:length(working_names[[i]])-1)) {
    unzip(working_names[[i]][j+1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- rbind(df, read.csv(file_inc))
    unlink(here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), recursive=TRUE)
  }
  
  df <- as_tibble(df)
  df <- df %>% 
    dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
  
  inc_mnths_plot <- df %>%
    dplyr::filter(analysis_interval == "months") %>%
    as.data.frame()
  
  # Put names instead of ids for outcomes
  plotAge <- inc_mnths_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calender year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  agelabels <- c(
    `0;6` = "0-6 Years",
    `7;11` = "7-11 Years",
    `12;18` = "12-18 Years",
    `19;40` = "19-40 Years",
    `41;64` = "41-64 Years",
    `65;120` = "65-120 Years")
  
  plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
    theme(strip.background = element_rect(colour="black", fill=NA),
          panel.border = element_rect(fill = NA, color = "black"))
  
  plotname <- paste0(name_plot[i], "_age_", db.name,"_month.pdf")
  
  pdf(here(output_plots, plotname),
      width = 7, height = 5)
  print(plotAge, newpage = FALSE)
  dev.off()
  
  inc_yrs_plot <- df %>%
    dplyr::filter(analysis_interval == "years") %>%
    as.data.frame()
  
  # Put names instead of ids for outcomes
  plotAge <- inc_yrs_plot %>%
    ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
    geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
    geom_line(color = "black", size = 0.25) +
    geom_point(size = 2.5) +
    xlab("Calender year") +
    ylab("Incidence rate per 100000 person-years") +
    labs(colour = "Outcome") +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          panel.background = element_blank() ,
          axis.line = element_line(colour = "black", size = 0.6) ,
          panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"))
  
  plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
    theme(strip.background = element_rect(colour="black", fill=NA),
          panel.border = element_rect(fill = NA, color = "black"))
  
  plotname <- paste0(name_plot[i], "_age_", db.name,"_year.pdf")
  
  pdf(here(output_plots, plotname),
      width = 7, height = 5)
  print(plotAge, newpage = FALSE)
  dev.off()
  
}

# Incidence: different base cohorts, age strata
for(b in base_names) {
  for(o in outcome_names) {
    working_names <- IP_file_names_full[grep(paste0("^",b,"_",o,".*\\Age.zip$"),IP_file_names)]
    
    name_file_inc <- list.files(path = here::here(output_ip, working_names[1]), pattern = "incidence_estimates")
    unzip(working_names[1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), recursive=TRUE)
    
    for(j in c(1:length(working_names)-1)) {
      name_file_inc <- list.files(path = here::here(output_ip, working_names[j+1]), pattern = "incidence_estimates")
      unzip(working_names[j+1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- rbind(df,read.csv(file_inc))
      unlink(here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), recursive=TRUE)
    }
    
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
    
    inc_mnths_plot <- df %>%
      dplyr::filter(analysis_interval == "months") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotAge <- inc_mnths_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    agelabels <- c(
      `0;6` = "0-6 Years",
      `7;11` = "7-11 Years",
      `12;18` = "12-18 Years",
      `19;40` = "19-40 Years",
      `41;64` = "41-64 Years",
      `65;120` = "65-120 Years")
    
    plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
      theme(strip.background = element_rect(colour="black", fill=NA),
            panel.border = element_rect(fill = NA, color = "black"))
    
    plotname <- paste0(b,"_",o,"_age_",db.name,"_month.pdf")
    
    pdf(here::here(output_plots, plotname),
        width = 7, height = 5)
    print(plotAge, newpage = FALSE)
    dev.off()
    
    inc_yrs_plot <- df %>%
      dplyr::filter(analysis_interval == "years") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotAge <- inc_yrs_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotAge <- plotAge + facet_wrap(~denominator_age_group, scales="free_y", labeller=labeller(denominator_age_group = agelabels)) +
      theme(strip.background = element_rect(colour="black", fill=NA),
            panel.border = element_rect(fill = NA, color = "black"))
    
    plotname <- paste0(b,"_",o,"_age_",db.name,"_year.pdf")
    
    pdf(here::here(output_plots, plotname),
        width = 7, height = 5)
    print(plotAge, newpage = FALSE)
    dev.off()
    
  }
}

if(vaccine_data) {
  # Incidence: whole population, vaccination strata
  working_names <- list()
  working_names[[1]] <- IP_file_names_full[grep("^Allpop_base.*\\Vacc.zip",IP_file_names)]
  working_names[[2]] <- IP_file_names_full[grep("^Allpop_overlap_any.*\\Vacc.zip$",IP_file_names)]
  working_names[[3]] <- IP_file_names_full[grep("^Allpop_overlap_.*\\Vacc.zip$",IP_file_names)]
  working_names[[3]] <- setdiff(working_names[[3]],working_names[[2]])
  # Possibly will have to break this in smaller bits
  
  name_plot <- c("Allpop_base", "Allpop_overlap_any", "Allpop_overlap")
  
  for(i in c(1:3)) {
    name_file_inc <- list.files(path = here::here(output_ip, working_names[[i]][1]), pattern = "incidence_estimates")
    unzip(working_names[[i]][1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), recursive=TRUE)
    
    for(j in c(1:length(working_names[[i]])-1)) {
      unzip(working_names[[i]][j+1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- rbind(df, read.csv(file_inc))
      unlink(here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), recursive=TRUE)
    }
    
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
    
    inc_mnths_plot <- df %>%
      dplyr::filter(analysis_interval == "months") %>%
      as.data.frame()
    
    
    # Put names instead of ids for outcomes
    plotVacc <- inc_mnths_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotname <- paste0(name_plot[i], "_vacc_", db.name,"_month.pdf")
    
    pdf(here(output_plots, plotname),
        width = 7, height = 5)
    print(plotVacc, newpage = FALSE)
    dev.off()
    
    inc_yrs_plot <- df %>%
      dplyr::filter(analysis_interval == "years") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotVacc <- inc_yrs_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotname <- paste0(name_plot[i], "_vacc_", db.name,"_year.pdf")
    
    pdf(here(output_plots, plotname),
        width = 7, height = 5)
    print(plotVacc, newpage = FALSE)
    dev.off()
    
  }
  
  # Non vaccinated people
  working_names <- list()
  working_names[[1]] <- IP_file_names_full[grep("^Allpop_base.*\\NonVacc.zip$",IP_file_names)]
  working_names[[2]] <- IP_file_names_full[grep("^Allpop_overlap_any.*\\NonVacc.zip$",IP_file_names)]
  working_names[[3]] <- IP_file_names_full[grep("^Allpop_overlap_.*\\NonVacc.zip$",IP_file_names)]
  working_names[[3]] <- setdiff(working_names[[3]],working_names[[2]])
  # Possibly will have to break this in smaller bits
  
  name_plot <- c("Allpop_base", "Allpop_overlap_any", "Allpop_overlap")
  
  for(i in c(1:3)) {
    name_file_inc <- list.files(path = here::here(output_ip, working_names[[i]][1]), pattern = "incidence_estimates")
    unzip(working_names[[i]][1], exdir = output_plots)
    file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
    df <- read.csv(file_inc)
    unlink(here::here(output_plots, substr(basename(working_names[[i]][1]), 1, nchar(basename(working_names[[i]][1])) - 4)), recursive=TRUE)
    
    for(j in c(1:length(working_names[[i]])-1)) {
      unzip(working_names[[i]][j+1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- rbind(df, read.csv(file_inc))
      unlink(here::here(output_plots, substr(basename(working_names[[i]][j+1]), 1, nchar(basename(working_names[[i]][j+1])) - 4)), recursive=TRUE)
    }
    
    df <- as_tibble(df)
    df <- df %>% 
      dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
    
    inc_mnths_plot <- df %>%
      dplyr::filter(analysis_interval == "months") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotnonVacc <- inc_mnths_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotname <- paste0(name_plot[i], "_nonvacc_", db.name,"_month.pdf")
    
    pdf(here(output_plots, plotname),
        width = 7, height = 5)
    print(plotnonVacc, newpage = FALSE)
    dev.off()
    
    inc_yrs_plot <- df %>%
      dplyr::filter(analysis_interval == "years") %>%
      as.data.frame()
    
    # Put names instead of ids for outcomes
    plotnonVacc <- inc_yrs_plot %>%
      ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
      geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
      geom_line(color = "black", size = 0.25) +
      geom_point(size = 2.5) +
      xlab("Calender year") +
      ylab("Incidence rate per 100000 person-years") +
      labs(colour = "Outcome") +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.background = element_blank() ,
            axis.line = element_line(colour = "black", size = 0.6) ,
            panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
            legend.key = element_rect(fill = "transparent", colour = "transparent"))
    
    plotname <- paste0(name_plot[i], "_nonvacc_", db.name,"_year.pdf")
    
    pdf(here(output_plots, plotname),
        width = 7, height = 5)
    print(plotnonVacc, newpage = FALSE)
    dev.off()
    
  }
  
  # Incidence: different base cohorts, vaccination strata
  for(b in base_names) {
    for(o in outcome_names) {
      working_names <- IP_file_names_full[grep(paste0("^",b,"_",o,".*\\Vacc.zip$"),IP_file_names)]
      
      name_file_inc <- list.files(path = here::here(output_ip, working_names[1]), pattern = "incidence_estimates")
      unzip(working_names[1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- read.csv(file_inc)
      unlink(here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), recursive=TRUE)
      
      for(j in c(1:length(working_names)-1)) {
        name_file_inc <- list.files(path = here::here(output_ip, working_names[j+1]), pattern = "incidence_estimates")
        unzip(working_names[j+1], exdir = output_plots)
        file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
        df <- rbind(df,read.csv(file_inc))
        unlink(here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), recursive=TRUE)
      }
      
      df <- as_tibble(df)
      df <- df %>% 
        dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
      
      inc_mnths_plot <- df %>%
        dplyr::filter(analysis_interval == "months") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotVacc <- inc_mnths_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years") +
        labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_vacc_",db.name,"_month.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotVacc, newpage = FALSE)
      dev.off()
      
      inc_yrs_plot <- df %>%
        dplyr::filter(analysis_interval == "years") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotVacc <- inc_yrs_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years") +
        labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_vacc_",db.name,"_year.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotVacc, newpage = FALSE)
      dev.off()
      
    }
  }
  
  # Non vaccinated
  
  for(b in base_names) {
    for(o in outcome_names) {
      working_names <- IP_file_names_full[grep(paste0("^",b,"_",o,".*\\NonVacc.zip$"),IP_file_names)]
      
      name_file_inc <- list.files(path = here::here(output_ip, working_names[1]), pattern = "incidence_estimates")
      unzip(working_names[1], exdir = output_plots)
      file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
      df <- read.csv(file_inc)
      unlink(here::here(output_plots, substr(basename(working_names[1]), 1, nchar(basename(working_names[1])) - 4)), recursive=TRUE)
      
      for(j in c(1:length(working_names)-1)) {
        name_file_inc <- list.files(path = here::here(output_ip, working_names[j+1]), pattern = "incidence_estimates")
        unzip(working_names[j+1], exdir = output_plots)
        file_inc <- list.files(path = here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), pattern = "incidence_es*", full.names = TRUE)
        df <- rbind(df,read.csv(file_inc))
        unlink(here::here(output_plots, substr(basename(working_names[j+1]), 1, nchar(basename(working_names[j+1])) - 4)), recursive=TRUE)
      }
      
      df <- as_tibble(df)
      df <- df %>% 
        dplyr::mutate(outcome_cohort_id = as.factor(outcome_cohort_id))
      
      inc_mnths_plot <- df %>%
        dplyr::filter(analysis_interval == "months") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotnonVacc <- inc_mnths_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years") +
        labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_nonvacc_",db.name,"_month.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotnonVacc, newpage = FALSE)
      dev.off()
      
      inc_yrs_plot <- df %>%
        dplyr::filter(analysis_interval == "years") %>%
        as.data.frame()
      
      # Put names instead of ids for outcomes
      plotnonVacc <- inc_yrs_plot %>%
        ggplot( aes(x = incidence_start_date, y = incidence_100000_pys, group=outcome_cohort_id, color = outcome_cohort_id)) +
        geom_ribbon(aes(ymin = incidence_100000_pys_95CI_lower, ymax = incidence_100000_pys_95CI_upper, fill = outcome_cohort_id), alpha = .3, color = NA, show.legend = FALSE) +
        geom_line(color = "black", size = 0.25) +
        geom_point(size = 2.5) +
        xlab("Calender year") +
        ylab("Incidence rate per 100000 person-years") +
        labs(colour = "Outcome") +
        theme(axis.text.x = element_text(angle = 45, hjust=1),
              panel.background = element_blank() ,
              axis.line = element_line(colour = "black", size = 0.6) ,
              panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))
      
      plotname <- paste0(b,"_",o,"_nonvacc_",db.name,"_year.pdf")
      
      pdf(here::here(output_plots, plotname),
          width = 7, height = 5)
      print(plotnonVacc, newpage = FALSE)
      dev.off()
      
    }
  }
}
