library(brms)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggdist)
library(flextable)
library(tidybayes)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


vpc <- function(model, params){
  
  
  draws_df <-  as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}


summary_table <- function(model){
  
  
  model_summary <- summary(model)
  pop_effects <- model_summary$fixed %>% as_tibble()
  pop_effects$name <- "Population-Level Effects"
  pop_effects$type <- "Intercept"
  pop_effects$level <- "" 
  
  pop_effects <- pop_effects %>% relocate(level)
  pop_effects <- pop_effects %>% relocate(type)
  pop_effects <- pop_effects %>% relocate(name)
  
  
  sigma <- model_summary$spec_pars %>% as_tibble()
  sigma$name <- "Family Specific Parameters"
  sigma$type <- "Sigma"
  sigma$level <- ""
  
  sigma <- sigma %>% relocate(level)
  sigma <- sigma %>% relocate(type)
  sigma <- sigma %>% relocate(name)
  
  group_effects <- lapply(names(model_summary$random), function(x){
    temp <- as_tibble(model_summary$random[[x]])
    
    temp$name <- "Group-Level Effects"
    temp$type <- "sd(Intercept)" 
    temp$level <- x
    
    temp <- temp %>% relocate(level)
    temp <- temp %>% relocate(type)
    temp <- temp %>% relocate(name)
    temp 
    
  }) %>% bind_rows()
  
   result <- bind_rows(pop_effects,sigma,group_effects)
   
   columns_to_numberic <- c('Estimate','Est.Error',"l-95% CI","u-95% CI",'Rhat', 'Bulk_ESS', 'Tail_ESS')
   result[columns_to_numberic] <- result[columns_to_numberic] %>% mutate(across(everything(),round,2))
  return(result)
  
}

plot_vpc <- function(model,
                     params,
                     readable_params,
                     title,
                     subtitle
                     
){

  data <- model$data
  number_of_points <- nrow(data)
  
  
  mean_hfias <- round(mean(data$hfias_numeric),2)
  sd_hfias <- round(sd(data$hfias_numeric))
  
   
  
  
  
  
  number_of_villages <- length(unique(data$village))
  people_per_village <- data %>% 
    group_by(village) %>% summarise(number_of_people=n())
  mean_people_per_village <- round(mean(people_per_village$number_of_people),2)
  
  number_of_subnational_regions <- length(unique(data$ADM2_CODE))
  villages_per_subnational_region <- data %>% 
    group_by(ADM2_CODE, village) %>% summarise(number_of_people=n()) %>% ungroup() %>% 
    group_by(ADM2_CODE) %>% summarise(number_of_villages=n())
  mean_villages_per_county <- round(mean(villages_per_subnational_region$number_of_villages),2)
  

  
  brm_anov_vpc <-vpc( 
    model,
    params)
  
  # brm_anov_vpc <- brm_anov_vpc %>% 
  #   gather() %>% 
  #   group_by(key) %>% 
  #   summarise(
  #     l95ci=quantile(value, probs=c(0.05)),
  #     estimate=quantile(value, probs=c(0.5)),
  #     u95ci=quantile(value, probs=c(0.95))
  #   )
  
  brm_anov_vpc %>% 
    gather() %>%
    ggplot(aes(y = key, x = value)) +
    stat_halfeye(aes(fill = after_stat(level))) +
    scale_fill_brewer(na.translate = FALSE) +
    labs(y="", x="VPC",title = title,
         caption = paste0("Points=estimates, Thick lines=stder, Thin lines=95% CIs\n",
         "N_hhs=",number_of_points,", N_villages=",number_of_villages,", N_subregions=",number_of_subnational_regions,"\n",
        "avg_hhs_per_village=",mean_people_per_village,", avg_villages_per_subregion=",mean_villages_per_county,"\n",
        "mean_hfias=",mean_hfias,", sd_hfias=",sd_hfias,"\n",
        "hfias scale 1-4"))+
    scale_y_discrete(name="VPC",
                     breaks=params,
                     labels=readable_params)
  
  
  
  
}


# model_path <- 
#   params <-
#   readable_params <- 
#   levels <- "Level 1: Individual, Level 2: Village, Level 3: Subcounty, Level 4: Country"
# title <- 
  
model <- loadRData( "./outputs/continental_gaussian_location/continental_gaussian_location/hfias_ADM0_NAME_ADM2_CODE_village.rda")
temp <- plot_vpc(model =model,
           params =  c( "sd_ADM0_NAME__Intercept",
                        "sd_ADM0_NAME:ADM2_CODE__Intercept",
                        "sd_ADM0_NAME:ADM2_CODE:village__Intercept",
                        "sigma"),
           readable_params = c("Between Country", "Between Subcounty", "Between Village", "Unexplained"),
           title = "VPCs for HFIAS All Data")

dir.create("outputs/vpc_plots/all_data")
ggsave( "outputs/vpc_plots/all_data/vpc_plot.png",temp)

summary <- summary_table(model)
ft <- flextable(summary)
ft <- autofit(ft)

save_as_image(ft, path = "outputs/vpc_plots/all_data/table_fit_summary.png")




# Trace Plots
png(filename = "./outputs/vpc_plots/all_data/trace_plots.png")
plot(model)
dev.off()


get_variables(model)

png(filename = "./outputs/vpc_plots/all_data/mcmc_scatter.png",width = 1000,height = 500,units="px")
# bayesplot::mcmc_scatter(model,
#                         pars=c("sd_ADM0_NAME__Intercept",
#                                "sd_ADM0_NAME:ADM2_CODE__Intercept"))

print(bayesplot::mcmc_pairs(model,  regex_pars = "sd_|sigma",
                      off_diag_args = list(size = 1, alpha = 0.5)))

dev.off()


dir.create("outputs/vpc_plots")





all_countries <- list.dirs("./outputs/continental_gaussian_location/per_country")
all_countries <- all_countries[all_countries!="./outputs/continental_gaussian_location/per_country"]


for (country_dir in all_countries){
  
  country <- strsplit(country_dir, "/")
  country <- lapply(country, function(x){
    x[length(x)]
  }) %>% unlist()
  model <- loadRData(paste0("./outputs/continental_gaussian_location/per_country/",country,"/ADM2_CODE_village.rda"))
  
  dir.create(paste0("outputs/vpc_plots/",country))
  # Plot VPCs
  temp <- plot_vpc(model = model,
                   params =  c("sd_ADM2_CODE__Intercept",
                               "sd_ADM2_CODE:village__Intercept",
                               "sigma"),
                   readable_params = c("Between Subcounty", "Between Village", "Unexplained"),
                   title = paste0("VPCs for HFIAS Model ",country))
  ggsave(paste0("outputs/vpc_plots/",country,"/vpc_plot.png"),temp)
  temp <- NULL 
  
  # Summary Convergence Stats
  summary <- summary_table(model)
  ft <- flextable(summary)
  ft <- autofit(ft)
  
  save_as_image(ft, path = paste0("outputs/vpc_plots/",country,"/table_fit_summary.png"))
  
  

  
  # Trace Plots
  png(filename = paste0("./outputs/vpc_plots/",country,"/trace_plots.png"))
  plot(model)
  dev.off()
  
  png(filename = paste0("./outputs/vpc_plots/",country,"/mcmc_scatter.png"),width=1000,height=500,units="px")
  print(bayesplot::mcmc_pairs(model,  regex_pars = "sd_|sigma",
                        off_diag_args = list(size = 1, alpha = 0.5)))
  dev.off()
  
 
}


