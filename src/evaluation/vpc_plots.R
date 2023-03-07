library(brms)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggdist)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


vpc <- function(model_path, params){
  model <- loadRData(model_path)
  
  
  draws_df <-  as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}

plot_vpc <- function(model_path,
                     params,
                     readable_params,
                     title,
                     subtitle
                     
){
  brm_anov_vpc <-vpc( 
    model_path,
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
         caption = "Points represent estimates.\nThicker lines represent standard error.\n Thin lines represent 95% credible intervals")+
    scale_y_discrete(name="VPC",
                     breaks=params,
                     labels=readable_params)
  
  
  
  
}


model_path <- 
  params <-
  readable_params <- 
  levels <- "Level 1: Individual, Level 2: Village, Level 3: Subcounty, Level 4: Country"
title <- 
  
  
  plot_vpc(model_path = "./outputs/continental_gaussian_location/continental_gaussian_location/ADM0_NAME_ADM2_CODE_village.rda",
           params =  c( "sd_ADM0_NAME__Intercept",
                        "sd_ADM0_NAME:ADM2_CODE__Intercept",
                        "sd_ADM0_NAME:ADM2_CODE:village__Intercept",
                        "sigma"),
           readable_params = c("Between Country", "Between Subcounty", "Between Village", "Unexplained"),
           title = "VPCs for HFIAS All Data")


dir.create("outputs/vpc_plots")



temp <- plot_vpc(model_path = "./outputs/continental_gaussian_location/continental_gaussian_location/per_country/BF/ADM2_CODE_village.rda",
                 params =  c("sd_ADM2_CODE__Intercept",
                             "sd_ADM2_CODE:village__Intercept",
                             "sigma"),
                 readable_params = c("Between Subcounty", "Between Village", "Unexplained"),
                 title = "VPCs for HFIAS Burkina_")
ggsave( "outputs/vpc_plots/all_data.png",temp)

all_countries <- list.dirs("./outputs/continental_gaussian_location/continental_gaussian_location/per_country")
all_countries <- all_countries[all_countries!="./outputs/continental_gaussian_location/continental_gaussian_location/per_country"]


for (country_dir in all_countries){
  
  country <- strsplit(country_dir, "/")
  country <- lapply(country, function(x){
    x[length(x)]
  }) %>% unlist()
  temp <- plot_vpc(model_path = paste0("./outputs/continental_gaussian_location/continental_gaussian_location/per_country/",country,"/ADM2_CODE_village.rda"),
                   params =  c("sd_ADM2_CODE__Intercept",
                               "sd_ADM2_CODE:village__Intercept",
                               "sigma"),
                   readable_params = c("Between Subcounty", "Between Village", "Unexplained"),
                   title = paste0("VPCs for HFIAS Model ",country))
  
  ggsave(paste0("outputs/vpc_plots/",country,".png"),temp)
 
  temp <- NULL 
}


brm_anov_vpc <-  vpc( 
  model,
  c( "sd_ADM0_NAME__Intercept",
     "sd_ADM0_NAME:ADM2_CODE__Intercept",
     "sd_ADM0_NAME:ADM2_CODE:village__Intercept",
     "sigma"))




model_name <- "ADM0_NAME_ADM2_CODE_village"








