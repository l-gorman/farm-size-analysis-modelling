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

plot_vpc <- function(model,
                     params,
                     readable_params,
                     title,
                     subtitle
){
  
  data <- model$data
  number_of_points <- nrow(data)
  
  
  mean_land_cultivated_ha <- round(mean(data$land_cultivated_ha),2)
  sd_land_cultivated_ha <- round(sd(data$land_cultivated_ha))
  
  
  
  
  
  
  number_of_villages <- length(unique(data$village))
  people_per_village <- data %>% 
    group_by(village) %>% summarise(number_of_people=n())
  mean_people_per_village <- round(mean(people_per_village$number_of_people),2)
  
  number_of_subnational_regions <- length(unique(data$gdlcode))
  villages_per_subnational_region <- data %>% 
    group_by(gdlcode, village) %>% summarise(number_of_people=n()) %>% ungroup() %>% 
    group_by(gdlcode) %>% summarise(number_of_villages=n())
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
  
  caption <- paste0("Points=estimates, Thick lines=stder, Thin lines=95% CIs\n",
                    "N_hhs=",number_of_points,", N_villages=",number_of_villages,", N_subregions=",number_of_subnational_regions,"\n",
                    "avg_hhs_per_village=",mean_people_per_village,", avg_villages_per_subregion=",mean_villages_per_county,"\n",
                    "mean_farm_size=",mean_land_cultivated_ha,", sd_farm_size=",sd_land_cultivated_ha
  )
  
  brm_anov_vpc %>% 
    gather() %>%
    ggplot(aes(y = key, x = value)) +
    stat_halfeye(aes(fill = after_stat(level))) +
    scale_fill_brewer(na.translate = FALSE) +
    labs(y="", x="VPC",title = title,
         caption = caption)+
    scale_y_discrete(name="VPC",
                     breaks=params,
                     labels=readable_params)
  
  
  
  
}


plot_variables <- function(model, 
                           vars=NULL,
                           grep_pattern=NULL,
                           readable_params=NULL,
                           title,
                           all=F,
                           replaceable_string=NULL,
                           xlim){
  
  model_variables <- get_variables(model)
  
  if(!is.null(grep_pattern)&!is.null(vars)){
    stop("Choose grep or vars, not both")
  }
  
  if(is.null(grep_pattern)&is.null(vars) & is.null(all)){
    stop("Choose variables or variable pattern")
  }
  
  if(!is.null(vars)){
    params <- vars
    if (is.null(readable_params)){
      stop("Provide readable version of params")
    }
    
  }
  
  if (all==T){
    params <- c(
      grep("^sd_",model_variables,value=T),
      grep("^sigma",model_variables,value=T),
      grep("^b_",model_variables,value=T)
    )
    readable_params <- params
    
  }
  
  if(!is.null(grep_pattern)){
    params <- grep(grep_pattern,model_variables, value=T)
    readable_params <- gsub(paste0(".*",grep_pattern),"",params)
    
    if (!is.null(replaceable_string)){
      readable_params <- gsub(paste0(".*",replaceable_string),"",params)
      
    }
    
  }
  
  draws <- as_draws_df(model)[params]
  
  plot <- draws %>% 
    gather() %>%
    ggplot(aes(y = key, x = value)) +
    stat_halfeye(aes(fill = after_stat(level))) +
    scale_fill_brewer(na.translate = FALSE) +
    # labs(y="", x="Estimates",title = title)+
    scale_y_discrete(name="",
                     breaks=params,
                     labels=readable_params)+
    xlim(xlim)
  #
  return(plot)
}

ref_model <- loadRData("outputs/projpred_test/proj_pred/horseshoe_ref_model.rda")

# ref_model <- loadRData("outputs/21_03_2023/proj_pred/proj_pred_ref_model.rda")


# vpcs <- vpc(ref_model,c( "sd_iso_country_code__Intercept",
#                          "sd_iso_country_code:gdlcode__Intercept",
#                          "sd_iso_country_code:gdlcode:village__Intercept",
#                          "sigma"))
plot_variables(ref_model,
               all = T,
               title="...",
               xlim=c(-1,1.1))

plot_variables(ref_model, 
               vars=c( "sd_iso_country_code__Intercept",
                       "sd_iso_country_code:gdlcode__Intercept",
                       "sd_iso_country_code:gdlcode:village__Intercept",
                       "sigma"),
               grep_pattern=NULL,
               readable_params=c( "Between Country",
                                  "Between County",
                                  "Between Village",
                                  "Unexplained"),
               title="Estimates for ...",
               replaceable_string=NULL)


plot_variables(ref_model, 
               vars=NULL,
               grep_pattern="education",
               readable_params=NULL,
               title="Estimates for ...",
               replaceable_string="b_education_")


plot_variables(ref_model, 
               vars=NULL,
               grep_pattern="aez_",
               readable_params=NULL,
               title="Estimates for ...",
               replaceable_string="b_aez_")

# plot_variables(ref_model, 
#                vars=NULL,
#                grep_pattern="b_gdl",
#                readable_params=NULL,
#                title="Estimates for ...",
#                replaceable_string="b_gdl_")

plot_variables(ref_model, 
               vars=NULL,
               grep_pattern="gdl_",
               readable_params=NULL,
               title="Estimates for ...",
               replaceable_string="")


plot_variables(ref_model, 
               vars="log_hh_size",
               # grep_pattern="b_gdl",
               readable_params="Household Size",
               title="Estimates for ...",
               replaceable_string=NULL)

plot_variables(ref_model, 
               vars="adjusted_length_growing_period",
               # grep_pattern="b_gdl",
               readable_params="Length Growing Period",
               title="Estimates for ...",
               replaceable_string=NULL)

plot_variables(ref_model, 
               vars="b_log_min_travel_time",
               # grep_pattern="b_gdl",
               readable_params="Minimum Travel Time",
               title="Estimates for ...",
               replaceable_string=NULL)

plot_variables(ref_model,
               all=T)


get_variables(ref_model)

temp <- plot_vpc(model =ref_model,
                 params =  c( "sd_iso_country_code__Intercept",
                              "sd_iso_country_code:gdlcode__Intercept",
                              "sd_iso_country_code:gdlcode:village__Intercept",
                              "sigma"),
                 readable_params = c("Between Country", "Between Subcounty", "Between Village", "Unexplained"),
                 title = "VPCs for Farm Size All Data")

