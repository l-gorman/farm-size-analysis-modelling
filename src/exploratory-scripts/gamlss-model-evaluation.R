
library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
base_path <- "./"
# base_output_path <- paste0(base_path, "outputs/","gamlss_test/")
base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

# base_path <- "./"
# base_output_path <- paste0(base_path, "outputs/","gamlss_logno/")


final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))


load(paste0(base_output_path, "flat_model.rda"))
load(paste0(base_output_path, "location_model.rda"))
load(paste0(base_output_path, "location_scale_model.rda"))
# load(paste0(base_output_path, "location_scale_model_selection.rda"))

load(paste0(base_output_path, "location_scale_skew_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model_selection.rda"))

base_output_path <- "./outputs/non_linear_test/"
load(paste0(base_output_path, "location_scale_skew_kurtosis_model_selection.rda"))



land_cover_columns <-c("evergreen_needle_leaf",
                       "evergreen_broad_leaf",
                       "deciduous_needle_leaf",
                       "deciduous_broad_leaf",
                       "mixed_forest",
                       "closed_shrubland",
                       "open_shrubland",
                       "woody_savana",
                       "savana",
                       "grass_land",
                       "wetland",
                       "cropland",
                       "urban",
                       "crop_natural_mosaic",
                       "snow_ice",
                       "barren",
                       "water_bodies")

aez_columns <- c("level_2_aez_33_classes_tropics_lowland_sub_humid",
                 "level_2_aez_33_classes_tropics_highland_sub_humid",
                 "level_2_aez_33_classes_land_with_severe_soil_or_terrain_limitations",
                 "level_2_aez_33_classes_tropics_lowland_semi_arid",
                 "level_2_aez_33_classes_tropics_highland_semi_arid",
                 "level_2_aez_33_classes_dominantly_water",
                 "level_2_aez_33_classes_tropics_highland_humid",
                 "level_2_aez_33_classes_tropics_lowland_humid",
                 "level_2_aez_33_classes_dominantly_hydromorphic_soils",
                 "level_2_aez_33_classes_dominantly_very_steep_terrain",
                 "level_2_aez_33_classes_no_valid_pixels",
                 "level_2_aez_33_classes_land_with_ample_irrigated_soils",
                 "level_2_aez_33_classes_cold_no_permafrost_wet",
                 "level_2_aez_33_classes_dominantly_built_up_land",
                 "level_2_aez_33_classes_desert_or_arid_climate",
                 "level_2_aez_33_classes_cold_no_permafrost_moist",
                 "level_2_aez_33_classes_sub_tropics_warm_semi_arid")

x <- c("healthcare_traveltime",
       "nightlights",
       "population_density",
       
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "length_growing_season"
)

# x <- c(x,aez_columns, land_cover_columns)

area_model_comparison <- function(dataset, 
                                  location_information, 
                                  model = flat_model,
                                  model_type="Flat",
                                  mu=F,
                                  sigma=F,
                                  nu=F,
                                  tau=F,
                                  family = dBCPEo,
                                  title="",
                                  bin_width,
                                  bin_center,
                                  xlabel,
                                  ylabel,
                                  xlimit,
                                  ylimit,
                                  legend_title="Model Type",
                                  
                                  farm_size_column="farm_size_ha",
                                  x=c("healthcare_traveltime",
                                      "nightlights",
                                      "population_density",
                                      
                                      
                                      "elevation",
                                      "ndvi",
                                      "topographic_diversity",
                                      "length_growing_season"
                                  )
                                  
){
  
  # Intervals for Density function
  
  # Getting all of the information for this specific location.
  location_data <- dataset[dataset$ADM0_NAME==location_information$ADM0_NAME&
                             dataset$ADM1_NAME==location_information$ADM1_NAME  &
                             dataset$ADM2_NAME== location_information$ADM2_NAME,] 
  
  intervals <- seq(0.01,max(location_data$farm_size_ha),0.01)
  
  prediction <- predictAll(model,newdata = location_data[x])
  
  if ( mu==F&
       sigma==F&
       nu==F&
       tau==F){
    density_function <- family(intervals)  
  }
  
  if ( mu==T&
       sigma==F&
       nu==F&
       tau==F){
    density_function <- family(intervals,
                               mu = prediction$mu[1])
  }
  
  if ( mu==T&
       sigma==T&
       nu==F&
       tau==F){
    density_function <-family(intervals,
                              mu = prediction$mu[1], 
                              sigma = prediction$sigma[1])
  }
  
  if ( mu==T&
       sigma==T&
       nu==T&
       tau==F){
    density_function <-family(intervals,
                              mu = prediction$mu[1], 
                              sigma = prediction$sigma[1],
                              nu = prediction$nu[1])
  }
  
  if ( mu==T&
       sigma==T&
       nu==T&
       tau==T){
    density_function <-family(intervals,
                              mu = prediction$mu[1], 
                              sigma = prediction$sigma[1],
                              nu = prediction$nu[1],
                              tau = prediction$tau[1])
  }
  
  
  density_tibble<- tibble::as_tibble(
    list(
      model_type=model_type,
      quantile=intervals,
      density=density_function
    )
  )
  
  
  geometric_mean <- exp(mean(log(location_data[[farm_size_column]])))
  arithmetic_mean <- mean(location_data[[farm_size_column]])

  means_df <- tibble::as_tibble(list(
    "type_of_qverage"=c("Arithmetic Mean", "Geometric Mean"),
    "value"=c(arithmetic_mean,geometric_mean)
  ))
  # bin_width <- max(location_data$farm_size_ha)
  plot <- ggplot(location_data,aes_string(x=farm_size_column))+
    geom_histogram(aes(y=..density..), fill ="grey", color="black",binwidth = bin_width, center=bin_center)+
    # 
    geom_line(data=density_tibble, 
              aes(x=quantile,y=density),inherit.aes = F)+
    labs(
      title=title,
      x=xlabel,
      y=ylabel
      
    )+
    guides(color=guide_legend(title=legend_title))+
    xlim(xlimit)+
    ylim(ylimit)+
    geom_vline(data=means_df, aes(xintercept=value, color=type_of_qverage) )
    # geom_vline(xintercept = mean(geometric_mean, fill="blue",na.rm=T))
    
    # geom_vline(xintercept = median(location_data[[farm_size_column]], na.rm=T))
  
  plot
  
  return(plot) 
  
  
  
  
}

plot_area_distribution<- function(
    dataset,
    location_information,
    farm_size_column="farm_size_ha"){
  location_data <- dataset[dataset$ADM0_NAME==location_information$ADM0_NAME&
                             dataset$ADM1_NAME==location_information$ADM1_NAME  &
                             dataset$ADM2_NAME== location_information$ADM2_NAME,] 
  plot <- ggplot(location_data,aes_string(x=farm_size_column))+
    geom_histogram(aes(y=..density..), fill ="grey", color="black")
    plot
  
  return(plot) 
  
  
}




set.seed(404)
# row <- sample(1:nrow(final_df),1)

# area_counts <- table(final_df$geo_id) %>% as.data.frame() 
# area_counts <- area_counts[area_counts$Freq>200,]
# areasample <- sample(area_counts$Var1, 1)
# row <- which(final_df$geo_id==areasample)[1]
# area_counts <- area_counts[order(area_counts$Freq,decreasing = T),]


# admin_info <- final_df[row, c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]

# dataset <- final_df
# location_information <- admin_info
# farm_size_column="farm_size_ha"






area_counts <- table(final_df$geo_id) %>% as.data.frame() 
area_counts <- area_counts %>% merge(final_df[c("ADM0_NAME", "ADM1_NAME","ADM2_NAME","geo_id")],
                                     by.x = "Var1", by.y="geo_id", all.x = T, all.y = F, all = F)

area_counts <- area_counts[duplicated(area_counts)==F,]
area_counts <- area_counts[order(area_counts$Freq,decreasing = T),]

#[1] "Malawi"                      "Niger"                       "Ethiopia"                   
#[4] "United Republic of Tanzania" "Uganda"                      "Nigeria" 

# malwawi_max_geoid <- area_counts[area_counts$ADM0_NAME=="Malawi",][which.max( area_counts$Freq),]

malwawi_max_geoid <-  area_counts[area_counts$ADM0_NAME=="Malawi"&
                                    area_counts$ADM1_NAME=="Central Region"&
                                    area_counts$ADM2_NAME=="Lilongwe",]
plot_area_distribution(final_df,
                       malwawi_max_geoid)

ethiopia_max_geoid <-  area_counts[area_counts$ADM0_NAME=="Ethiopia"&
                                    area_counts$ADM1_NAME=="Amhara"&
                                    area_counts$ADM2_NAME=="West Gojam",]
plot_area_distribution(final_df,
                       ethiopia_max_geoid)


niger_max_geo_id <-  area_counts[area_counts$ADM0_NAME=="Niger"&
                                     area_counts$ADM1_NAME=="Maradi"&
                                     area_counts$ADM2_NAME=="Mayahi",]
plot_area_distribution(final_df,
                       niger_max_geo_id)

nigeria_max_geo_id <-  area_counts[area_counts$ADM0_NAME=="Nigeria"&
                                   area_counts$ADM1_NAME=="Bauchi"&
                                   area_counts$ADM2_NAME=="Ganjuwa",]
plot_area_distribution(final_df,
                       nigeria_max_geo_id)


area_counts[area_counts$ADM0_NAME=="Nigeria",c("Freq","ADM0_NAME","ADM1_NAME","ADM2_NAME")]

# niger_max_geoid <- area_counts[area_counts$ADM0_NAME=="Niger",][which.max( area_counts$Freq),]
# ethiopia_max_geoid <- area_counts[area_counts$ADM0_NAME=="Ethiopia",][which.max( area_counts$Freq),]
# tanzania_max_geoid <- area_counts[area_counts$ADM0_NAME=="United Republic of Tanzania",][which.max( area_counts$Freq),]
# uganda_max_geoid <- area_counts[area_counts$ADM0_NAME=="Uganda",][which.max( area_counts$Freq),]
# nigeria_max_geoid <- area_counts[area_counts$ADM0_NAME=="Nigeria",][which.max( area_counts$Freq),]


area_counts[area_counts$Var1=="79_1229_40804",]


admin_info <- final_df[row, c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]
admin_info <- admin_info[!duplicated(admin_info),]

dataset <- final_df
location_information <- admin_info
farm_size_column="farm_size_ha"

model <- location_scale_model_selection
model <- location_scale_skew_kurtosis_model_selection

family <- dBCTo

mu=T
sigma=T
nu=T
tau=T

title_base <- "Location Scale, Skew, Kurtosis Model Prediction (Box Cox T)\n"
# title_base <- "Location Scale Model Prediction (Lognormal Distribution)\n"
area_model_comparison(dataset=final_df, 
                      location_information=ethiopia_max_geoid, 
                      model = model,
                      model_type="Location Scale",
                      mu=mu,
                      sigma=sigma,
                      nu=nu,
                      tau=tau,
                      family = family,
                      title=paste0(title_base,ethiopia_max_geoid$ADM2_NAME,", ",ethiopia_max_geoid$ADM1_NAME,", ",ethiopia_max_geoid$ADM0_NAME),
                      bin_width=0.16,
                      bin_center=0.08,
                      ylimit=c(0,1),
                      xlimit=c(0,2.5),
                      xlabel="Farm Size (ha)",
                      ylabel="Density",
                      legend_title="Summary Statistics",
                      x=x
                      
)

area_model_comparison(dataset=final_df, 
                      location_information=malwawi_max_geoid, 
                      model = model,
                      model_type="Location Scale",
                      mu=mu,
                      sigma=sigma,
                      nu=nu,
                      tau=tau,
                      family = family,
                      title=paste0(title_base,malwawi_max_geoid$ADM2_NAME,", ",malwawi_max_geoid$ADM1_NAME,", ",malwawi_max_geoid$ADM0_NAME),
                      bin_width=0.25,
                      bin_center=0.125,
                      ylimit=c(0,1.2),
                      xlimit=c(0,4),
                      xlabel="Farm Size (ha)",
                      ylabel="Density",
                      legend_title="Summary Statistics",
                      x=x
                      
)

area_model_comparison(dataset=final_df, 
                      location_information =uganda_max_geoid, 
                      model = model,
                      model_type="Location Scale",
                      mu=mu,
                      sigma=sigma,
                      nu=nu,
                      tau=tau,
                      family = family,
                      title=paste0(title_base,uganda_max_geoid$ADM2_NAME,", ",uganda_max_geoid$ADM1_NAME,", ",uganda_max_geoid$ADM0_NAME),
                      bin_width=0.4,
                      bin_center=0.2,
                      ylimit=c(0,1.5),
                      xlimit=c(0,3),                      
                      xlabel="Farm Size (ha)",
                      ylabel="Density",
                      legend_title="Summary Statistics",
                      x=x
                      
)

area_model_comparison(dataset=final_df, 
                      location_information =niger_max_geo_id, 
                      model = model,
                      model_type="Location Scale",
                      mu=T,
                      sigma=T,
                      nu=F,
                      tau=F,
                      family = family,
                      title=paste0(title_base,niger_max_geo_id$ADM2_NAME,", ",niger_max_geo_id$ADM1_NAME,", ",niger_max_geo_id$ADM0_NAME),
                      bin_width=1,
                      bin_center=0.5,
                      ylimit=c(0,0.3),
                      xlimit=c(0,18),                      
                      xlabel="Farm Size (ha)",
                      ylabel="Density",
                      legend_title="Summary Statistics",
                      x=x
                      
)






# Plot real quantile
quantile_comparison <-function(dataset, 
                               quantile_to_plot=0.5,
                               model,
                               family,
                               title="",
                               xlabel="",
                               ylabel="",
                               ymax,
                               xmax,
                               mu=F,
                               sigma=F,
                               nu=F,
                               tau=F,
                               legend_title="Model Type",
                               
                               farm_size_column="farm_size_ha",
                               x=c("healthcare_traveltime",
                                   "nightlights",
                                   "population_density",
                                   
                                   
                                   "elevation",
                                   "ndvi",
                                   "topographic_diversity",
                                   "length_growing_season"
                               )
                               
){
  
  true_quantile_prediction <- dataset %>% 
    group_by(geo_id) %>% 
    summarise(
      sample_size=n(),
      true_value= quantile(farm_size_ha, probs=quantile_to_plot)
    )
  
  true_quantile_prediction <-true_quantile_prediction %>% merge(
    dataset[c("geo_id", "ADM0_NAME")],
    by="geo_id",
    all.x=T,
    all.y = T
  )
  true_quantile_prediction <- true_quantile_prediction %>% rename(Country=ADM0_NAME)
  

  
  
  location_prediction_data <- dataset[c("geo_id",x)]
  location_prediction_data <- location_prediction_data[duplicated(location_prediction_data)==F,]
  
  prediction <- predictAll(model,newdata = location_prediction_data[x])
  
  if ( mu==F&
       sigma==F&
       nu==F&
       tau==F){
    quantile_results <- family(quantile_to_plot)  
  }
  
  if ( mu==T&
       sigma==F&
       nu==F&
       tau==F){
    quantile_results <- family(quantile_to_plot,
                               mu = prediction$mu)
  }
  
  if ( mu==T&
       sigma==T&
       nu==F&
       tau==F){
    quantile_results <-family(quantile_to_plot,
                              mu = prediction$mu, 
                              sigma = prediction$sigma)
  }
  
  if ( mu==T&
       sigma==T&
       nu==T&
       tau==F){
    quantile_results <-family(quantile_to_plot,
                              mu = prediction$mu, 
                              sigma = prediction$sigma,
                              nu = prediction$nu)
  }
  
  if ( mu==T&
       sigma==T&
       nu==T&
       tau==T){
    quantile_results <-family(quantile_to_plot,
                              mu = prediction$mu, 
                              sigma = prediction$sigma,
                              nu = prediction$nu,
                              tau = prediction$tau)
  }
  
  
  quantile_tibble<- tibble::as_tibble(
    list(
      geo_id =location_prediction_data$geo_id,

      model_type=model_type,
      quantile=quantile_to_plot,
      quantile_prediction=quantile_results
    )
  )
  
  
  
  
  

  
  
  

  
 
  
  quantile_tibble <- quantile_tibble %>% merge(true_quantile_prediction,
                                                         by="geo_id", 
                                                         all.x=T,
                                                         all.y=F
  )
  plot <- ggplot(quantile_tibble, aes(x = true_value, y=quantile_prediction))+
    # geom_hex()+
    geom_point(shape = 21,aes(size=sample_size, fill=Country), color="black", )+
    geom_abline(slope = 1, intercept = 0)+
    scale_size_continuous(name="Sample Size")+
    ylim(0,ymax)+
    xlim(0,xmax)+
    labs(title=title,
         x=xlabel,
         y=ylabel)+
    theme(plot.title = element_text(hjust=0.5))
  plot
  
  fit <- lm(quantile_tibble,
            formula =true_value ~ quantile_prediction)
            # weights = sample_size)
  summary(fit)
  
  plot
  return(plot)
  
  
  
}



family <- qBCTo

 quantile_comparison(dataset = final_df, 
                    quantile_to_plot=0.5,
                    family=family,
                    model=model,
                    mu=mu,
                    sigma=sigma,
                    nu = nu,
                    tau = tau,
                    title="Predicting 50th percentile with BCT \nLocational,Shape, Scale, and Kurtosis Model",
                    xlabel="True Value",
                    ylabel="Predicted Quantile",
                    ymax=5,
                    xmax=5,
                   
                    farm_size_column = "farm_size_ha",
                
                    legend_title="Model Type",x = x)

quantile_comparison(dataset = final_df, 
                    quantile_to_plot=0.25,
                    family=family,
                    model=model,
                    mu=mu,
                    sigma=sigma,
                    nu = nu,
                    tau = tau,
                    title="Predicting 25th percentile with BCT \nLocational, Shape, Scale, and Kurtosis Model",
                    xlabel="True Value",
                    ylabel="Predicted Quantile",
                    ymax=5,
                    xmax=5,
                    
                    legend_title="Model Type",x = x)

quantile_comparison(dataset = final_df, 
                    quantile_to_plot=0.75,
                    family=family,
                    model=model,
                    mu=mu,
                    sigma=sigma,
                    nu = nu,
                    tau = tau,
                    title="Predicting 75th percentile with BCT \nLocational, Shape, Scale, and Kurtosis Model",
                    xlabel="True Value",
                    ylabel="Predicted Quantile",
                    ymax=5,
                    xmax=5,
                    
                    legend_title="Model Type",x = x)


