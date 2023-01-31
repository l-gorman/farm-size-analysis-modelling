library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
base_path <- "./"
base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

base_output_path <- paste0(base_path, "outputs/","gamlss_test/")


final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))



fit_area_data <- function(
    df,
    family,
    admin_info
    
){
  subset_df <- df[
    df$ADM0_NAME==admin_info$ADM0_NAME&
      df$ADM1_NAME==admin_info$ADM1_NAME&
      df$ADM2_NAME==admin_info$ADM2_NAME,c(x,"farm_size_ha")]
  result <- tryCatch(
    {
      
      fit <- gamlss(farm_size_ha~1,
                    data=subset_df,
                    family = BCTo(),
                    control = gamlss.control(n.cyc = 200))
      parameter <- as.data.frame(summary(fit))
      
      return_df <- admin_info
      
      return_df$mu <- parameter$Estimate[1]
      return_df$sigma <- parameter$Estimate[2]
      return_df$nu <- parameter$Estimate[3]
      return_df$tau <- parameter$Estimate[4]
      return_df
    },error=function(err){
      print(err)
      
      return_df <- admin_info
      return_df$mu <- NA
      return_df$sigma <- NA
      return_df$nu <- NA
      return_df$tau <- NA
      return(return_df)
      
    }
    
  )
  
  
  return(result)
  
}

family <- BCPEo()

x=c("healthcare_traveltime",
    "nightlights",
    "population_density",
    
    
    "elevation",
    "ndvi",
    "topographic_diversity",
    "length_growing_season"
)
area_fits <- list()


combinations <- final_df[ c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]
combinations <- combinations[!duplicated(combinations),]


for (row in 1:nrow(combinations)){
  admin_info <- combinations[row, c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]
  area_fits[[row]] <- fit_area_data(df = final_df,
                                    family = family,
                                    admin_info=admin_info
  )
  
}


area_fits <- area_fits %>% dplyr::bind_rows()

prediction_df <- final_df[,c(x,"ADM0_NAME","ADM0_CODE", "ADM1_NAME","ADM1_CODE","ADM2_NAME","ADM2_CODE")]
table(duplicated(prediction_df))
prediction_df <- prediction_df[duplicated(prediction_df)==F,]
# prediction_location_scale_skew<- predictAll(location_scale_skew_kurtosis_model_selection,newdata = prediction_df[x])


hist(area_fits$mu)
