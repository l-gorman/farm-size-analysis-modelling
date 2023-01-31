
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)

n_cores <- 4

indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
# indicator_data_geo <- st_as_sf(indicator_data, coords = c("x_gps_latitude", "x_gps_longitude"), 
#                                crs = 4326, agr = "constant", remove = F)


indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]

indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]

land_categories <-  readr::read_csv("./data/prepped-data/land_cover_classes.csv")



land_cat_columns <- paste0("land_cat_",c(1:17))
indicator_data[land_cat_columns] <- lapply(indicator_data[land_cat_columns] , function(column){
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/indicator_data$pixelCount
  return(column)
}) %>% dplyr::bind_cols()




new_land_cat_columns <- land_categories$Tag
colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns
# colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns

colnames(indicator_data)[colnames(indicator_data) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(indicator_data)[colnames(indicator_data) == "b1_mean_mean"] <- "nightlights"
colnames(indicator_data)[colnames(indicator_data) == "population_density_mean_mean"] <- "population_density"

colnames(indicator_data)[colnames(indicator_data) == "elevation_mean"] <- "elevation"
colnames(indicator_data)[colnames(indicator_data) == "NDVI_mean_mean"] <- "ndvi"
colnames(indicator_data)[colnames(indicator_data) == "constant_mean"] <- "topographic_diversity"



indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
                                indicator_data$ADM1_CODE, "_",
                                indicator_data$ADM2_CODE)



# Checking Different Levels

# adm0_anova <- brms::brm(
#   formula = log(land_cultivated_ha)~(1|ADM0_NAME),
#   data = indicator_data,
#   cores = n_cores
# )
# save(adm0_anova,file="./outputs/brms_anova/multi_level_normal/adm0_anova.rda")

# adm0_adm_1_anova <- brms::brm(
#   formula = log(land_cultivated_ha)~(1|ADM0_NAME/ADM1_CODE),
#   data = indicator_data,
#   cores = n_cores
# )
# save(adm0_adm_1_anova,file="./outputs/brms_anova/multi_level_normal/adm0_adm_1_anova.rda")

# adm0_adm_1_adm_2_anova <- brms::brm(
#   formula = log(land_cultivated_ha)~(1|ADM0_NAME/ADM1_CODE/ADM2_CODE),
#   data = indicator_data,
#   cores = n_cores
# )
# save(adm0_adm_1_adm_2_anova,file="./outputs/brms_anova/multi_level_normal/adm0_adm_1_adm_2_anova.rda")

# adm0_adm_1_adm_2_village_anova <- brms::brm(
#   formula = log(land_cultivated_ha)~(1|ADM0_NAME/ADM1_CODE/ADM2_CODE/village),
#   data = indicator_data,
#   cores = n_cores
# )
# save(adm0_adm_1_adm_2_village_anova,file="./outputs/brms_anova/multi_level_normal/adm0_adm_1_adm_2_village_anova.rda")


# adm_0_adm_2_village_anova<- brms::brm(
#   formula = log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
#   data = indicator_data,
#   cores = n_cores
# )
# save(adm_0_adm_2_village_anova,file="./outputs/brms_anova/multi_level_normal/adm_0_adm_2_village_anova.rda")
# 


