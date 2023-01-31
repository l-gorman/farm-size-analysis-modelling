library(readr)
library(dplyr)
library(tibble)
library(jsonlite)
library(tidyr)



# Graph Plotting
library(ggplot2)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(GGally)

# library(moments) # Package for skewness
library(gamlss) # Gaussian additive models for location, scale, and shape
# library(bamlss) # Bayesian additive models for location, scale, and shape
library(brms) # General package for bayesian models with lme4 syntax and stan backend
library(lme4) # General package for bayesian multi-level modelling.
library(FactoMineR) # Package for pca
library(factoextra) # Extra pca features

library(fitdistrplus)


indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$gps_lat) & !is.na(indicator_data$gps_lon),]
# indicator_data_geo <- st_as_sf(indicator_data, coords = c("gps_lon", "gps_lat"), 
#                                crs = 4326, agr = "constant", remove = F)



indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.na(indicator_data$AEZ_Classes_33),]

land_cat_columns <- paste0("land_cat_",c(1:17))
indicator_data[land_cat_columns] <- lapply(indicator_data[land_cat_columns] , function(column){
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/indicator_data$pixelCount
  return(column)
}) %>% dplyr::bind_cols()

fao_level_2[land_cat_columns] <- lapply(fao_level_2[land_cat_columns] , function(column){
  column <- as.numeric(column)
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/fao_level_2$pixelCount
  return(column)
}) %>% dplyr::bind_cols()


new_land_cat_columns <- land_categories$Tag
colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns
colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns

colnames(indicator_data)[colnames(indicator_data) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(indicator_data)[colnames(indicator_data) == "b1_mean_mean"] <- "nightlights"
colnames(indicator_data)[colnames(indicator_data) == "population_density_mean_mean"] <- "population_density"

colnames(indicator_data)[colnames(indicator_data) == "elevation_mean"] <- "elevation"
colnames(indicator_data)[colnames(indicator_data) == "NDVI_mean_mean"] <- "ndvi"
colnames(indicator_data)[colnames(indicator_data) == "constant_mean"] <- "topographic_diversity"

colnames(fao_level_2)[colnames(fao_level_2) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(fao_level_2)[colnames(fao_level_2) == "b1_mean_mean"] <- "nightlights"
colnames(fao_level_2)[colnames(fao_level_2) == "population_density_mean_mean"] <- "population_density"

colnames(fao_level_2)[colnames(fao_level_2) == "elevation_mean"] <- "elevation"
colnames(fao_level_2)[colnames(fao_level_2) == "NDVI_mean_mean"] <- "ndvi"
colnames(fao_level_2)[colnames(fao_level_2) == "constant_mean"] <- "topographic_diversity"

indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
                                indicator_data$ADM1_CODE, "_",
                                indicator_data$ADM2_CODE)



# Anova

indicator_data$log_land_cultivated_ha <- log(indicator_data$land_cultivated_ha)

indicator_data <- indicator_data[is.infinite(indicator_data$log_land_cultivated_ha)==F,]


res.aov <- aov(log_land_cultivated_ha ~ geo_id, data = indicator_data)
res.tukey <-TukeyHSD(res.aov)
res.tukey <- as.data.frame(res.tukey$geo_id)

table(res.tukey["p adj"]<0.05)

res.aov <- aov(log_land_cultivated_ha ~ iso_country_code, data = indicator_data)
res.tukey <-TukeyHSD(res.aov)
res.tukey <- as.data.frame(res.tukey$geo_id)


res.aov <- aov(log_land_cultivated_ha ~ geo_id, data = indicator_data[indicator_data$iso_country_code=="KE",])
summary(res.aov)
res.tukey <-TukeyHSD(res.aov)
res.tukey <- as.data.frame(res.tukey$geo_id)

table(res.tukey["p adj"]<0.05)


res.tukey$geo_id


