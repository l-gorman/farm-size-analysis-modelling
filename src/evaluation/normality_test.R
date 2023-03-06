library(lme4)

opt <- list(
  iter=2000,
  warmup=1000,
  data="./data/",
  output="./outputs/gaussian_location/",
  ncores=4
)


indicator_data <- readr::read_csv(paste0(opt$data,"/prepped-data/rhomis-ee-gaez.csv"))

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]

land_categories <-  readr::read_csv(paste0(opt$data,"/prepped-data/land_cover_classes.csv"))

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






level_combos <- list(
  c("ADM0_NAME"),
  c("ADM0_NAME","ADM1_CODE"),
  c("ADM0_NAME","ADM1_CODE","ADM2_CODE"),
  c("ADM0_NAME","ADM1_CODE","ADM2_CODE","village"),
  
  c("ADM0_NAME","ADM2_CODE","village")
)

indicator_data$log_farm_size <- log(indicator_data$land_cultivated_ha)


model <- lme4::lmer(log_farm_size ~ (1|ADM0_NAME) + (1|ADM0_NAME:ADM2_CODE) +(1|ADM0_NAME:ADM2_CODE:village),data = indicator_data)

plot(model)
summary(model)
indicator_data  %>%
  ggplot(aes(sample = log_farm_size)) +
  geom_qq() + geom_qq_line() +
  facet_wrap(~iso_country_code, scales = "free_y")


