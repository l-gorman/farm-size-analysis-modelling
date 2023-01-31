
library(brms)
library(ggplot2)
library(tidyr)
library(tibble)
library(optparse)
library(readr)


# option_list = list(
#   make_option(c("-i", "--iter"),  type='integer',
#               help="Iterations"),
#   # make_option(c("-n", "--number"),  type='integer',
#   #             help="Number of hhs to sample"),
#   make_option(c("-b", "--base"), type='character',
#               help="Base directory where files will be loaded from"),
#   make_option(c("-d", "--directory"), type='character',
#               help="The directory where the file will be saved"),
#   make_option(c("-c", "--ncores"), type='character',
#               help="The number of chains/cores")
# 
# )

# 
opt <- list(
  iter=2000,
  directory="./",
  base="./outputs/",
  ncores="4"
)





indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
# indicator_data_geo <- st_as_sf(indicator_data, coords = c("x_gps_latitude", "x_gps_longitude"), 
#                                crs = 4326, agr = "constant", remove = F)

indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
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



x <- c("healthcare_traveltime",
       "nightlights",
       "population_density",
       
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "adjusted_length_growing_period",
       
       
       
       
       
       new_land_cat_columns
)


formula_quant_reg <- as.formula(
  paste0("log(land_cultivated_ha) ~ ", 
         paste0(x, collapse = " + ")
         )
  )


brm_quant_reg <- brms::brm(
  formula=bf(formula_quant_reg, quantile = 0.1),
  prior = c(prior(normal(5, 5), class = "Intercept"), 
            # Prior guess of 20% of the terms are non-zero
            # prior(horseshoe(par_ratio = 2 / 8), class = "b"),
            prior(horseshoe(), class = "b"),
            prior(student_t(4, 0, 1), class = "sigma")), 
  data = indicator_data,
  family = asym_laplace(),
  cores = 4
  )

save(brm_quant_reg,file="./outputs/brms_quant_reg/quantile_reg_horseshoe.rda")







