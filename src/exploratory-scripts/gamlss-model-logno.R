#' See here for thorough description
#' of GAMLSS method and the 
#' fitting algorithm
#' 
#' https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1467-9876.2005.00510.x
#' 
#' See pg 59 here for a list of
#' the continuous distributions 
#' available under gamlss
#' 
#' https://www.gamlss.com/wp-content/uploads/2018/01/DistributionsForModellingLocationScaleandShape.pdf
#' 
#' See here for information on model selection with GAMLSS
#' 
#' http://www.gamlss.com/wp-content/uploads/2018/01/Model-Selection.pdf
#' 
#' Paper on Model Comparison here
#' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8073334/

library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)


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
#   
#   
#   
# )
# 
# opt_parser = OptionParser(option_list=option_list);
# opt = parse_args(opt_parser);

# 
opt <- list(
  iter=2000,
  directory="gamlss_logno",
  base="./",
  ncores="4"
)

main_folder <- paste0(opt$base,"outputs/",opt$directory)
dir.create(main_folder, showWarnings = F)

final_df <- readr::read_csv(paste0(opt$base,"prepared-data/final-modelling-dataset.csv"))

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

x <- c(x, land_cover_columns, aez_columns)

colSums(is.na(final_df[land_cover_columns]))


# Rescaling  --------------------------------------------------------------

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (column in c(x)){
  if (column %in% aez_columns==F & column %in% land_cover_columns)
  final_df[[column]]<- range01(final_df[[column]])
  
}

table(complete.cases(final_df))
colSums(is.na(final_df))

# Testing with Skew Normal Model ------------------------------------------

params <- paste0(" ~ ", paste0(c(x),collapse=" + "))

formula <- as.formula(params)
y_formula <- as.formula(paste0("farm_size_ha ",params))


flat_model <- gamlss(farm_size_ha ~1, 
                     sigma.formula = ~1, 
                     nu.formula = ~1, 
                     tau.formula = ~1,
                     data =final_df[,c(x,"farm_size_ha")],  
                     family=gamlss.dist::LOGNO(),
                     control = gamlss.control(n.cyc = opt$iter)
)


# plot(flat_model)
# summary(flat_model)

save(flat_model, file = paste0(main_folder,"/flat_model.rda"))


location_model <- gamlss(y_formula, 
                         sigma.formula =~1, 
                         nu.formula = ~1, 
                         tau.formula = ~1,
                         data =final_df[,c(x,"farm_size_ha")],  
                         family=gamlss.dist::LOGNO(),
                         control = gamlss.control(n.cyc = opt$iter)
)





# plot(location_model)
# summary(location_model)

save(location_model, file = paste0(main_folder,"/location_model.rda"))

location_scale_model <- gamlss(y_formula, 
                               sigma.formula = formula, 
                               nu.formula = ~1, 
                               tau.formula = ~1,
                               data =final_df[,c(x,"farm_size_ha")],  
                               family=gamlss.dist::LOGNO(),
                               control = gamlss.control(n.cyc = opt$iter)
)


# plot(location_scale_model)
# summary(location_scale_model)
save(location_scale_model, file = paste0(main_folder,"/location_scale_model.rda"))



# Saving Model Comparison ----------------------------------------------------------

location_scale_model_selection <- gamlss::stepGAICAll.A(flat_model,
                                                                      parallel = "multicore",
                                                                      ncpus = 7,
                                                                      scope=list(lower=~1,upper=formula))

save(location_scale_model_selection, file = paste0(main_folder,"/location_scale_model_selection.rda"))

print("Complete")












