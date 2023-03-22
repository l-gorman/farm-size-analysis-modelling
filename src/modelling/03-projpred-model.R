# sbatch src/bc-run-scripts/run_brms_model.sh  -s "03-projpred-model.R" -i 5000 -w 2000 -n 4 -o brms_anova_21_03_2023

library(projpred)
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(magrittr)
library(optparse)

option_list = list(
  make_option(c("-i", "--iter"),  type='integer',
              help="Iterations"),
  make_option(c("-w", "--warmup"),  type='integer',
              help="Warmup"),
  make_option(c("-d", "--data"), type='character',
              help="Directory where data will be loaded from"),
  make_option(c("-o", "--output"), type='character',
              help="The directory where results will be written"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores")
  
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
# 
# 
# opt <- list(
#   iter=20,
#   warmup=10,
#   data="./data/",
#   output="./outputs/projpred_test/",
#   ncores=4
# )

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))



# load in data ------------------------------------------------------------

indicator_data <- readr::read_csv(paste0(opt$data,"/prepped-data/rhomis-gaez-gdl.csv"))

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha<100,]

indicator_data <- indicator_data[!is.na(indicator_data$village),]

# indicator_data$total_income_ppp_per_year <- indicator_data$total_income_lcu_per_year/indicator_data$currency_conversion_lcu_to_ppp
# indicator_data$value_farm_products_consumed_ppp_per_hh_per_year <- indicator_data$value_farm_products_consumed_lcu_per_hh_per_year/indicator_data$currency_conversion_lcu_to_ppp
# 
# hist(indicator_data$total_income_ppp_per_year)
# quantile(indicator_data$total_income_ppp_per_year, probs=c(0.1,0.25,0.5,0.75,0.9),na.rm = T)
# quantile(indicator_data$value_farm_products_consumed_ppp_per_hh_per_year, probs=c(0.1,0.25,0.5,0.75,0.9), na.rm = T)
# quantile(indicator_data$c, probs=c(0.1,0.25,0.5,0.75,0.9), na.rm = T)

x_vars <- c(
  #Household Level,
  # "livestock_tlu",
  # "hh_size_mae",
  
  # Village Level
  "adjusted_length_growing_period",
  "AEZ_Classes_33_cold_no_permafrost_moist",                
  "AEZ_Classes_33_cold_no_permafrost_wet",                  
  "AEZ_Classes_33_desert_or_arid_climate",                  
  "AEZ_Classes_33_dominantly_built_up_land",                
  "AEZ_Classes_33_dominantly_hydromorphic_soils",           
  "AEZ_Classes_33_dominantly_very_steep_terrain",           
  "AEZ_Classes_33_dominantly_water",                        
  "AEZ_Classes_33_land_with_ample_irrigated_soils",         
  "AEZ_Classes_33_land_with_severe_soil_or_terrain_limitations",
  "AEZ_Classes_33_sub_tropics_cool_semi_arid",               
  "AEZ_Classes_33_sub_tropics_moderately_cool_semi_arid",    
  "AEZ_Classes_33_sub_tropics_moderately_cool_sub_humid",    
  "AEZ_Classes_33_sub_tropics_warm_humid",                   
  "AEZ_Classes_33_sub_tropics_warm_semi_arid",               
  "AEZ_Classes_33_temperate_cool_wet",                       
  "AEZ_Classes_33_tropics_highland_humid",                   
  "AEZ_Classes_33_tropics_highland_semi_arid",               
  "AEZ_Classes_33_tropics_highland_sub_humid",               
  "AEZ_Classes_33_tropics_lowland_humid",                    
  "AEZ_Classes_33_tropics_lowland_semi_arid",                
  "AEZ_Classes_33_tropics_lowland_sub_humid",
  
  #Subcounty Level
  "gdl_healthindex",
  "gdl_sgdi",
  "gdl_shdi",
  "gdl_lifexp"
)



levels <- c("iso_country_code",
            "gdlcode",
            "village")

colSums(is.na(indicator_data[x_vars]))


dir.create(paste0(opt$output,"/proj_pred/"))

bf_formula <- bf(paste0(
  "log(land_cultivated_ha) ~ ",
  paste0(x_vars, collapse = " + "),
  " + (1|iso_country_code/gdlcode/village)"
))

ref_model <- brm(
  formula = bf_formula,
  data = indicator_data,
  family = gaussian(),
  iter=opt$iter, 
  warmup=opt$warmup,
  cores=opt$ncores
  # control=list(adapt_delta=0.9)
)

save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_ref_model.rda"))

land_cultivated_varsel <- cv_varsel(get_refmodel(ref_model),
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 1)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_1.rda"))

land_cultivated_varsel <- cv_varsel(get_refmodel(ref_model),
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 2)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_2.rda"))

land_cultivated_varsel <- cv_varsel(get_refmodel(ref_model),
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 3)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_3.rda"))

land_cultivated_varsel <- cv_varsel(get_refmodel(ref_model),
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 4)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_4.rda"))



