# sbatch src/bc-run-scripts/run_brms_anova_location_per_country.sh  -i 5000 -w 2000 -n 4 -o brms_anova_21_03_2023
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(magrittr)
library(optparse)
library(fastDummies)
library(projpred)
library(cmdstanr)


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
#   output="./outputs/continental_gaussian_location_test/",
#   ncores=4
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))


# function for running brms hierarchical model ----------------------------

run_model <- function(data,levels, quantile=NULL, sigma, iter, warmup,ncores ){
  
  
  
  levels_args <- paste0(levels,collapse="/")
  temp_formula <- paste0("log(land_cultivated_ha) ~ 1 + (1|",levels_args,")")
  
  
  if (sigma==F & is.null(quantile)){
    final_formula <- bf(temp_formula)
    family <- gaussian()
  }
  
  if (sigma==T & is.null(quantile)){
    sigma_formula <- paste0("sigma ~ 1 + (1|",levels_args,")")
    
    final_formula <- bf(temp_formula, sigma_formula)
    family <- gaussian()
    
    
  }
  
  if (!is.null(quantile)){
    if (quantile>0 & quantile < 1 & sigma==F){
      
      final_formula <- bf(temp_formula, quantile=quantile)
      family <- asym_laplace()
      
    }
  }
  
  
  
  if (!is.null(quantile)){
    if (quantile>0 & quantile < 1 & sigma==T){
      sigma_formula <- paste0("sigma ~ 1 + (1|",levels_args,")")
      
      final_formula <- bf(temp_formula, sigma_formula,quantile=quantile)
      family <- asym_laplace()
      
    }
  }
  
  
  result <- brms::brm(
    formula=final_formula,
    data = data,
    family=family,
    cores = ncores,
    warmup = warmup,
    iter=iter,
    control = list(adapt_delta = 0.95)
  )
  
  return(result)
}





# indicator_data <- readr::read_csv(paste0(opt$data,"/prepped-data/rhomis-ee-gaez.csv"))
indicator_data <- readr::read_csv(paste0(opt$data,"/prepped-data/rhomis-gaez-gdl.csv"))

# Removing null Land cultivated values
indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha<100,]
indicator_data$log_land_cultivated <-  log(indicator_data$land_cultivated_ha)

# Cleaning Travel time columns
travel_time_cols <- grep("travel_time", colnames(indicator_data), value=T)
min_travel_time <-  apply( indicator_data[travel_time_cols], 1, min)
indicator_data$min_travel_time <- min_travel_time
indicator_data <- indicator_data[indicator_data$min_travel_time!=0,]
indicator_data$log_min_travel_time <- log(indicator_data$min_travel_time)

#Cleaning HH_Size
indicator_data <- indicator_data[indicator_data$hh_size_mae!=0,]
indicator_data$log_hh_size <- log(indicator_data$hh_size_mae)

# Clean AEZ cols
aez_cols <- grep("AEZ", colnames(indicator_data),value=T)
aez_cols <- aez_cols[aez_cols!="AEZ_Classes_33"]
colnames(indicator_data)[colnames(indicator_data) %in% aez_cols] <- gsub("AEZ_Classes_33_","aez_",colnames(indicator_data)[colnames(indicator_data) %in% aez_cols])
aez_cols <- grep("aez_", colnames(indicator_data), value=T)


# Clean Education Columns -------------------------------------------------

education_conversions <- tribble(
  ~survey_value, ~conversion,
  "postsecondary","postsecondary",
  "college",   "postsecondary",
  "illiterate",   "no_school",
  "no_school",   "no_school",
  "none", "no_school",
  "enrolled_not_completed",   "enrolled_not_completed",
  "no_answer", NA,
  "primary","primary",
  "primary_1","primary",
  "primary_2","primary",
  "religious_school","religious_school",
  "islamic_school","religious_school",
  "koranic_school", "religious_school",
  "secondary","secondary",
  "secondary_1","secondary",
  "secondary_2","secondary",
  "literate","literate",
  "technical", "vocational",
  "vocational", "vocational",
  "adult_education",   "adult_education",
)

new_education_level <- indicator_data["head_education_level"]
new_education_level$index <- c(1:nrow(new_education_level))
new_education_level <- new_education_level %>% merge(education_conversions, by.x="head_education_level", by.y = "survey_value", all.x = T, all.y=F)
new_education_level <- new_education_level[order(new_education_level$index),]
row.names(new_education_level) <- new_education_level$index
indicator_data$education <- new_education_level$conversion
indicator_data <- indicator_data[!is.na(indicator_data$education),]








levels <- c("iso_country_code",
            "gdlcode",
            "village")





# indicator_data$hfias_numeric<- NA
# indicator_data$hfias_numeric[indicator_data$hfias_status=="severely_fi"] <- 1
# indicator_data$hfias_numeric[indicator_data$hfias_status=="moderately_fi"] <- 2
# indicator_data$hfias_numeric[indicator_data$hfias_status=="mildly_fi"] <- 3
# indicator_data$hfias_numeric[indicator_data$hfias_status=="food_secure"] <- 4

# land_categories <-  readr::read_csv(paste0(opt$data,"/prepped-data/land_cover_classes.csv"))
# 
# land_cat_columns <- paste0("land_cat_",c(1:17))
# indicator_data[land_cat_columns] <- lapply(indicator_data[land_cat_columns] , function(column){
#   column[is.na(column)] <- 0
#   # Dividing the number of pixels by total pixel count for that area
#   column <- column/indicator_data$pixelCount
#   return(column)
# }) %>% dplyr::bind_cols()
# 
# 
# new_land_cat_columns <- land_categories$Tag
# colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns
# # colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns
# 
# colnames(indicator_data)[colnames(indicator_data) == "accessibility_mean"] <-"healthcare_traveltime"
# colnames(indicator_data)[colnames(indicator_data) == "b1_mean_mean"] <- "nightlights"
# colnames(indicator_data)[colnames(indicator_data) == "population_density_mean_mean"] <- "population_density"
# 
# colnames(indicator_data)[colnames(indicator_data) == "elevation_mean"] <- "elevation"
# colnames(indicator_data)[colnames(indicator_data) == "NDVI_mean_mean"] <- "ndvi"
# colnames(indicator_data)[colnames(indicator_data) == "constant_mean"] <- "topographic_diversity"
# 
# 
# 
# indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
#                                 indicator_data$ADM1_CODE, "_",
#                                 indicator_data$ADM2_CODE)
# 





# level_combos <- list(
#   # c("ADM0_NAME"),
#   # c("ADM0_NAME","ADM1_CODE"),
#   # c("ADM0_NAME","ADM1_CODE","ADM2_CODE"),
#   # c("ADM0_NAME","ADM1_CODE","ADM2_CODE","village"),
#   
#   c("ADM2_CODE","village")
# )



###########################################################################################
###########################################################################################
###########################################################################################
# Gaussian Models Location Only
###########################################################################################
###########################################################################################
###########################################################################################

dir.create(paste0(opt$output,"/continental_gaussian_location/"))

dir.create(paste0(opt$output,"/continental_gaussian_location/per_country"))


country_codes <- unique(indicator_data$iso_country_code)
for (country in country_codes){
  temp_data <-indicator_data[indicator_data$iso_country_code==country,]
  dir.create(paste0(opt$output,"/continental_gaussian_location/per_country/",country,"/"))
  result <- run_model(temp_data,levels =  c("gdlcode","village"), sigma=F, iter=opt$iter, warmup=opt$warmup,ncores=opt$ncores)
  save(result,file=paste0(opt$output,"/continental_gaussian_location/per_country/",country,"/",paste0( c("ADM2_CODE","village"), collapse="_"),".rda"))
}




# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# d <- loadRData("outputs/brm_anov_31_01_2023/continental_gaussian_location/ADM0_NAME.rda")
# 



