# sbatch src/bc-run-scripts/run_projpred_ref_model.sh  -s "03-projpred-ref-model.R" -i 8000 -w 4000 -n 4 -o brms_anova_21_03_2023


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

opt <- list(
  iter=20,
  warmup=10,
  data="./data/",
  output="./outputs/projpred_test/",
  ncores=4
)

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


aez_cols <- grep("AEZ", colnames(indicator_data),value=T)
aez_cols <- aez_cols[aez_cols!="AEZ_Classes_33"]
colnames(indicator_data)[colnames(indicator_data) %in% aez_cols] <- gsub("AEZ_Classes_33_","aez_",colnames(indicator_data)[colnames(indicator_data) %in% aez_cols])
aez_cols <- grep("aez_", colnames(indicator_data), value=T)
# indicator_data$aez_col <- colnames(indicator_data[aez_cols])[max.col(indicator_data[aez_cols])]
# indicator_data$aez_col <- gsub("AEZ_Classes_33_","AEZ_Class_",indicator_data$aez_col)


travel_time_cols <- grep("travel_time", colnames(indicator_data), value=T)
min_travel_time <-  apply( indicator_data[travel_time_cols], 1, min)
indicator_data$min_travel_time <- min_travel_time

table(indicator_data$head_education_level)
table(is.na(indicator_data$head_education_level))




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
table(is.na(indicator_data$head_education_level))


indicator_data$head_education_level


new_education_level <- indicator_data["head_education_level"]
new_education_level$index <- c(1:nrow(new_education_level))

new_education_level <- new_education_level %>% merge(education_conversions, by.x="head_education_level", by.y = "survey_value", all.x = T, all.y=F)
new_education_level <- new_education_level[order(new_education_level$index),]
row.names(new_education_level) <- new_education_level$index

indicator_data$education <- new_education_level$conversion

indicator_data <- indicator_data[!is.na(indicator_data$education),]

indicator_data$education <- factor(indicator_data$education,
                                      levels=c("no_school",
                                               "primary",
                                               "secondary",
                                               "adult_education",
                                               "religious_school",
                                               "postsecondary",
                                               "vocational",
                                               "enrolled_not_completed",
                                               "literate"),
                                   ordered = F)

dummy_education <- fastDummies::dummy_cols(indicator_data$education)
dummy_education$.data <- NULL
colnames(dummy_education) <- gsub(".data","education",colnames(dummy_education))
indicator_data <- cbind(indicator_data,dummy_education) 
indicator_data$log_hh_size <- log(indicator_data$hh_size_mae)

indicator_data$log_min_travel_time <- indicator_data$min_travel_time 

indicator_data <- indicator_data[!is.na(indicator_data$log_hh_size),]
indicator_data <- indicator_data[!is.infinite(indicator_data$log_hh_size),]

indicator_data <- indicator_data[!is.na(indicator_data$log_min_travel_time),]
indicator_data <- indicator_data[!is.infinite(indicator_data$log_min_travel_time),]

x_vars <- c(
  #Household Level,
  colnames(dummy_education),
  "log_hh_size",
  # "household_type",
 
  
  # Village Level
  "adjusted_length_growing_period",
  aez_cols,
  "log_min_travel_time",
  
  #Subcounty Level
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
  cores=opt$ncores,
  backend = "cmdstanr"
  # cores=4,
  # control=list(adapt_delta=0.9)
)

save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_ref_model.rda"))

# ref_model <-get_refmodel(ref_model)
# 
# land_cultivated_varsel <- cv_varsel(ref_model,
#                                     method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 1)
# save(land_cultivated_varsel,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_1.rda"))

# land_cultivated_varsel <- cv_varsel(ref_model,
#                                     method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 2)
# save(land_cultivated_varsel,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_2.rda"))
# 
# land_cultivated_varsel <- cv_varsel(ref_model,
#                                     method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 3)
# save(land_cultivated_varsel,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_3.rda"))
# 
# land_cultivated_varsel <- cv_varsel(ref_model,
#                                     method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 4)
# save(land_cultivated_varsel,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_4.rda"))



