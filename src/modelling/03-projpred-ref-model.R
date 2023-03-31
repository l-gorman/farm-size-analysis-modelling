# sbatch src/bc-run-scripts/run_projpred_ref_model.sh  -s "03-projpred-ref-model.R" -i 15000 -w 7500 -n 4 -o brms_anova_21_03_2023


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


cmdstanr::set_cmdstan_path()
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

# opt <- list(
#   iter=4000,
#   warmup=2000,
#   data="./data/",
#   output="./outputs/projpred_test/",
#   ncores=4
# )

options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading
# cmdstanr::set_cmdstan_path("/user/home/lg14410/.cmdstan/cmdstan-2.31.0")

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))



# load and clean data ------------------------------------------------------------


# Read in data and remove null rows
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



dummy_education <- fastDummies::dummy_cols(indicator_data$education)
dummy_education$.data <- NULL
colnames(dummy_education) <- gsub(".data","education",colnames(dummy_education))
indicator_data <- cbind(indicator_data,dummy_education)
education_cols <- colnames(dummy_education) 




# Rescaling variables -----------------------------------------------------

# Rescale variables to have mean zero and standard deviation of 0.5
rescale <- function(x){
  mean_x <- mean(x)
  sd <- sd(x)
  
  new_mean <- 0
  new_sd <- 0.5
  rescaled <- (x- mean_x)*(new_sd/sd)
  return(rescaled)
}




continuous_vars <- c("log_min_travel_time",
  "gdl_lifexp",
  "gdl_shdi",
  "log_hh_size",
  "adjusted_length_growing_period")

categorical_vars <- c(education_cols,aez_cols)

rescaled_continuous <- sapply(continuous_vars, function(x){
  rescale(indicator_data[[x]])
}, simplify = F) %>% bind_cols()

colnames(rescaled_continuous) <- paste0("rescaled_", colnames(rescaled_continuous))
indicator_data <- cbind(indicator_data,rescaled_continuous)
continuous_vars <- colnames(rescaled_continuous)



x_vars <- c(continuous_vars,categorical_vars)


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

# Setting Priors ----------------------------------------------------------

# Discussion on prior choice
horseshoe_priors <- c(
  set_prior("horseshoe(1)", class="b"),# HorseShoe
  
  # set_prior("normal(0, 10)", class = "b"),# Weakly informative
  set_prior('normal(0, 10)', class = 'sd'),
  
  set_prior('normal(0, 10)', class = 'sigma'),
  set_prior('normal(0, 10)', class = 'Intercept')
  
)

weak_priors <- c(
  # set_prior("horseshoe(1)", class="b"),# HorseShoe
  
  set_prior("normal(0, 10)", class = "b"),# Weakly informative
  set_prior('normal(0, 10)', class = 'sd'),
  
  set_prior('normal(0, 10)', class = 'sigma'),
  set_prior('normal(0, 10)', class = 'Intercept')
  
)




# stop()
horseshoe_model <- brm(
  formula = bf_formula,
  data = indicator_data,
  family = gaussian(),
  iter=opt$iter, 
  warmup=opt$warmup,
  cores=opt$ncores,
  backend = "cmdstanr",
  prior=horseshoe_priors
  # cores=1,
  # threads = threading(7)
  # control=list(adapt_delta=0.9)
)

save(horseshoe_model,file=paste0(opt$output,"/proj_pred/horseshoe_ref_model.rda"))


weak_prior_model <- brm(
  formula = bf_formula,
  data = indicator_data,
  family = gaussian(),
  iter=opt$iter, 
  warmup=opt$warmup,
  cores=opt$ncores,
  backend = "cmdstanr",
  prior=weak_priors
)

save(weak_prior_model,file=paste0(opt$output,"/proj_pred/weak_prior_ref_model.rda"))

# save(ref_model,file=paste0(opt$output,"/proj_pred/horseshoe_ref_model.rda"))

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



