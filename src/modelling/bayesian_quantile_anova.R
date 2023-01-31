# sbatch src/bc-run-scripts/run_brms_models.sh -s src/modelling/bayesian_quantile_anova.R -i 10000 -n 4 -o brms_anova_quantile_31_01_2023 


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
  make_option(c("-d", "--data"), type='character',
              help="Directory where data will be loaded from"),
  make_option(c("-o", "--output"), type='character',
              help="The directory where results will be written"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores")

)

 
# opt <- list(
#   iter=2,
#   data="./data/",
#   output="./outputs/brm_anov_31_01_2023/",
#   ncores=4
# )

dir.create(opt$output)


opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))




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







# Quant 0.5 ---------------------------------------------------------------
adm_0_adm_2_village_anova_q_0.5 <- brms::brm(
  formula=bf(log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
             quantile=0.5),
  
  data = indicator_data,
  family = asym_laplace(),
  
  iter = opt$iter,
  cores = opt$ncores 
)
save(adm_0_adm_2_village_anova_q_0.5,file=paste0(opt$output, "/adm_0_adm_2_village_anova_q_0.5.rda"))


# Quant 0.1 ---------------------------------------------------------------
adm_0_adm_2_village_anova_q_0.1 <- brms::brm(
  formula=bf(log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
             quantile=0.1),
  
  data = indicator_data,
  family = asym_laplace(),
  
  iter = opt$iter,
  cores = opt$ncores 
)
save(adm_0_adm_2_village_anova_q_0.1,file=paste0(opt$output, "/adm_0_adm_2_village_anova_q_0.1.rda"))


# Quant 0.25 ---------------------------------------------------------------
adm_0_adm_2_village_anova_q_0.25 <- brms::brm(
  formula=bf(log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
             quantile=0.25),
  
  data = indicator_data,
  family = asym_laplace(),
  
  iter = opt$iter,
  cores = opt$ncores 
)
save(adm_0_adm_2_village_anova_q_0.25,file=paste0(opt$output, "/adm_0_adm_2_village_anova_q_0.25.rda"))

# Quant 0.75 ---------------------------------------------------------------
adm_0_adm_2_village_anova_q_0.75 <- brms::brm(
  formula=bf(log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
             quantile=0.75),
  
  data = indicator_data,
  family = asym_laplace(),
  
  iter = opt$iter,
  cores = opt$ncores 
)
save(adm_0_adm_2_village_anova_q_0.75,file=paste0(opt$output, "/adm_0_adm_2_village_anova_q_0.75.rda"))


# Quant 0.9 ---------------------------------------------------------------
adm_0_adm_2_village_anova_q_0.9 <- brms::brm(
  formula=bf(log(land_cultivated_ha)~(1|ADM0_NAME/ADM2_CODE/village),
             quantile=0.9),
  
  data = indicator_data,
  family = asym_laplace(),
  
  iter = opt$iter,
  cores = opt$ncores 
)

save(adm_0_adm_2_village_anova_q_0.9,file=paste0(opt$output, "/adm_0_adm_2_village_anova_q_0.9.rda"))




