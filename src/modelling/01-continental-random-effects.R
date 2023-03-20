# sbatch src/bc-run-scripts/run_brms_anova_location.sh  -i 2000 -w 1000 -n 4 -o brms_anova_21_03_2023
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
# opt <- list(
#   iter=2000,
#   warmup=1000,
#   data="./data/",
#   output="./outputs/continental_gaussian_location/",
#   ncores=4
# )



opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output,showWarnings = F)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))


# function for running brms hierarchical model ----------------------------

run_model <- function(data,levels, quantile=NULL, sigma, iter, warmup,ncores ){
  
  
  
  levels_args <- paste0(levels,collapse="/")
  temp_formula <- paste0("hfias_numeric ~ 1 + (1|",levels_args,")")
  
  
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
    data = indicator_data,
    family=family,
    cores = ncores,
    warmup = warmup,
    iter=iter
  )
  
  return(result)
}





indicator_data <- readr::read_csv(paste0(opt$data,"/prepped-data/rhomis-gaez-gdl.csv"))

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
# indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data <- indicator_data[!is.na(indicator_data$village),]
# indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]

# indicator_data$fies_score
# table(!is.na(indicator_data$hfias_status))
# 
# indicator_data$hfias_numeric<- NA
# indicator_data$hfias_numeric[indicator_data$hfias_status=="severely_fi"] <- 1
# indicator_data$hfias_numeric[indicator_data$hfias_status=="moderately_fi"] <- 2
# indicator_data$hfias_numeric[indicator_data$hfias_status=="mildly_fi"] <- 3
# indicator_data$hfias_numeric[indicator_data$hfias_status=="food_secure"] <- 4




indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
                                indicator_data$ADM1_CODE, "_",
                                indicator_data$ADM2_CODE)





# 
# level_combos <- list(
#   c("ADM0_NAME"),
#   c("ADM0_NAME","ADM1_CODE"),
#   c("ADM0_NAME","ADM1_CODE","ADM2_CODE"),
#   c("ADM0_NAME","ADM1_CODE","ADM2_CODE","village"),
#   
#   c("ADM0_NAME","ADM2_CODE","village")
# )

level_combos <- list(
  c("iso_country_code"),
  c("iso_country_code","gdlcode"),
  c("iso_country_code","gdlcode","village")
  # c("ADM0_NAME","ADM1_CODE","ADM2_CODE","village"),
  # 
  # c("ADM0_NAME","ADM2_CODE","village")
)



###########################################################################################
###########################################################################################
###########################################################################################
# Gaussian Models Location Only
###########################################################################################
###########################################################################################
###########################################################################################

dir.create(paste0(opt$output,"/continental_gaussian_location/"))

for (level_combo in level_combos){
  
  
  result <- run_model(data = indicator_data,level_combo, sigma=F, iter=opt$iter, warmup=opt$warmup,ncores=opt$ncores)
  save(result,file=paste0(opt$output,"/continental_gaussian_location/",paste0(level_combo, collapse="_"),".rda"))
  
  
  
}


# result <- brms::brm(
#   formula="hfias_numeric ~ 1 + (1|ADM0_NAME/ADM2_CODE/village)",
#   data = indicator_data,
#   family=gaussian(),
#   cores = 4,
#   warmup = 1000,
#   iter=2000
# )

# save(result,file=paste0(opt$output,"/continental_gaussian_location/","hfias_ADM0_NAME_ADM2_CODE_village",".rda"))









# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# d <- loadRData("outputs/brm_anov_31_01_2023/continental_gaussian_location/ADM0_NAME.rda")
# 



