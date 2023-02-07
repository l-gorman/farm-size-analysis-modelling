# sbatch src/bc-run-scripts/run_brms_anova_quantile_location_scale.sh  -i 4000 -w 2000 -n 4 -o brms_anova_03_02_2023 
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(magrittr)
library(optparse)

library(foreach)
library(doParallel)

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
              help="The number of chains/cores"),
  make_option(c("-j", "--index"), type='character',
              help="index for village quant combo")
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);






# opt <- list(
#   iter=2,
#   warmup=1,
#   data="./data/",
#   output="./outputs/brm_anov_31_01_2023/",
#   ncores=4, #Actually the number of chains
#   workers=4
#   # cores=9,
#   
# )



# workers <- opt$workers
# cl <- parallel::makeCluster(workers)
# # register the cluster for using foreach
# doParallel::registerDoParallel(cl)


dir.create(opt$output)


opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

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
    data = indicator_data,
    family=family,
    cores = ncores,
    warmup = warmup,
    iter=iter
  )
  
  return(result)
}





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


village_quant_combos <- list()
i <-1
for (level_combo in level_combos){
  for (quantile in seq(0.1,0.9,0.1)){
    
    village_quant_combos[[i]] <- list(
      level_combo=level_combo,
      quantile=quantile
    )
    i <- i + 1
    
  }
  
}






###########################################################################################
###########################################################################################
###########################################################################################
# Gaussian Models Location Only
###########################################################################################
###########################################################################################
###########################################################################################

dir.create(paste0(opt$output,"/quantile_location/"))

# for (level_combo in level_combos){
#   
#   
#   for (quantile in seq(0.1,0.9,0.1)){
#     result <- run_model(data,level_combo, quantile=quantile, sigma=F, iter=opt$iter, warmup=opt$warmup,ncores=opt$ncores)
#     save(result,file=paste0(opt$output,"/quantile_location/",paste0(paste0(level_combo, collapse="_"),"_",quantile),".rda"))
#   }
#   
#   
#   
#   
# }

dir.create(paste0(opt$output,"/quantile_location_scale/"))

# foreach(i = c(1:length(village_quant_combos)),  .packages = c("brms")) %dopar% {
i <- opt$index
  quantile <- village_quant_combos[[i]][["quantile"]]
  level_combo <- village_quant_combos[[i]][["level_combo"]]
  
  result <- run_model(data,level_combo, quantile=quantile, sigma=T, iter=opt$iter, warmup=opt$warmup,ncores=opt$ncores)
  save(result,file=paste0(opt$output,"/quantile_location_scale/",paste0(paste0(level_combo, collapse="_"),"_",quantile),".rda"))
# }





# 
# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
#   d <- loadRData("outputs/brm_anov_31_01_2023/quantile_location/ADM0_NAME_ADM1_CODE_ADM2_CODE_village_0.8.rda")
# # 



