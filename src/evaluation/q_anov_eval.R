# sbatch src/bc-run-scripts/compare_quantile_location_models.sh   -c 28 -d quantile_location -o model_summaries  
# sbatch src/bc-run-scripts/compare_quantile_location_models.sh   -c 28 -d quantile_location -o model_summaries  


library(brms)
library(tidybayes)
library(tidyr)
library(dplyr)
library(ggplot2)

library(optparse)
library(doParallel)
library(foreach)
library(magrittr)



option_list = list(
  make_option(c("-d", "--indirectory"), type='character',
              help="Directory where data will be loaded from"),
  make_option(c("-o", "--outdirectory"), type='character',
              help="The directory where results will be written"),
  make_option(c("-n", "--outname"), type='character',
              help="The name of the output file"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores")
)



opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# opt <- list(
#   indirectory="outputs/quantile_location/",
#   outdirectory="outputs/quantile_model_summaries/",
#   
#   outname="quantile_location_summary",
#   ncores="4"
#   
#   
# 
# )

opt$indirectory <- gsub("/$", "", opt$indirectory)
opt$outdirectory <- gsub("/$", "", opt$outdirectory)



cl <- makeCluster(as.numeric(opt$ncores))
registerDoParallel(cl)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


model_files <- list.files(opt$indirectory)
model_files <- model_files[grepl(".rda", model_files,fixed=T)]


final_result <- foreach(i=1:length(model_files),.combine=rbind,.packages = c("magrittr", "tidybayes"))%dopar%{
  
  model_file <- model_files[i]
  m <- loadRData(paste0(opt$indirectory,"/",model_file))
  
  
  model <- model_file %>% 
    gsub(".rda","",fixed=T,.) %>% 
    sub("_[^_]+$", "", .)
  
  quantile <- model_file %>% 
    gsub(".rda","",fixed=T,.) %>% 
    gsub(paste0(model,"_"), "", .)
  
  
  result <- m %>%
    gather_draws(b_Intercept,sigma,`.*sd_ADM0_NAME.*`, regex = T
    ) %>%
    median_qi()
  
 
  result$model <- model
  result$quantile <- quantile
  
  result
  
}


dir.create(opt$outdirectory)

readr::write_csv(final_result,paste0(opt$outdirectory, "/",opt$outname))





