# sbatch src/bc-run-scripts/run_projpred_ref_model.sh  -s "04-projpred-selection.R" -n 5 -o brms_anova_21_03_2023


library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(magrittr)
library(optparse)
library(projpred)
library(cmdstanr)

options(mc.cores = 28, brms.backend = "cmdstanr") # allows threading
cmdstanr::set_cmdstan_path("/user/home/lg14410/.cmdstan/cmdstan-2.31.0")


# Solution to globals size, found here:
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
options(future.globals.maxSize = 8000 * 1024^2)

### Only for technical reasons in this vignette (you can omit this when running
### the code yourself):
###


option_list = list(
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
#   output="./outputs/21_03_2023/",
#   ncores=4
# )
writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

options(mc.cores = opt$ncores)


dir.create(opt$output)


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


ref_model <- loadRData(paste0(opt$output,"/proj_pred/weak_prior_ref_model.rda"))
ref_model <- get_refmodel(ref_model)


land_cultivated_varsel <- cv_varsel(ref_model,
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 1)
save(land_cultivated_varsel,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_1.rda"))

land_cultivated_varsel <- cv_varsel(ref_model,
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 2)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_2.rda"))

land_cultivated_varsel <- cv_varsel(ref_model,
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 3)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_3.rda"))

land_cultivated_varsel <- cv_varsel(ref_model,
                                    method = 'forward', cv_method = 'kfold', K = 5, verbose = TRUE, seed = 4)
save(ref_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_4.rda"))




