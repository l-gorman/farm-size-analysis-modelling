library(brms)
library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggdist)
library(flextable)
library(tidybayes)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


ref_model <- loadRData("outputs/21_03_2023/proj_pred/proj_pred_ref_model.rda")
