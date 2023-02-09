
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(tidybayes)
library(magrittr)


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


adm0_anova <- loadRData("outputs/gaussian_location_scale/gaussian_location_scale/ADM0_NAME.rda")
adm0_adm_1_anova <- loadRData("outputs/gaussian_location_scale/gaussian_location_scale/ADM0_NAME_ADM1_CODE.rda")
adm0_adm_1_adm_2_anova <- loadRData("outputs/gaussian_location_scale/gaussian_location_scale/ADM0_NAME_ADM1_CODE_ADM2_CODE.rda")
adm0_adm_1_adm_2_village_anova <- loadRData("outputs/gaussian_location_scale/gaussian_location_scale/ADM0_NAME_ADM1_CODE_ADM2_CODE_village.rda")
adm_0_adm_2_village_anova <- loadRData("outputs/gaussian_location_scale/gaussian_location_scale/ADM0_NAME_ADM2_CODE_village.rda")


# LOO  ----------------------------------------------------------
adm0_anova <- add_criterion(adm0_anova, "loo")
adm0_adm_1_anova <- add_criterion(adm0_adm_1_anova, "loo")
adm0_adm_1_adm_2_anova <-  add_criterion(adm0_adm_1_adm_2_anova, "loo")
adm_0_adm_2_village_anova <-  add_criterion(adm_0_adm_2_village_anova, "loo")
adm0_adm_1_adm_2_village_anova <-  add_criterion(adm0_adm_1_adm_2_village_anova, "loo")

compaloo_compare(adm0_anova, adm0_adm_1_anova, adm0_adm_1_adm_2_anova, adm0_adm_1_adm_2_village_anova,adm_0_adm_2_village_anova,criterion = c("loo"), model_names = NULL)


posterior <- as.array(adm0_adm_1_adm_2_village_anova)
bayesplot::mcmc_intervals(posterior,
                          pars = c("b_sigma_Intercept", "sd_ADM0_NAME__sigma_Intercept","sd_ADM0_NAME:ADM2_CODE__sigma_Intercept","sd_ADM0_NAME:ADM2_CODE:village__sigma_Intercept"))







