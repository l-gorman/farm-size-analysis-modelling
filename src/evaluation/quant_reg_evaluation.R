
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(tidybayes)
library(magrittr)
# load("./outputs/brms_anova/adm2_anova.rda")
load("./outputs/brms_quant_reg/quantile_reg_horseshoe.rda")


summary(brm_quant_reg)

