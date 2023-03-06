
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



adm0_adm_2_anova_gaus <- loadRData("outputs/brms_anova/multi_level_normal/adm_0_adm_2_village_anova.rda")
adm0_adm_2_anova_q0.5 <- loadRData("outputs/quantile_location/ADM0_NAME_ADM2_CODE_village_0.5.rda")


params <- c("b_Intercept", 
            "sd_ADM0_NAME__Intercept",
            "sd_ADM0_NAME:ADM2_CODE__Intercept",
            "sd_ADM0_NAME:ADM2_CODE:village__Intercept",
            "sigma")

gaus_model_draws <- as_draws_df(adm0_adm_2_anova_gaus) %>% 
  select(params)

gaus_model_draws$model <- "gaussian"

quantile_draws  <- as_draws_df(adm0_adm_2_anova_q0.5) %>% 
  select(params)
quantile_draws$model <- "quantile"

all_draws <- rbind(gaus_model_draws,quantile_draws)
all_draws <- gather(data=all_draws, key = "variable",value = "value", -`model`)


all_draws <- all_draws %>% 
  group_by(model,variable) %>% 
  summarise(
    l95ci=quantile(value,c(0.05)),
    estimate=mean(value),
    u95ci=quantile(value,c(0.95)),
  )

all_draws %>%
  ggplot(aes(x = estimate,y=interaction(model, variable), color=model)) +
  geom_point()+
  geom_segment(aes(y=interaction(model, variable),yend=interaction(model, variable), x=l95ci, xend=u95ci))
# scale_fill_brewer(na.translate = FALSE) +
# labs(y="", x="VPC",title = "Variance Partition Coefficients for 3 Level Hierarchical Model",subtitle=paste0("Levels: ",model_name),
# caption = "Points represent estimates.\nThicker lines represent standard error.\n Thin lines represent 95% credible intervals")+
# scale_y_discrete(name="VPC",
#                  breaks=c("sd_ADM0_NAME__Intercept", "sd_ADM0_NAME:ADM2_CODE__Intercept", "sd_ADM0_NAME:ADM2_CODE:village__Intercept", "sigma"),
#                  labels=c("Between Country", "Between Subcounty", "Between Village", "Unexplained\nVariance"))



synthetic <- brms::rasym_laplace(n=1000,mu=2,sigma=1,quantile = 0.1)


asym_laplace_density <- tibble(
  quantile=numeric(),
  x=numeric(),
  y=numeric()
)


for (quant in seq(0.25,0.75,0.25)){
  
  
  x <- seq(0,20,0.1)
  y <- brms::dasym_laplace(x=x,mu = 10,sigma = 1,quantile = quant)
  quantile_vect <- rep(quant,length(x))
  temp_tibble <- tibble::as_tibble(list(
    quantile=quantile_vect,
    x=x,
    y=y
  ))
  asym_laplace_density <- asym_laplace_density %>% bind_rows(temp_tibble)
  
}


asym_laplace_density$quantile <- as.factor(asym_laplace_density$quantile)


ggplot(asym_laplace_density, aes(x=x,y=y, group=quantile, color=quantile))+
  geom_line()



