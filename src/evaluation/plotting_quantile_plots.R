library(readr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)

model_summary <- readr::read_csv("./outputs/model_summaries/model_summaries/quantile_location_summary.csv")
# model_summary <- model_summary[model_summary$.variable!="b_Intercept",]
unique(model_summary$model)
unique(model_summary$.variable)

model_summary$shifted_quantile <- NA
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME__Intercept"] - 0.01                   
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE__Intercept"]        
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept"] +0.01
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE:ADM2_CODE:village__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME:ADM1_CODE:ADM2_CODE:village__Intercept"]+0.21
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME:ADM2_CODE:village__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME:ADM2_CODE:village__Intercept"]+0.01
model_summary$shifted_quantile[model_summary$.variable=="sd_ADM0_NAME:ADM2_CODE__Intercept"] <- model_summary$quantile[model_summary$.variable=="sd_ADM0_NAME:ADM2_CODE__Intercept"]
model_summary$shifted_quantile[model_summary$.variable=="sigma"] <- model_summary$quantile[model_summary$.variable=="sigma"] 

model_summary %>% 
  filter(model==model_name, .variable=="sigma")


model_name <- "ADM0_NAME_ADM2_CODE_village"
param <- "sd_ADM0_NAME:ADM2_CODE:village__Intercept"
model_summary %>% 
  filter(model==model_name, .variable!="b_Intercept") %>% 
  ggplot(aes(x=shifted_quantile,y=.value, color=.variable)) +
  scale_color_discrete(name="Level of Variation",
                      breaks=c("sigma","sd_ADM0_NAME__Intercept", "sd_ADM0_NAME:ADM2_CODE__Intercept", "sd_ADM0_NAME:ADM2_CODE:village__Intercept", "sigma"),
                      labels=c("unexplained","Between Country", "Between Subcounty", "Between Village", "Within Village"))+
  geom_point()+
  geom_segment(aes(x=shifted_quantile, xend=shifted_quantile, y=.lower, yend=.upper))+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  
  labs(y="Estimate", x="Quantile (Jittered)",title = "Quantile Model",subtitle=paste0("Levels: ",model_name),
       caption = "Points represent estimates. Lines represent 95% credible intervals")


model_name <- "ADM0_NAME_ADM2_CODE_village"
param <- "sd_ADM0_NAME:ADM2_CODE__Intercept"
model_summary %>% 
  filter(model==model_name, .variable==param) %>% 
  ggplot(aes(x=quantile,y=.value)) +
  geom_point()+
  geom_segment(aes(x=quantile, xend=quantile, y=.lower, yend=.upper))+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  
  labs(y=param, title = "Quantile Model",subtitle=paste0("Levels: ",model_name, "\nParam: ",param),
       caption = "Lines represent 95% credible intervals")

model_name <- "ADM0_NAME_ADM2_CODE_village"
param <- "b_Intercept"
model_summary %>% 
  filter(model==model_name, .variable==param) %>% 
  ggplot(aes(x=quantile,y=.value)) +
  geom_point()+
  geom_segment(aes(x=quantile, xend=quantile, y=.lower, yend=.upper))+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  
  labs(y=param, title = "Quantile Model",subtitle=paste0("Levels: ",model_name, "\nParam: ",param),
       caption = "Lines represent 95% credible intervals")
  # facet_wrap(vars(.variable))
