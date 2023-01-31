
library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(tidybayes)
library(magrittr)
library(performance)
# load("./outputs/brms_anova/adm2_anova.rda")
load("./outputs/brms_anova/multi_level_normal/adm0_anova.rda")
plot(adm0_anova)

get_variables(adm0_anova)
posterior <- as.array(adm0_anova)
bayesplot::mcmc_areas(posterior,
                      pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sigma"))
pp_check(adm0_anova)



load("./outputs/brms_anova/multi_level_normal/adm0_adm_1_anova.rda")
plot(adm0_adm_1_anova)
get_variables(adm0_adm_1_anova)
posterior <- as.array(adm0_adm_1_anova)
bayesplot::mcmc_areas(posterior,
                      pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM1_CODE__Intercept","sigma"))


load("./outputs/brms_anova/multi_level_normal/adm0_adm_1_adm_2_anova.rda")
plot(adm0_adm_1_adm_2_anova)
get_variables(adm0_adm_1_adm_2_anova)
posterior <- as.array(adm0_adm_1_adm_2_anova)
bayesplot::mcmc_areas(posterior,
                      pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM1_CODE__Intercept","sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept","sigma"))


load("./outputs/brms_anova/multi_level_normal/adm_0_adm_2_village_anova.rda")
plot(adm_0_adm_2_village_anova)
get_variables(adm_0_adm_2_village_anova)
posterior <- as.array(adm_0_adm_2_village_anova)
bayesplot::mcmc_areas(posterior,
                      pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM2_CODE__Intercept","sd_ADM0_NAME:ADM2_CODE:village__Intercept","sigma"))
pp_check(adm_0_adm_2_village_anova)

load("./outputs/brms_anova/multi_level_normal/adm0_adm_1_adm_2_village_anova.rda")
plot(adm0_adm_1_adm_2_village_anova)
get_variables(adm0_adm_1_adm_2_village_anova)
posterior <- as.array(adm0_adm_1_adm_2_village_anova)
bayesplot::mcmc_areas(posterior,
                      pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM1_CODE__Intercept","sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept","sd_ADM0_NAME:ADM1_CODE:ADM2_CODE:village__Intercept","sigma"))
pp_check(adm0_adm_1_adm_2_village_anova)


# Adding LOO criteria
adm0_anova <- add_criterion(adm0_anova, "loo")
adm0_adm_1_anova <- add_criterion(adm0_adm_1_anova, "loo")
adm0_adm_1_adm_2_anova <-  add_criterion(adm0_adm_1_adm_2_anova, "loo")
adm_0_adm_2_village_anova <-  add_criterion(adm_0_adm_2_village_anova, "loo")
adm0_adm_1_adm_2_village_anova <-  add_criterion(adm0_adm_1_adm_2_village_anova, "loo")

loo_compare(adm0_anova, adm0_adm_1_anova, adm0_adm_1_adm_2_anova.rda, adm0_adm_1_adm_2_village_anova,adm_0_adm_2_village_anova,criterion = c("loo"), model_names = NULL)


load("./outputs/brms_anova/multi_level_quant/adm_0_adm_2_village_anova_q_0.1.rda")
summary(adm_0_adm_2_village_anova_q_0.1)

png("./outputs/brms_anova/plot_diagnostics.png",width = 1000,height=700)
plot(brm_anov_country)
dev.off()

draws_array <- brms::as_draws_array(brm_anov_country)

brms::variables(brm_anov_country)[c(1:10)]


level_1_level_2 <- bayesplot::mcmc_scatter(brm_anov_country,
                        pars=c("sd_ADM0_NAME__Intercept",
                               "sd_ADM0_NAME:ADM1_CODE__Intercept"))
ggsave("outputs/brms_anova/pairplots/level_1_level_2.png",plot=level_1_level_2,width = 5,height=4)



level_2_level_3 <- bayesplot::mcmc_scatter(brm_anov_country,
                        pars=c("sd_ADM0_NAME:ADM1_CODE__Intercept",
                               "sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept"))
ggsave("outputs/brms_anova/pairplots/level_2_level_3.png",plot=level_2_level_3,width = 5,height=4)




level_3_level_4 <-bayesplot::mcmc_scatter(brm_anov_country,
                        pars=c("sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept",
                               "sd_ADM0_NAME:ADM1_CODE:ADM2_CODE:village__Intercept"))
ggsave("outputs/brms_anova/pairplots/level_3_level_4.png",plot=level_3_level_4,width = 5,height=4)





# indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
# counties_per_region<- indicator_data %>% 
#   group_by(ADM1_CODE,ADM2_CODE) %>% 
#   summarise(
#     count = n()
#   ) %>% 
#   group_by(ADM1_CODE) %>% 
#   summarise(
#     number_of_counties = n()
#   )
# 
# counties_per_region_plot <- ggplot(counties_per_region, aes(x=number_of_counties)) + 
#   geom_histogram(binwidth = 1,boundary=0, color="black", fill="dodgerblue4") +
#   labs(title="Number of Subcounties (ADM2)\n in a County (ADM1)",x="Number of Subcounties in County",y="Count")+
#   theme(plot.title = element_text(hjust=0.5))
# ggsave("outputs/brms_anova/pairplots/subcounties_in_county.png",plot=counties_per_region_plot,width = 10,height=8)



bayesplot::mcmc_scatter(brm_anov_country,pars=c("sd_ADM0_NAME:ADM1_CODE:ADM2_CODE__Intercept","sd_ADM0_NAME:ADM1_CODE:ADM2_CODE:village__Intercept"))
bayesplot::mcmc_scatter(brm_anov_country,pars=c("sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM1_CODE__Intercept"))
bayesplot::mcmc_scatter(brm_anov_country,pars=c("sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM1_CODE__Intercept"))




# draws_fit <- brm_anov_country %>%
#   tidybayes::summarise_draws()


bayesplot::mcmc_pairs(brm_anov_country)

performance::variance_decomposition(brm_anov_country)
performance::icc(brm_anov_country)


summary(brm_anov_country)

vars <- as_draws(brm_anov_country, pars = "ADM0_NAME:ADM1_CODE")^2

# level 1 VPC = sigma^2_e / (sigma^2_v + sigma^2_u + sigma^2_e)


0.81

# spread_draws(b_Intercept, r_condition[condition,]) %>%
# median_qi(condition_mean = b_Intercept + r_condition, .width = c(.95, .66)) %>%
# ggplot(aes(y = condition, x = condition_mean, xmin = .lower, xmax = .upper)) +
# geom_pointinterval() 

# get_variables(brm_anov_country)
# summary(brm_anov_country)
# 
# draws_fit <- as_draws_df(brm_anov_country)
# draws_fit <- gather(draws_fit)

draws_fit$level <- NA
draws_fit$level[grepl("r_ADM0_NAME[", draws_fit$variable,fixed=T)] <- "level_1"
draws_fit$level[grepl("r_ADM0_NAME:ADM1_CODE[", draws_fit$variable,fixed=T)] <- "level_2"
draws_fit$level[grepl("r_ADM0_NAME:ADM1_CODE:ADM2_CODE[", draws_fit$variable,fixed=T)] <- "level_3"
draws_fit$level[grepl("r_ADM0_NAME:ADM1_CODE:ADM2_CODE:village[", draws_fit$variable,fixed=T)] <- "level_4"

draws_fit$variable <- gsub("r_ADM0_NAME:ADM1_CODE:ADM2_CODE:village[","",draws_fit$variable,fixed=T)
draws_fit$variable <- gsub("r_ADM0_NAME:ADM1_CODE:ADM2_CODE[","",draws_fit$variable,fixed=T)
draws_fit$variable <- gsub("r_ADM0_NAME:ADM1_CODE[","",draws_fit$variable,fixed=T)
draws_fit$variable <- gsub("r_ADM0_NAME[","",draws_fit$variable,fixed=T)
draws_fit$variable <- gsub(",Intercept]","",draws_fit$variable)



# row_subset <- draws_fit$level %in% c("level_1","level_2", "level_3")
row_subset <- draws_fit$level %in% c("level_1","level_2", "level_3", "level_4")

split_ids <- strsplit(draws_fit$variable[row_subset], split = "_")
location_ids <- lapply(split_ids, function(x){
  list_to_return <- list()
  if(length(split_ids)>0){
    list_to_return$adm0 <- x[1]
    
  }
  if(length(split_ids)>1){
    list_to_return$adm1 <- x[2]
    
  }
  
  if(length(split_ids)>2){
    list_to_return$adm2 <- x[3]
    
  }
  
  if(length(split_ids)>3){
    list_to_return$adm3 <- x[4]
    
  }
  
  return(list_to_return)
})


location_ids <- location_ids %>% bind_rows()



draws_fit$adm0 <- NA
draws_fit$adm1 <- NA
draws_fit$adm2 <- NA
draws_fit$adm3 <- NA

draws_fit$adm0[row_subset] <- location_ids$adm0
draws_fit$adm1[row_subset] <- location_ids$adm1
draws_fit$adm2[row_subset] <- location_ids$adm2
draws_fit$adm3[row_subset] <- location_ids$adm2

draws_fit$adm0 <- gsub(".", "_", draws_fit$adm0, fixed=T)


adm0_effects <- draws_fit %>%
  filter(level=="level_1" &  !is.na(level)) %>% 
  ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95,color=adm0)) +
  geom_pointinterval()+
  labs(y="ADM0 Area",x="Intercept",title="Intercepts for Multilevel Land_Cultivated Model")+
  # xlim(-1.5,2)+
  geom_vline(xintercept = 0)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust=0.5))

ggsave("outputs/brms_anova/adm0_effects.png",plot=adm0_effects,width = 10,height=8)

adm1_effects <- draws_fit %>%
  filter(level=="level_2" &  !is.na(level)) %>% 
  ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95,color=adm0)) +
  geom_pointinterval()+
  labs(y="ADM1 Area",x="Intercept",title="Intercepts for Multilevel Land_Cultivated Model")+
  # xlim(-1.5,2)+
  geom_vline(xintercept = 0)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust=0.5))

ggsave("outputs/brms_anova/adm1_effects.png",plot=adm1_effects,width = 10,height=8)

adm2_effects <- draws_fit %>%
  filter(level=="level_3" &  !is.na(level)) %>% 
  ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95,color=adm0)) +
  geom_pointinterval()+
  labs(y="ADM2 Area",x="Intercept",title="Intercepts for Multilevel Land_Cultivated Model")+
  # xlim(-1.5,2)+
  geom_vline(xintercept = 0)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust=0.5))
ggsave("outputs/brms_anova/adm2_effects.png",plot=adm2_effects,width = 10,height=8)



for (country in unique(draws_fit$adm0)){
  
  village_effects <- draws_fit %>%
    filter(level=="level_4" &  !is.na(level) & adm0==country) %>% 
    ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95)) +
    geom_pointinterval()+
    labs(y="Village",x="Intercept",title=paste0("Village Intercepts for Multilevel Land_Cultivated Model\n", country))+
    # xlim(-1.5,2)+
    geom_vline(xintercept = 0)+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust=0.5))
  ggsave(paste0("outputs/brms_anova/village_effects_",country,".png"),plot=village_effects,width = 10,height=8)
  
  
  
}
village_effects <- draws_fit %>%
  filter(level=="level_4" &  !is.na(level) & adm0=="Burkina_Faso") %>% 
  ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95)) +
  geom_pointinterval()+
  labs(y="Village",x="Intercept",title="Village Intercepts for Multilevel Land_Cultivated Model\n Burkina Faso")+
  # xlim(-1.5,2)+
  geom_vline(xintercept = 0)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust=0.5))
ggsave("outputs/brms_anova/village_effects_bf.png",plot=village_effects,width = 10,height=8)


countries <- unique

village_effects <- draws_fit %>%
  filter(level=="level_4" &  !is.na(level) & adm0=="Burkina.Faso") %>% 
  ggplot(aes(y = variable, x = mean, xmin = q5, xmax = q95)) +
  geom_pointinterval()+
  labs(y="Village",x="Intercept",title="Village Intercepts for Multilevel Land_Cultivated Model\n Burkina Faso")+
  # xlim(-1.5,2)+
  geom_vline(xintercept = 0)+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust=0.5))
ggsave("outputs/brms_anova/village_effects_bf.png",plot=village_effects,width = 10,height=8)






