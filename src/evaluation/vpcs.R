
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# model <- loadRData("./outputs/brms_anova/multi_level_normal/adm_0_adm_2_village_anova.rda")
model <- loadRData("./outputs/gaussian_location/gaussian_location/hfias_ADM0_NAME_ADM2_CODE_village.rda")
model

bayes_R2(model)
posterior <- as.array(model)
bayesplot::mcmc_intervals(posterior,
                          pars = c("b_Intercept", "sd_ADM0_NAME__Intercept","sd_ADM0_NAME:ADM2_CODE__Intercept","sd_ADM0_NAME:ADM2_CODE:village__Intercept","sigma"))

as_draws(model)

vpc <- function(model, params){
  
  draws_df <- results <- as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}


brm_anov_vpc <-  vpc( 
  model,
  c( "sd_ADM0_NAME__Intercept",
     "sd_ADM0_NAME:ADM2_CODE__Intercept",
     "sd_ADM0_NAME:ADM2_CODE:village__Intercept",
     "sigma"))




model_name <- "ADM0_NAME_ADM2_CODE_village"
brm_anov_vpc %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(
    l95ci=quantile(value, probs=c(0.05)),
    estimate=quantile(value, probs=c(0.5)),
    u95ci=quantile(value, probs=c(0.95))
    
  ) %>% 
  ggplot(aes(x=key, y=estimate)) +
  geom_point()+
  geom_segment(aes(x=key, xend=key, y=l95ci, yend=u95ci))+

  labs(y="Estimate", x="Quantile (Jittered)",title = "Variance Partition Coefficients for 3 Level hierarchical Model",subtitle=paste0("Levels: ",model_name),
       caption = "Points represent estimates. Lines represent 95% credible intervals")+
  scale_x_discrete(name="VPC",
                       breaks=c("sd_ADM0_NAME__Intercept", "sd_ADM0_NAME:ADM2_CODE__Intercept", "sd_ADM0_NAME:ADM2_CODE:village__Intercept", "sigma"),
                       labels=c("Between Country", "Between Subcounty", "Between Village", "Within Village"))
  
brm_anov_vpc %>% 
  gather() %>%
  ggplot(aes(y = key, x = value)) +
  stat_halfeye(aes(fill = after_stat(level))) +
  scale_fill_brewer(na.translate = FALSE) +
  labs(y="", x="VPC",title = "Variance Partition Coefficients for 3 Level Hierarchical Model",subtitle=paste0("Levels: ",model_name),
       caption = "Points represent estimates.\nThicker lines represent standard error.\n Thin lines represent 95% credible intervals")+
  scale_y_discrete(name="VPC",
                   breaks=c("sd_ADM0_NAME__Intercept", "sd_ADM0_NAME:ADM2_CODE__Intercept", "sd_ADM0_NAME:ADM2_CODE:village__Intercept", "sigma"),
                   labels=c("Between Country", "Between Subcounty", "Between Village", "Unexplained\nVariance"))







