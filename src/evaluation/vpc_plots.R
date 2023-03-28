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


vpc <- function(model, params){
  
  
  draws_df <-  as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}


summary_table <- function(model){
  
  
  model_summary <- summary(model)
  pop_effects <- model_summary$fixed %>% as_tibble()
  pop_effects$param_type <- "Population-Level Effects"
  pop_effects$param <- "Intercept"
  pop_effects$level <- "" 
  
  pop_effects <- pop_effects %>% relocate(level)
  pop_effects <- pop_effects %>% relocate(param)
  pop_effects <- pop_effects %>% relocate(param_type)
  
  
  sigma <- model_summary$spec_pars %>% as_tibble()
  sigma$param_type <- "Family Specific Parameters"
  sigma$param <- "Sigma"
  sigma$level <- ""
  
  sigma <- sigma %>% relocate(level)
  sigma <- sigma %>% relocate(param)
  sigma <- sigma %>% relocate(param_type)
  
  group_effects <- lapply(names(model_summary$random), function(x){
    temp <- as_tibble(model_summary$random[[x]])
    
    temp$param_type <- "Group-Level Effects"
    temp$param <- "sd(Intercept)" 
    temp$level <- x
    
    temp <- temp %>% relocate(level)
    temp <- temp %>% relocate(param)
    temp <- temp %>% relocate(param_type)
    temp 
    
  }) %>% bind_rows()
  
  result <- bind_rows(pop_effects,sigma,group_effects)
  
  columns_to_numberic <- c('Estimate','Est.Error',"l-95% CI","u-95% CI",'Rhat', 'Bulk_ESS', 'Tail_ESS')
  result[columns_to_numberic] <- result[columns_to_numberic] %>% mutate(across(everything(),round,2))
  return(result)
  
}

plot_vpc <- function(model,
                     params,
                     readable_params,
                     title,
                     subtitle
){
  
  data <- model$data
  number_of_points <- nrow(data)
  
  
  mean_land_cultivated_ha <- round(mean(data$land_cultivated_ha),2)
  sd_land_cultivated_ha <- round(sd(data$land_cultivated_ha))
  
  
  
  
  
  
  number_of_villages <- length(unique(data$village))
  people_per_village <- data %>% 
    group_by(village) %>% summarise(number_of_people=n())
  mean_people_per_village <- round(mean(people_per_village$number_of_people),2)
  
  number_of_subnational_regions <- length(unique(data$gdlcode))
  villages_per_subnational_region <- data %>% 
    group_by(gdlcode, village) %>% summarise(number_of_people=n()) %>% ungroup() %>% 
    group_by(gdlcode) %>% summarise(number_of_villages=n())
  mean_villages_per_county <- round(mean(villages_per_subnational_region$number_of_villages),2)
  
  
  
  brm_anov_vpc <-vpc( 
    model,
    params)
  
  # brm_anov_vpc <- brm_anov_vpc %>% 
  #   gather() %>% 
  #   group_by(key) %>% 
  #   summarise(
  #     l95ci=quantile(value, probs=c(0.05)),
  #     estimate=quantile(value, probs=c(0.5)),
  #     u95ci=quantile(value, probs=c(0.95))
  #   )
  
  caption <- paste0("Points=estimates, Thick lines=stder, Thin lines=95% CIs\n",
                    "N_hhs=",number_of_points,", N_villages=",number_of_villages,", N_subregions=",number_of_subnational_regions,"\n",
                    "avg_hhs_per_village=",mean_people_per_village,", avg_villages_per_subregion=",mean_villages_per_county,"\n",
                    "mean_farm_size=",mean_land_cultivated_ha,", sd_farm_size=",sd_land_cultivated_ha
  )
  
  brm_anov_vpc %>% 
    gather() %>%
    ggplot(aes(y = key, x = value)) +
    stat_halfeye(aes(fill = after_stat(level))) +
    scale_fill_brewer(na.translate = FALSE) +
    labs(y="", x="VPC",title = title,
         caption = caption)+
    scale_y_discrete(name="VPC",
                     breaks=params,
                     labels=readable_params)
  
  
  
  
}

plot_multiple_vpcs <- function(country_vec,
                               model_paths,
                               params,
                               param_of_interest,
                               readable_params,
                               title,
                               subtitle,
                               facet_wrap=F){
  
  
  
  all_vpcs <- lapply(1:length(model_paths), function(i){
    model <- loadRData(model_paths[i])
    brm_anov_vpc <-vpc( 
      model,
      params)
    brm_anov_vpc$country <- country_vec[i]
    return(brm_anov_vpc)
  }) %>% bind_rows()
  
  if (facet_wrap==T){
    result <- all_vpcs %>%
      pivot_longer(!country) %>%
      ggplot(aes(y = name, x = value)) +
      stat_pointinterval() +
      scale_fill_brewer(na.translate = FALSE) +
      labs(y="", x="VPC",title = "Variance Partition Coefficients for Individual Country Models")+
      scale_y_discrete(
        breaks=params,
        labels=readable_params)+
      facet_wrap(~country)
    return(result)
  }else{
    
    result <- 
      
      ggplot(all_vpcs,aes(y = country, x = .data[[param_of_interest]])) +
      stat_pointinterval() +
      scale_fill_brewer(na.translate = FALSE) +
      labs(y="County", x="VPC",title = title)
    # scale_y_discrete(
    #   breaks=params,
    #   labels=readable_params)
  }
  
  return(result)
  
  
  
  
  
  
}


# model_path <- 
#   params <-
#   readable_params <- 
#   levels <- "Level 1: Individual, Level 2: Village, Level 3: Subcounty, Level 4: Country"
# title <- 
dir.create("outputs/vpc_plots/")

dir.create("outputs/vpc_plots/gdl")


model <- loadRData( "./outputs/21_03_2023/iso_country_code_gdlcode_village.rda")
get_variables(model)[1:10]
temp <- plot_vpc(model =model,
                 params =  c( "sd_iso_country_code__Intercept",
                              "sd_iso_country_code:gdlcode__Intercept",
                              "sd_iso_country_code:gdlcode:village__Intercept",
                              "sigma"),
                 readable_params = c("Between Country", "Between Subcounty", "Between Village", "Unexplained"),
                 title = "VPCs for Farm Size All Data")

dir.create("outputs/vpc_plots/gdl/all_data")
ggsave( "outputs/vpc_plots/gdl/all_data/vpc_plot.png",temp)

overall_summary <- summary_table(model)
overall_summary <- overall_summary %>% 
  add_column(country = rep("all_countries",nrow(overall_summary)), .before = "param_type")  
ft <- flextable(overall_summary)
ft <- autofit(ft)

save_as_image(ft, path = "outputs/vpc_plots/gdl/all_data/table_fit_summary.png")




# Trace Plots
png(filename = "./outputs/vpc_plots/gdl/all_data/trace_plots.png")
plot(model)
dev.off()


get_variables(model)

png(filename = "./outputs/vpc_plots/gdl/all_data/mcmc_scatter.png",width = 1000,height = 500,units="px")
# bayesplot::mcmc_scatter(model,
#                         pars=c("sd_ADM0_NAME__Intercept",
#                                "sd_ADM0_NAME:ADM2_CODE__Intercept"))

print(bayesplot::mcmc_pairs(model,  regex_pars = "sd_|sigma",
                            off_diag_args = list(size = 1, alpha = 0.5)))

dev.off()







all_countries <- list.dirs("./outputs/21_03_2023/per_country")
all_countries <- all_countries[all_countries!="./outputs/21_03_2023/per_country"]

list_of_plots <- list()

for (country_dir in all_countries){
  
  country <- strsplit(country_dir, "/")
  country <- lapply(country, function(x){
    x[length(x)]
  }) %>% unlist()
  model <- loadRData(paste0("./outputs/21_03_2023/per_country/",country,"/ADM2_CODE_village.rda"))
  
  dir.create(paste0("outputs/vpc_plots/gdl/",country))
  # Plot VPCs
  temp <- plot_vpc(model = model,
                   params =  c(
                     "sd_gdlcode__Intercept",
                     "sd_gdlcode:village__Intercept",
                     "sigma"),
                   readable_params = c("Between Subcounty", "Between Village", "Unexplained"),
                   title = paste0("VPCs for Land Cultivated Model ",country))
  ggsave(paste0("outputs/vpc_plots/gdl/",country,"/vpc_plot.png"),temp)
  
  
  temp <- NULL 
  
  caption <- 
    
    
    # Summary Convergence Stats
    summary <- summary_table(model)
  
  summary <- summary %>% 
    add_column(country = rep(country,nrow(summary)), .before = "param_type")  
  
  overall_summary <-bind_rows(overall_summary,summary)
  
  
  ft <- flextable(summary)
  ft <- autofit(ft)
  
  save_as_image(ft, path = paste0("outputs/vpc_plots/gdl/",country,"/table_fit_summary.png"))
  
  
  
  
  # Trace Plots
  png(filename = paste0("./outputs/vpc_plots/gdl/",country,"/trace_plots.png"))
  plot(model)
  dev.off()
  
  png(filename = paste0("./outputs/vpc_plots/gdl/",country,"/mcmc_scatter.png"),width=1000,height=500,units="px")
  print(bayesplot::mcmc_pairs(model,  regex_pars = "sd_|sigma",
                              off_diag_args = list(size = 1, alpha = 0.5)))
  dev.off()
  
  
}

dir.create("./outputs/vpc_plots/gdl/summary")

countries_all <- gsub('.*/ ?(\\w+)', '\\1', all_countries) 
model_paths <- paste0("./outputs/21_03_2023/per_country/",countries_all,"/ADM2_CODE_village.rda")
country_vec <- countries_all

model_paths
params =  c(
  "sd_gdlcode__Intercept",
  "sd_gdlcode:village__Intercept",
  "sigma")

county_vpcs <- plot_multiple_vpcs(country_vec,
                                  model_paths,
                                  params,
                                  "sd_gdlcode__Intercept",
                                  c("Between Subcounty", "Between Village", "Unexplained"),
                                  "VPCs for Between County Variance",
                                  "",
                                  facet_wrap=F)
ggsave("./outputs/vpc_plots/gdl/summary/county_vpc_comparisons.png",county_vpcs)

village <- plot_multiple_vpcs(country_vec,
                              model_paths,
                              params,
                              "sd_gdlcode:village__Intercept",
                              c("Between Subcounty", "Between Village", "Unexplained"),
                              "VPCs for Between Village Variance",
                              "",
                              facet_wrap=F)
ggsave("./outputs/vpc_plots/gdl/summary/village_vpc_comparisons.png",village)


unexplained <- plot_multiple_vpcs(country_vec,
                                  model_paths,
                                  params,
                                  "sigma",
                                  c("Between Subcounty", "Between Village", "Unexplained"),
                                  "VPCs for Unexplained Variance",
                                  "",
                                  facet_wrap=F)
ggsave("./outputs/vpc_plots/gdl/summary/unexplained_vpc_comparisons.png",unexplained)

facet_vpcs <- plot_multiple_vpcs(country_vec,
                                 model_paths,
                                 params,
                                 "sigma",
                                 c("Between Subcounty", "Between Village", "Unexplained"),
                                 "VPCs for Per Country",
                                 "",
                                 facet_wrap=T)
ggsave("./outputs/vpc_plots/gdl/summary/vpc_comparisons_facet.png",facet_vpcs,width = 4000,height = 2000,units = "px")



# Plotting Summary Estimates ----------------------------------------------


plotting_data <- overall_summary %>% 
  group_by(country) %>% 
  filter(param!="Intercept") 
plotting_data$variance_estimate <- plotting_data$Estimate^2
plotting_data$min_variance_estimate <- plotting_data$`l-95% CI`^2
plotting_data$max_variance_estimate <- plotting_data$Estimate^2

plotting_data$country[plotting_data$country=="all_countries"] <- 'All'

total_variance <- plotting_data %>% group_by(country) %>% 
  summarise(total_variance = sum(Estimate^2))

plotting_data <- plotting_data %>% merge(total_variance,by="country",all.x = T,all.y = F)
plotting_data$vpc <- plotting_data$variance_estimate/plotting_data$total_variance

plotting_data$vpc_level <- NA
plotting_data$vpc_level[plotting_data$param=="Sigma"] <- "Unexplained"
plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code")] <- "Between Country"
plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code:gdlcode","gdlcode")] <- "Between Subcounty"
plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code:gdlcode:village","gdlcode:village")] <- "Between Villages"

vpc_summary <- ggplot(plotting_data, aes(x=country, y=vpc, fill=vpc_level)) +
  geom_bar(stat='identity', position = 'stack')+
  labs(title = "Estimated Variance Partition Coefficients", x="Country", y="Proportion of Variance",fill='Source of Variation') +
  theme(axis.text.x = element_text(face = c('bold', rep("plain", length(unique(plotting_data$country))-1))))
ggsave(plot =vpc_summary,filename =  "./outputs/vpc_plots/gdl/summary/vpc_summary_stacked.png")

readr::write_csv(overall_summary, "./outputs/vpc_plots/gdl/summary/model_comparison_table_full.csv")



# plotting_data$vpc_level[plotting_data$param=="Sigma"] <- "Unexplained"
# plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code")] <- "Between Country"
# plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code:gdlcode","gdlcode")] <- "Between Subcounty"
# plotting_data$vpc_level[plotting_data$level%in%c("iso_country_code:gdlcode:village","gdlcode:village")] <- "Between Villages"

short_summary <- overall_summary
short_summary$param_clean <- NA
short_summary$param_clean[short_summary$param=="Intercept"] <- "Intercept"

short_summary$param_clean[short_summary$param=="Sigma"] <- "Unexplained"
short_summary$param_clean[short_summary$level%in%c("iso_country_code")] <- "Between Country"
short_summary$param_clean[short_summary$level%in%c("iso_country_code:gdlcode","gdlcode")] <- "Between Subcounty"
short_summary$param_clean[short_summary$level%in%c("iso_country_code:gdlcode:village","gdlcode:village")] <- "Between Villages"

short_summary$param_type <- NULL
short_summary$param <- NULL
short_summary$level <- NULL

final_summary <- short_summary %>% pivot_longer(cols = Estimate:Tail_ESS) %>% 
  pivot_wider(names_from = "country",values_from = "value")


final_summary <- final_summary %>% 
  rename(Parameter=param_clean) %>% 
  rename(Value=name) %>% 
  rename("All Countries"=all_countries) %>% 
  filter(Value %in% c("Rhat", "Bulk_ESS", "Tail_ESS")==F)

final_summary$Parameter <- factor(final_summary$Parameter,levels = c("Between Country",
                                                                     "Between Subcounty",
                                                                     "Between Villages",
                                                                     "Unexplained",
                                                                     "Intercept"),ordered = T)

final_summary$Value <- factor(final_summary$Value,levels = c("Estimate",
                                                                     "Est.Error",
                                                                     "l-95% CI",
                                                                     "u-95% CI"),ordered = T)

final_summary <- final_summary[order(final_summary$Parameter,final_summary$Value),]
final_summary <- final_summary %>% relocate("All Countries", .after="ZM")

readr::write_csv(final_summary, "./outputs/vpc_plots/gdl/summary/model_comparison_table_clean.csv")


# short_summary$estimate_string <- paste0(
#   overall_summary$Estimate," (err: ",overall_summary$Est.Error,", l95 CI: ",overall_summary$`l-95% CI`,", u95 CI: ",overall_summary$`u-95% CI`,")")
# 
# 
# short_summary <- tibble::as_tibble(list(
#   Model = short_summary$country,
#   param = short_summary$param_clean,
#   estimates = short_summary$estimate_string
#   ))
# 
# short_summary <- short_summary %>% pivot_wider(id_cols = "Model",names_from="param", values_from = "estimates")
# short_summary <- short_summary[c("Model",
#                                  "Intercept",
#                                  "Between Villages",
#                                  "Between Subcounty",
#                                  "Unexplained",
#                                  "Between Country")]



final_summary %>% pivot_longer(
  cols = BF:`All Countries`
) %>% pivot_wider(id_cols = c("Parameter","name"),names_from = "Value",values_from = "value")%>% 
  filter(name!="All Countries" & Parameter!="Between Country") %>% 
  ggplot(aes(x=Estimate,y=name))+
  geom_point()+
  geom_segment(aes(x=.data[["l-95% CI"]],xend=.data[["u-95% CI"]],y=name,yend=name))+
  facet_wrap(~Parameter)


