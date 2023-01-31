library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggdist)
base_path <- "./"
# base_output_path <- paste0(base_path, "outputs/","gamlss_test/")
base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

# base_path <- "./"
# base_output_path <- paste0(base_path, "outputs/","gamlss_logno/")


final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))
final_df <- readr::read_csv(paste0(base_path,"prepared-data/rhomis-ee-gaez.csv"))
final_df$geo_id <- paste0(final_df$ADM0_CODE, "_",final_df$ADM1_CODE, "_",final_df$ADM2_CODE)
final_df<- final_df[!is.na(final_df$ADM0_CODE),]

farm_size_column <- "land_cultivated_ha"
# farm_size_column <- "farm_size_ha"

# final_df$farm_size_ha

median_no_na <- function(x){
  median(x,na.rm = T)
}


mean_no_na <- function(x){
  mean(x,na.rm=T)
}

sd_no_na <- function(x){
  sd(x,na.rm=T)
}

spread <- final_df %>% group_by(geo_id) %>% 
  summarise(across(farm_size_column, 
                   list(median =median_no_na, 
                        count = ~ n(), 
                        mean=mean_no_na, 
                        sd=sd_no_na)),
  )


median_col <- paste0(farm_size_column,"_median")
mean_col <- paste0(farm_size_column,"_mean")
sd_col <- paste0(farm_size_column,"_sd")
count_col <- paste0(farm_size_column,"_count")


spread$coefficient_of_variation <- spread[[sd_col]]/spread[[mean_col]]
spread <- spread %>% merge(final_df[c("ADM0_NAME","geo_id")], 
                           all.x = T, 
                           all.y = F, 
                           by.x="geo_id",
                           by.y="geo_id")
spread <- spread[duplicated(spread$geo_id)==F,]

ggplot(spread, aes_string(x=count_col,fill = "ADM0_NAME"))+
  geom_histogram(color="black",binwidth = 50, center=25)+ 
  labs(title = "Sample Size 250 Subnational Areas")


ggplot(spread, aes_string(x=mean_col))+
  geom_histogram(color="black")+ 
  xlim(0,8.5)+
  labs(title = "Mean Farm Size for 250 Subnational Areas")


ggplot(spread, aes_string(x=sd_col))+
  xlim(0,8.5)+
  
  geom_histogram(color="black")

ggplot(spread, aes(x=coefficient_of_variation))+
  geom_histogram(color="black")+ 
  xlim(c(0,4))+
  
  labs(title = "Coefficient of Variation  in Farm Size for 250 Subnational Areas")




ggplot(final_df, aes(x=farm_size_ha,y=ADM1_CODE))+
  stat_halfeye()+
  facet_wrap(~ADM0_NAME)

final_df$ADM2_CODE <- as.factor(final_df$ADM2_CODE)
ggplot(final_df, aes_string(x=farm_size_column,fill="ADM2_CODE"))+
  geom_density(alpha = 0.1)+
  theme(legend.position = "none")+
  xlim(c(0,10))+
  ylim(c(0,2.5))+
  facet_wrap(~ADM0_NAME)+
  labs(title = "Density of Land Size (ha) per Administrative Area (GAUL Level 2)")
