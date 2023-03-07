


#' Exploring and Analysing Farm-Size Dsitributions
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tibble)
library(tidyr)

library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(GGally)



indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data <- indicator_data[!is.na(indicator_data$village),]
# indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]


indicator_data$hfias_numeric<- NA
indicator_data$hfias_numeric[indicator_data$hfias_status=="severely_fi"] <- 1
indicator_data$hfias_numeric[indicator_data$hfias_status=="moderately_fi"] <- 2
indicator_data$hfias_numeric[indicator_data$hfias_status=="mildly_fi"] <- 3
indicator_data$hfias_numeric[indicator_data$hfias_status=="food_secure"] <- 4



# Adding Region Counts ----------------------------------------------------


dir.create("outputs/geographical_counts")


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Area Counts ------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
per_country_summary <- indicator_data %>% 
  group_by(iso_country_code) %>% summarise(
    subnational_areas = n_distinct(ADM2_CODE),
    villages = n_distinct(village),
    households = n()
  )  

new_row <- colSums(per_country_summary[c("subnational_areas","villages","households")]) %>% as.list()
new_row$iso_country_code <- "Total"
new_row <- tibble::as_tibble(new_row)
per_country_summary <- per_country_summary %>% bind_rows(new_row)

readr::write_csv(per_country_summary,"outputs/geographical_counts/per_country_summary.csv")


# Areas_per_country -------------------------------------------------------
per_country_summary_plot_df <- per_country_summary[per_country_summary$iso_country_code!="Total",]
per_country_summary_plot_df <- pivot_longer(per_country_summary, cols = c("subnational_areas",
                                                                          "villages",
                                                                          "households"))
temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=subnational_areas))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Subnational Areas in per Country",x="Country", y="Subnational Areas")
ggsave("outputs/geographical_counts/subnation_areas_per_country.png",temp)


temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=villages))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Villages  per Country",x="Country", y="Number of Villages")
ggsave("outputs/geographical_counts/villages_per_country.png",temp)

temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=households))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Surveys per Country",x="Country", y="Number of Surveys")
ggsave("outputs/geographical_counts/surveys_per_country.png",temp)


# Villages_per_area -------------------------------------------------------
villages_per_area <- indicator_data %>% 
  group_by(iso_country_code,ADM2_CODE) %>% summarise(count = n_distinct(village)) 

temp <- ggplot(villages_per_area,
      aes(x=iso_country_code,y=count))+
  geom_boxplot(fill="dodgerblue4", colour="black", outlier.shape = NA)+
  scale_y_continuous(limits = quantile(villages_per_area$count, c(0, 0.99)))+
  labs(title="Villages Per Subnational Area",x="Country", y="Number of Villages in Subnational Area")
ggsave("outputs/geographical_counts/villages_per_subnational_region.png",temp)

#-------------------------------------------------------------------------
indicator_data$village[indicator_data$iso_country_code=="NG"]
#-------------------------------------------------------------------------
# People Counts ------------------------------------------------------------

villages_per_area <- indicator_data %>% 
  group_by(iso_country_code,ADM2_CODE) %>% summarise(count = n_distinct(village)) 

ggplot(villages_per_area,
       aes(x=iso_country_code,y=count))+
  geom_boxplot(fill="dodgerblue4", colour="black", outlier.shape = NA)+
  scale_y_continuous(limits = quantile(villages_per_area$count, c(0, 0.99)))+
  labs(title="Villages Per Subnational Area",x="Country", y="Number of Villages in Subnational Area")
ggsave("outputs/geographical_counts/villages_per_subnational_region.png",temp)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# people_per_village ------------------------------------------------------


# people_per_county -------------------------------------------------------



unique(temp$country_count)

table(temp)


