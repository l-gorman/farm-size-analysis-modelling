


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



indicator_data <- readr::read_csv("./prepared-data/rhomis-gaez-gdl.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data <- indicator_data[!is.na(indicator_data$village),]
# indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]


indicator_data$hfias_numeric<- NA
indicator_data$hfias_numeric[indicator_data$hfias_status=="severely_fi"] <- 1
indicator_data$hfias_numeric[indicator_data$hfias_status=="moderately_fi"] <- 2
indicator_data$hfias_numeric[indicator_data$hfias_status=="mildly_fi"] <- 3
indicator_data$hfias_numeric[indicator_data$hfias_status=="food_secure"] <- 4

projects_to_include <- c(
  "BF_CW2_2015",
  "ML_CW1_2015",
  "TZ_CFM_2015",
  "BF_SIL_2016",
  "ET_SRL_2016",
  "KE_SRL_2016",
  "TZ_SRL_2016",
  "KE_CM1_2016",
  "KE_CM2_2016",
  "CD_CLP_2017",
  "KE_VCD_2017",
  "ET_TA1_2017",
  "ML_TA3_2017",
  "UG_CWU_2017",
  
  "CD_FRT_2017",
  "KE_SCN_2017",
  "ZM_SCN_2017",
  "CR_IND_2017",
  
  "BF_GLD_2018",
  "ML_GLD_2018",
  "NE_GLD_2018",
  "BF_TA4_2018",
  "GH_TA7_2018",
  
  "CI_ARC_2018",
  "GH_ARC_2018",
  "KH_SIL_2018",
  "EC_CIP_2018",
  "SL_IFD_2018",
  "PS_FAO_2018",
  "SN_FTF_2018",
  "BI_CLP_2018",
  "ZM_GIZ_2018",
  "PE_MKP_2018",
  "TZ_CRA_2018",
  "BO_AID_2018",
  "KE_GLT_2019",
  "NG_GLT_2019",
  "ET_GTC_2019",
  "KE_GTC_2019",
  "IN_GEF_2018",
  "BF_ADN_2019",
  "BI_SNV_2019",
  "KE_LGS_2019",
  "MA_GDI_2019",
  "CD_LGS_2019",
  "ET_LGS_2020"
  
)

projects_with_control_groups <- c(
  "GH_TA2_2017",
  "IN_BIO_2018",
  "RW_OAF_2018",
  "ET_ARI_2018",
  "PS_FAO_2018",
  "ET_TA9_2019", # VTE group membership
  "GH_T10_2019", # VTE group membership
  "NE_TA8_2019",
  "GH_ADN_2019",
  "NE_T11_2019",# VTE group membership
  "MW_FAW_2019",
  "ZM_FAW_2019",
  "KM_DHA_2019",
  "ET_CAF_2020",
  "VN_CSI_2020",
  "KH_CSI_2020"
)

# Removing Datasets in bias
crit_1 <- (indicator_data$id_form %in% tolower(projects_to_include) | indicator_data$id_form %in% tolower(projects_with_control_groups) )
table(crit_1)
crit_2 <- !is.na(indicator_data$x_gps_latitude)&
  !is.na(indicator_data$x_gps_longitude) &
  !is.na(indicator_data$village)
table(crit_1&crit_2)

# crit_3 <-  (is.na(indicator_data$beneficiary) | indicator_data$beneficiary %in% c("control","n","core","no_participation","non_beneficiary"))
# table(crit_1&crit_2&crit_3)

crit_4 <- !is.na(indicator_data$hfias_status) 
table(crit_1&crit_2&crit_3&crit_4)



subset <- crit_1&crit_2&crit_4

indicator_data_test <- indicator_data[subset,]





# Adding Region Counts ----------------------------------------------------


dir.create("outputs/geographical_counts",showWarnings = F)


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Area Counts ------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
per_country_summary <- indicator_data %>% 
  group_by(iso_country_code) %>% summarise(
    subnational_areas = n_distinct(gdlcode),
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
  group_by(iso_country_code,gdlcode) %>% summarise(count = n_distinct(village)) 

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
  group_by(iso_country_code,gdlcode) %>% summarise(count = n_distinct(village)) 

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





