library(readr)
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)


lsms_data <- readr::read_csv("./prepared-data/lsms-ee-gaez.csv")
lsms_data <- lsms_data[!is.na(lsms_data$ADM2_NAME),]
unique(lsms_data$ADM2_NAME)
unique(lsms_data$ADM0_NAME)

arhomis_data <- readr::read_csv("./prepared-data/rhomis-ee-gaez.csv")
rhomis_data <- rhomis_data[!is.na(rhomis_data$ADM2_NAME),]


ggplot(lsms_data, aes(x=farm_size_ha,fill=factor(ADM2_CODE)))+
  geom_density(alpha = 0.1, bw=2)+
  theme(legend.position = "none")+
  xlim(c(0,10))+
  facet_wrap(~ADM0_NAME)+
  labs(title = "Density of Land Size (ha) per Administrative Area (GAUL Level 2)")

ggplot(lsms_data, aes(x=farm_size_ha))+
  geom_density(bw=1, color="black", fill="dodgerblue4")+
  theme(legend.position = "none")+
  xlim(c(0,10))+
  facet_wrap(~ADM0_NAME)+
  labs(title = "Density of Land Size (ha) per Administrative Area (GAUL Level 2)")



ggplot(rhomis_data, aes(x=land_cultivated_ha,fill=factor(ADM2_CODE)))+
  geom_density(alpha = 0.1, bw=1)+
  theme(legend.position = "none")+
  xlim(c(0,10))+
  facet_wrap(~ADM0_NAME)+
  labs(title = "Density of Land Size (ha) per Administrative Area (GAUL Level 2)")


ggplot(rhomis_data, aes(x=land_cultivated_ha))+
  geom_density(bw=1, color="black", fill="dodgerblue4")+
  theme(legend.position = "none")+
  xlim(c(0,10))+
  facet_wrap(~ADM0_NAME)+
  labs(title = "Density of Land Size (ha) per Administrative Area (GAUL Level 2)")


