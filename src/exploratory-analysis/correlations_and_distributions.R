#' Exploring and Analysing Farm-Size Dsitributions
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tibble)
library(jsonlite)
library(tidyr)

# Spatial Packages
library(sf)
library(sp)
library(stars)
library(geojsonsf)
library(corrplot)
library(raster)
library(leaflet)
library(mapview)

# Graph Plotting
library(ggplot2)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(GGally)

# library(moments) # Package for skewness
library(gamlss) # Gaussian additive models for location, scale, and shape
# library(bamlss) # Bayesian additive models for location, scale, and shape
library(brms) # General package for bayesian models with lme4 syntax and stan backend
library(lme4) # General package for bayesian multi-level modelling.
library(FactoMineR) # Package for pca
library(factoextra) # Extra pca features


indicator_data <- readr::read_csv("./prepared-data/rhomis-gaez-gdl.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data_geo <- st_as_sf(indicator_data, coords = c("x_gps_latitude", "x_gps_longitude"), 
                               crs = 4326, agr = "constant", remove = F)





outlier_filter <- quantile(indicator_data[["land_cultivated_ha"]], probs = c(0.01,0.99))

table(indicator_data[c("land_cultivated_ha")]==outlier_filter[1]) # 236 with 0 land cult (investigate further)
table(indicator_data[c("land_cultivated_ha")]>outlier_filter[2]) # 201 with land cult greater than 99th percentile (24th)


indicator_data <- indicator_data[indicator_data[c("land_cultivated_ha")] != outlier_filter[1] & indicator_data[c("land_cultivated_ha")] <= outlier_filter[2],]


# 

indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]


indicator_data$hfias_numeric<- NA
indicator_data$hfias_numeric[indicator_data$hfias_status=="severely_fi"] <- 1
indicator_data$hfias_numeric[indicator_data$hfias_status=="moderately_fi"] <- 2
indicator_data$hfias_numeric[indicator_data$hfias_status=="mildly_fi"] <- 3
indicator_data$hfias_numeric[indicator_data$hfias_status=="food_secure"] <- 4






# Plotting Correlations --------------------------------------------------

colnames(indicator_data)
indicator_data$sgdi
x <- c("travel_time_to_cities_9",
       "travel_time_to_cities_8",
       "travel_time_to_cities_7.2",
       "travel_time_to_cities_7.1",
       
       "shdi",
       "healthindex",
       "incindex",
       "edindex",
       "esch",
       "msch",
       "lifexp",
       "gnic",
       "lgnic",
       
       "adjusted_length_growing_period"
       
       

       
       
)


aez_33 <- grep("AEZ_Classes_33_", colnames(indicator_data), value=T)




y <-c("land_cultivated_ha")
# y <-c("hfias_numeric")



# Looking at subnational indicators vas land cultivated
cor(indicator_data[c(x,aez_33,y)])

corr_matrix <- round(cor(indicator_data[c(x,aez_33,y)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")


land_cult_corr <- corr_matrix[corr_matrix$var1==y, ]
land_cult_corr <- land_cult_corr[land_cult_corr$var2!=y,]

land_cult_corr$var2 <- gsub("AEZ_Classes_33_", "",land_cult_corr$var2)

land_cult_corr$var2 <- factor(land_cult_corr$var2, levels=c(x,gsub("AEZ_Classes_33_", "",aez_33)), ordered = T)
land_cult_corr$factor_level <- as.numeric(land_cult_corr$var2)

socio_economic <- c("shdi",
                    "healthindex",
                    "incindex",
                    "edindex",
                    "esch",
                    "msch",
                    "lifexp",
                    "gnic",
                    "lgnic",
                    "travel_time_to_cities_9",
                    "travel_time_to_cities_8",
                    "travel_time_to_cities_7.2",
                    "travel_time_to_cities_7.1")

environmental <- c("adjusted_length_growing_period")




land_cult_corr$var_group <- NA
land_cult_corr$var_group[land_cult_corr$var2 %in% socio_economic] <- "Socio-Economic"
land_cult_corr$var_group[land_cult_corr$var2 %in% environmental] <- "Environmental"
land_cult_corr$var_group[land_cult_corr$var2 %in% gsub("AEZ_Classes_33_", "",aez_33)] <- "Agro-Eco-Zone"

land_cult_corr$var_group <- factor(land_cult_corr$var_group, 
                                      levels=c("Agro-Eco-Zone",
                                               "Environmental",
                                               "Socio-Economic"),
                                      ordered=T, )

land_cult_corr <- land_cult_corr[!is.na(land_cult_corr$value),]

colors <- brewer.pal(length(unique(land_cult_corr$var_group)), name="Dark2")


land_cult_corr <- land_cult_corr %>%
  mutate(., color = with(., case_when(
    (var_group=="Socio-Economic") ~ colors[3],
    (var_group=="Environmental")  ~ colors[2],
    (var_group=="Agro-Eco-Zone")  ~ colors[1]
  )))


ggplot(data = land_cult_corr, aes(y=var2, x=value)) + 
  geom_segment( aes(x=0, xend=value, y=var2, yend=var2), color="black")+
  geom_point( size=4, aes(x=value, color=var_group)) +
  scale_color_manual(values=colors) +
  
  coord_cartesian(xlim = c(-0.4, 0.4), # This focuses the x-axis on the range of interest
                  clip = 'off')+
 
labs(title="Correlations with HFIAS",
     x ="Pearsons Correlation Coeff", y ="Landscape predictiors",
     color="Variable Class",
     caption = "\nLandscape predictors sourced from GAEZ v4 and Google Earth Engine.
     Environmental, land-cover, and socio-economic data aggregated to FAO GAUL level 2.
     Length growing period and AEZ classes averaged for subnational area.
     AEZ simplified 33 class has been used, only 10/33 AEZs featured in the data.
     Correlations represent correlation between household level land cultivated (ha)
     and aggregated landscape variable.
     Outliers have been removed using 99th percentile (>24ha)*
     ") +
   theme(text = element_text(size = 16),
         axis.text.y = element_text(colour = land_cult_corr$color)
   )           



# Corellation with Household Level
x_hh <- c("hh_size_mae",
          "livestock_tlu",
          "land_cultivated_ha",
          "ppi_likelihood")

test_df <- indicator_data[c(x_hh,y)]
test_df <- test_df[complete.cases(test_df),]

corr_hh <- round(cor(test_df),2) %>% tibble::as_tibble()

corr_hh$var <- colnames(corr_hh)
corr_hh <- corr_hh %>% pivot_longer(cols = colnames(corr_hh)[colnames(corr_hh)!="var"])

colnames(corr_hh) <- c("var1", "var2", "value")
corr_hh <- corr_hh[corr_hh$var1==y, ]
corr_hh <- corr_hh[corr_hh$var2!=y,]

# Correlation with AEZ classes
y_index <- which(colnames(indicator_data)==y)
corr_matrix <- round(cor(indicator_data[c(aez_33,y_index)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")


land_cult_corr <- corr_matrix[corr_matrix$var1=="land_cultivated_ha", ]
land_cult_corr <- land_cult_corr[land_cult_corr$var2!="land_cultivated_ha",]
land_cult_corr$var2 <- gsub("AEZ_Classes_33_", "",land_cult_corr$var2)
land_cult_corr <- land_cult_corr[!is.na(land_cult_corr$var2),]
land_cult_corr <- land_cult_corr[!is.na(land_cult_corr$value),]

ggplot(data = land_cult_corr, aes(y=var2, x=value)) + 
  geom_segment( aes(x=0, xend=value, y=var2, yend=var2), color="black")+
  geom_point( size=2, aes(x=value), color="Orange") +
  coord_cartesian(xlim = c(-0.3, 0.3), # This focuses the x-axis on the range of interest
                  clip = 'off')+
  
  labs(title="Correlations with Land Cultivated (ha)",
       x ="Pearsons Correlation Coeff", y ="Agro-Ecological Zone",

       caption = "\nAEZ zones sourced from GAEZ v4 (simplified 33 class).
     Households in the dataset covered 10 out of the 33 zones.
     **Outliers have been removed using 99th percentile (>24ha)
     ") 

# Looking at Land Cult Stats ----------------------------------------------

land_cult_summary <- indicator_data %>% 
  dplyr::group_by(
    geo_id
  ) %>% 
  summarise(
    land_cult_mean = mean(land_cultivated_ha, na.rm=T),
    land_cult_stdev = sd(land_cultivated_ha, na.rm=T),
    land_cult_med = median(land_cultivated_ha, na.rm=T),
    land_cult_iqr = IQR(land_cultivated_ha, na.rm=T),
    land_cult_q_0.25 = quantile(land_cultivated_ha, probs=c(0.25),na.rm=T),
    land_cult_q_0.75 = quantile(land_cultivated_ha, probs=c(0.75),na.rm=T),
    land_cult_skew = moments::skewness(land_cultivated_ha, na.rm=T),
    land_cult_kurtosis = moments::kurtosis(land_cultivated_ha, na.rm=T)
  )


fao_level_2_land_cult <- merge(land_cult_summary,
                               fao_level_2, 
                               by="geo_id",
                               all.x=T,
                               all.y=F) %>% tibble::as_tibble()

colSums(is.na(fao_level_2))

y_subn <- c(
  "land_cult_mean",
  "land_cult_stdev",
  "land_cult_med",
  "land_cult_iqr",
  "land_cult_q_0.25",
  "land_cult_q_0.75"
)
x_subn <- x[x %in% colnames(fao_level_2_land_cult)]


fao_level_2_land_cult[c(x_subn,y_subn)] <- lapply(fao_level_2_land_cult[c(x_subn,y_subn)], 
                                                  function(x){
                                                    as.numeric(x)
                                                  }) %>% dplyr::bind_cols()

colSums(is.na(fao_level_2_land_cult))

corr_matrix <- round(cor(fao_level_2_land_cult[c(x_subn,y_subn)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")

corr_matrix <- corr_matrix[corr_matrix$var1 %in% x_subn==F,]
corr_matrix <- corr_matrix[corr_matrix$var2 %in% y_subn==F,]

ggplot(data = corr_matrix, aes(x=var2, y=var1, fill=value)) +
  geom_tile() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1,
                                    hjust = 1))


# Reducing layers

FactoMineR::PCA(fao_level_2_land_cult[x_subn], scale.unit=TRUE, ncp=5, graph=T)

table(indicator_data$AEZ_Classes_33)

