# Geo Packages
library(sf)
library(sp)
library(stars)
library(geojsonsf)
library(corrplot)
library(raster)
library(leaflet)
library(mapview)
library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
# # Graph Plotting
library(ggplot2)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(GGally)
library(dplyr)


# Loading Data ------------------------------------------------------------


indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data_geo <- st_as_sf(indicator_data, coords = c( "x_gps_longitude","x_gps_latitude"), 
                               crs = 4326, agr = "constant", remove = F)

rhomis_data <- readr::read_csv("./data/raw-data/rhomis/processed_data.csv")
rhomis_data <- rhomis_data[!is.na(rhomis_data$x_gps_latitude) & !is.na(rhomis_data$x_gps_longitude),]
rhomis_data <- rhomis_data[!is.na(rhomis_data$village),]


rhomis_data_geo<- st_as_sf(indicator_data, coords = c("x_gps_longitude","x_gps_latitude"), 
                               crs = 4326, agr = "constant", remove = F)



# FAO administrative data
fao_level_2 <- geojson_sf('data/raw-data/earth-engine/fao-gaul-level-2.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2_geo <-st_set_crs(fao_level_2,'EPSG:4326')
# fao_level_2_geo <- st_simplify(fao_level_2_geo, 10000)
fao_level_2 <- tibble::as_tibble(fao_level_2)





world <- ne_countries(scale = "medium", returnclass = "sf")
world$highlight <- world$wb_a2 %in% indicator_data$iso_country_code



# Continent Plot ----------------------------------------------------------


ggplot(data = world[world$continent=="Africa",]) +
  geom_sf(aes(fill=highlight), color="black")+
  scale_fill_manual(values = c("white", "dodgerblue4"))+
  theme_void() + theme(legend.position="none")




# Country Plot ------------------------------------------------------------
ggplot(data = fao_level_2_geo[fao_level_2_geo$ADM0_NAME=="Rwanda",]) +
  geom_sf()+
  theme_void() + theme(legend.position="none")


# Village Plot ------------------------------------------------------------

village_points <- rhomis_data %>% 
  filter(country=="rwanda")%>% 
  group_by(village) %>% 
  summarise(x_gps_latitude=mean(x_gps_latitude, na.rm=T),
            x_gps_longitude=mean(x_gps_longitude, na.rm=T))

village_points <- st_as_sf(village_points, coords = c( "x_gps_longitude","x_gps_latitude"), 
                        crs = 4326, agr = "constant", remove = F)


ggplot() +
  geom_sf(data = fao_level_2_geo[fao_level_2_geo$ADM0_NAME=="Rwanda",])+
  geom_sf(data=village_points)+
  theme_void() + theme(legend.position="none")


ggplot() +
  geom_sf(data = fao_level_2_geo[fao_level_2_geo$ADM0_NAME=="Rwanda",])+
  geom_sf(data=st_jitter(indicator_data_geo[indicator_data_geo$iso_country_code=="RW",], factor=0.01))+
  theme_void() + theme(legend.position="none")













