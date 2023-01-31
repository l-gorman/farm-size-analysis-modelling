
# Checking Normality


indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
# indicator_data_geo <- st_as_sf(indicator_data, coords = c("x_gps_latitude", "x_gps_longitude"), 
#                                crs = 4326, agr = "constant", remove = F)


indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.infinite(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[indicator_data$land_cultivated_ha>0,]

indicator_data <- indicator_data[!is.na(indicator_data$village),]

log_land_cult <- log(indicator_data["land_cultivated_ha"])
log_land_cult <- log_land_cult[!is.na(log_land_cult)]
colnames(log_land_cult)<- "log_farm_size"
ggplot(log_land_cult, aes(x=log_farm_size))+
  geom_histogram()

qqnorm(y = log_land_cult)


shapiro.test(log_land_cult[1:5000])



