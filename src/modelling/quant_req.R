library(quantreg)
library(factoextra)
library(FactoMineR)
library(ggplot2)


indicator_data <- readr::read_csv("./data/prepped-data/rhomis-ee-gaez.csv")
indicator_data <- indicator_data[!is.na(indicator_data$gps_lat) & !is.na(indicator_data$gps_lon),]
indicator_data_geo <- st_as_sf(indicator_data, coords = c("gps_lon", "gps_lat"), 
                               crs = 4326, agr = "constant", remove = F)





# Cleaning Column Names ---------------------------------------------------


land_categories <-  readr::read_csv("./data/prepped-data/land_cover_classes.csv")

land_cat_columns <- paste0("land_cat_",c(1:17))
indicator_data[land_cat_columns] <- lapply(indicator_data[land_cat_columns] , function(column){
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/indicator_data$pixelCount
  return(column)
}) %>% dplyr::bind_cols()


new_land_cat_columns <- land_categories$Tag
colnames(indicator_data)[colnames(indicator_data) %in% land_cat_columns] <- new_land_cat_columns
colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns

colnames(indicator_data)[colnames(indicator_data) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(indicator_data)[colnames(indicator_data) == "b1_mean_mean"] <- "nightlights"
colnames(indicator_data)[colnames(indicator_data) == "population_density_mean_mean"] <- "population_density"

colnames(indicator_data)[colnames(indicator_data) == "elevation_mean"] <- "elevation"
colnames(indicator_data)[colnames(indicator_data) == "NDVI_mean_mean"] <- "ndvi"
colnames(indicator_data)[colnames(indicator_data) == "constant_mean"] <- "topographic_diversity"

colnames(fao_level_2)[colnames(fao_level_2) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(fao_level_2)[colnames(fao_level_2) == "b1_mean_mean"] <- "nightlights"
colnames(fao_level_2)[colnames(fao_level_2) == "population_density_mean_mean"] <- "population_density"

colnames(fao_level_2)[colnames(fao_level_2) == "elevation_mean"] <- "elevation"
colnames(fao_level_2)[colnames(fao_level_2) == "NDVI_mean_mean"] <- "ndvi"
colnames(fao_level_2)[colnames(fao_level_2) == "constant_mean"] <- "topographic_diversity"

indicator_data$geo_id <- paste0(indicator_data$ADM0_CODE, "_", 
                                indicator_data$ADM1_CODE, "_",
                                indicator_data$ADM2_CODE)


# Remove NAs

indicator_data <- indicator_data[!is.na(indicator_data$land_cultivated_ha),]
indicator_data <- indicator_data[!is.na(indicator_data$AEZ_Classes_33),]


# Identifying X and Y Vars ------------------------------------------------



x <- c("healthcare_traveltime",
       "nightlights",
       "population_density",
       
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "adjusted_length_growing_period",
       
       
       
       
       
       new_land_cat_columns
)







aez_33 <- grep("AEZ_Classes_33_", colnames(indicator_data), value=T)


arcsine_transform <- function(column){
  return(asin(sqrt(column)))
}

indicator_data[c(aez_33, new_land_cat_columns)] <- lapply(indicator_data[c(aez_33, new_land_cat_columns)],
                                                          function(x){
                                                            arcsine_transform(x)
                                                          })



# PCA

pca.res <- PCA(X = indicator_data[c("healthcare_traveltime",
                                    "nightlights",
                                    "population_density",
                                    
                                    
                                    "elevation",
                                    "ndvi",
                                    "topographic_diversity",
                                    "adjusted_length_growing_period")])

pca.res <- PCA(indicator_data[new_land_cat_columns])
pca.res <- PCA(indicator_data[c(x,aez_33,y)])

head(pca.res$eig[, 1:2])
barplot(pca.res$eig[, 2], names.arg=1:nrow(pca.res$eig), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")





y <-c("land_cultivated_ha")


results <- as.data.frame(pca.res$ind$coord)
colnames(results) <- paste0("PC_", c(1:ncol(results)))
indicator_data <- cbind(indicator_data,results)



pc_formula <- as.formula(paste0("land_cultivated_ha ~ " ,paste0("PC_", c(1:ncol(results)), collapse = " + ")))

model_pc_0.5<-quantreg:rq(formula= pc_formula, 
                    tau=0.5,
                    data=indicator_data[c(paste0("PC_", c(1:ncol(results))),y)])

summary(model_pc_0.5,se = "boot", bsmethod= "xy")



model_pc_0.1<-quantreg::rq(formula= pc_formula, 
                           tau=0.1,
                           data=indicator_data[c(paste0("PC_", c(1:ncol(results))),y)])
summary(model_pc_0.5)
summary(model_pc_0.1)


ggplot(indicator_data, aes(x=population_density, y=land_cultivated_ha))+
  geom_point() +
  ylim(0,50)

ggplot(indicator_data, aes(x=ndvi, y=land_cultivated_ha))+
  geom_point() +
  ylim(0,50)



model<-quantreg::rq(formula= as.formula(land_cultivated_ha ~ 1), 
                    tau=0.5,
                    data=indicator_data[c(x,aez_33,y)])








formula <- as.formula(paste0(" ~ ", paste0(x, collapse=" + ")))


# y_formula <- as.formula(paste0("land_cultivated_ha ~ ", paste0(x, collapse=" + ")))
# model<-quantreg::rq(formula= y_formula, 
#                     tau=0.5,
#                     data=indicator_data[c(x,y)],
#                     method="lasso")



# For interaction affects
#formula <- as.formula(paste0(" ~ ", paste0(x, collapse=" * "))) 


aic_fit <- MASS::stepAIC(model, 
              scope=list(upper = formula, lower = ~1),direction = "both")

aic_fit$anova

rq.fit.lasso(x=indicator_data[c(x,aez_33)], y=indicator_data[[y]],)
# 
# 
# 
# 
# summary <- indicator_data %>% 
#   group_by(geo_id) %>% 
#   summary(
#     mean=mean(land_cultivated_ha,na.rm=T)
#   )


