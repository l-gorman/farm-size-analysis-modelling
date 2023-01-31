library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
base_path <- "./"
base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

x=c("healthcare_traveltime",
    "nightlights",
    "population_density",
    
    
    "elevation",
    "ndvi",
    "topographic_diversity",
    "length_growing_season"
)

final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))

comparison_fits <- list()

combinations <- final_df[ c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]
combinations <- combinations[!duplicated(combinations),]

for (row in 1:nrow(combinations)){
  admin_info <- combinations[row, c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]
  
  subset_df <- final_df[
    final_df$ADM0_NAME==admin_info$ADM0_NAME&
      final_df$ADM1_NAME==admin_info$ADM1_NAME&
      final_df$ADM2_NAME==admin_info$ADM2_NAME,c(x,"farm_size_ha")]
  
  
  compare_fits <- fitDist(subset_df$farm_size_ha, type="realplus",n.cyc = 100)
  compare_fits <- as.data.frame(compare_fits$fits)
  colnames(compare_fits) <- "GAIC"
  compare_fits$model <- row.names(compare_fits)
  compare_fits <- compare_fits %>% pivot_wider(names_from ="model",values_from="GAIC")
  compare_fits <- cbind(admin_info,compare_fits)
  
  
  comparison_fits[[row]] <-compare_fits
  
  
                              
  
}


comparison_fits_combined <- comparison_fits %>% dplyr::bind_rows()
admin_columns <- c("ADM0_NAME","ADM1_NAME","ADM2_NAME")
reference_column <- c("BCCG")
other_columns <- colnames(comparison_fits_combined)[colnames(comparison_fits_combined) %in%admin_columns==F&
                                             colnames(comparison_fits_combined) %in%reference_column==F]

comparison_fits_combined[c(reference_column,other_columns)] <- comparison_fits_combined[[reference_column]]- comparison_fits_combined[c(reference_column,other_columns)]
comparison_fits_combined[c(reference_column,other_columns)]
comparison_fits_combined <- pivot_longer(data=comparison_fits_combined, 
                                cols = !c(ADM0_NAME,ADM1_NAME,ADM2_NAME),
                                names_to = "Model",values_to = "GAIC")

ggplot(comparison_fits_combined, aes(x=Model, y=GAIC))+
  geom_violin(outlier.shape = NA,scale = 3, draw_quantiles=c(0.5))+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
  ylim(-10,10)+
  labs(title="GAIC of Distribution Families Fit by Location", y='GAIC', x="Distribution Family")+
  theme(plot.title  = element_text(hjust=0.5))
