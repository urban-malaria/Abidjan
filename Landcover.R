############LANDCOVER

source("load_path.R", echo=FALSE)
source("~/Abidjanf/load_path.R", echo=FALSE)

#install.packages("rgdal")
#library(rgdal)
install.packages("hdf5r") 
library(hdf5r) 
library(terra)

LandcoverDir <- file.path(Earthdata, "MODIS-TERRA_LandCoverType_Yearly_Global_500m_2013-2023")


landcover_files  = list.files( file.path(LandcoverDir), 
                               pattern = ".hdf", full.names = TRUE)


# Read the files

raster_data10  = lapply(seq_along(landcover_files), 
                        function(x) rast(landcover_files[x]))
# compute mode
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

landcover_mode <- raster_data10 %>%
  purrr::map(~raster::extract(., df_abidjan1, fun = compute_mode))

landcovertest <- landcover_mode[[1]]


##################  Dictionary###################################################
#### Rename column headings
new_column_names <- c("HealthDistrict", "IGBP_Classification", "UMD_Classification", 
                      "LAI_Classification", "BGC_Classification", "PFT_Classification", 
                      "LCCS1_Layer_confidence", "LCCS2_Layer_confidence", 
                      "LCCS3_Layer_confidence", "LCCS1_land_cover_layer", "LCCS2_land_use_layer",
                      "LCCS3_surface_hydrology_layer", "Quality", "Land_Water_Mask")

landcover_mode <- lapply(landcover_mode, function(df) {
 names(df) <- new_column_names
  return(df)
})

# cell Value translations
translation_list <- list(
  `HealthDistrict` = c("1" = "YOPOUGON-OUEST-SONGON", "2" = "ADJAME-PLATEAU-ATTECOUBE", "3" = "COCODY BINGERVILLE", "4" = "ANYAMA", "5" = "PORT BOUET-VRIDI", "6" = "ABOBO EST", "7" = "ABOBO OUEST", "8" = "KOUMASSI", "9" = "TREICHVILLE-MARCORY", "10" = "YOPOUGON-EST", "11" = "JACQUEVILLE", "12" = "DABOU", "13" = "ALEPE", "14" = "GRAND-BASSAM", "15" = "AGBOVILLE"),
  `IGBP_Classification` = c("1" = "Evergreen Needleleaf Forests", "2" = "Evergreen Broadleaf Forests", "3" = "Deciduous Needleleaf Forests", "4" = "Deciduous Broadleaf Forests", "5" = "Mixed Forests", "6" = "Closed Shrublands", "7" = "Open Shrublands", "8" = "Woody Savannas", "9" = "Savannas", "10" = "Grasslands", "11" = "Permanent Wetlands", "12" = "Croplands", "13" = "Urban and Built-up Lands", "14" = "Cropland/Natural Vegetation Mosaics", "15" = "Permanent Snow and Ice", "16" = "Barren", "17" = "Water Bodies", "255" = "Unclassified"),
  `UMD_Classification` = c("0" = "Water Bodies", "1" = "Evergreen Needleleaf Forests", "2" = "Evergreen Broadleaf Forests", "3" = "Deciduous Needleleaf Forests", "4" = "Deciduous Broadleaf Forests", "5" = "Mixed Forests", "6" = "Closed Shrublands", "7" = "Open Shrublands", "8" = "Woody Savannas", "9" = "Savannas", "10" = "Grasslands", "11" = "Permanent Wetlands", "12" = "Croplands", "13" = "Urban and Built-up Lands", "14" = "Cropland/Natural Vegetation Mosaics", "15" = "Non-Vegetated Lands", "255" = "Unclassified"),
  `LAI_Classification` = c("0" = "Water Bodies", "1" = "Grasslands", "2" = "Shrublands", "3" = "Broadleaf Croplands", "4" = "Savannas", "5" = "Evergreen Broadleaf Forests", "6" = "Deciduous Broadleaf Forests", "7" = "Evergreen Needleleaf Forests", "8" = "Deciduous Needleleaf Forests", "9" = "Non-Vegetated Lands", "10" = "Urban and Built-up Lands", "255" = "Unclassified"),
  `BGC_Classification` = c("0" = "Water Bodies", "1" = "Evergreen Needleleaf Vegetation", "2" = "Evergreen Broadleaf Vegetation", "3" = "Deciduous Needleleaf Vegetation", "4" = "Deciduous Broadleaf Vegetation", "5" = "Annual Broadleaf Vegetation", "6" = "Annual Grass Vegetation", "7" = "Non-Vegetated Lands", "8" = "Urban and Built-up Lands", "255" = "Unclassified"),
  `PFT_Classification` = c("0" = "Water Bodies", "1" = "Evergreen Needleleaf Trees", "2" = "Evergreen Broadleaf Trees", "3" = "Deciduous Needleleaf Trees", "4" = "Deciduous Broadleaf Trees", "5" = "Shrub", "6" = "Grass", "7" = "Cereal Croplands", "8" = "Broadleaf Croplands", "9" = "Urban and Built-up Lands", "10" = "Permanent Snow and Ice", "11" = "Barren", "255" = "Unclassified"),
  `LCCS1_land_cover_layer` = c("1" = "Barren", "2" = "Permanent Snow and Ice", "3" = "Water Bodies", "11" = "Evergreen Needleleaf Forests", "12" = "Evergreen Broadleaf Forests", "13" = "Deciduous Needleleaf Forests", "14" = "Deciduous Broadleaf Forests", "15" = "Mixed Broadleaf/Needleleaf Forests", "16" = "Mixed Broadleaf Evergreen/Deciduous Forests", "21" = "Open Forests", "22" = "Sparse Forests", "31" = "Dense Herbaceous", "32" = "Sparse Herbaceous", "41" = "Dense Shrublands", "42" = "Shrubland/Grassland Mosaics", "43" = "Sparse Shrublands", "255" = "Unclassified"),
  `LCCS2_land_use_layer` = c("1" = "Barren", "2" = "Permanent Snow and Ice", "3" = "Water Bodies", "9" = "Urban and Built-up Lands", "10" = "Dense Forests", "20" = "Open Forests", "25" = "Forest/Cropland Mosaics", "30" = "Natural Herbaceous", "35" = "Natural Herbaceous/Croplands Mosaics", "36" = "Herbaceous Croplands", "40" = "Shrublands", "255" = "Unclassified"),
  `LCCS3_surface_hydrology_layer` = c("1" = "Barren", "2" = "Permanent Snow and Ice", "3" = "Water Bodies", "10" = "Dense Forests", "20" = "Open Forests", "27" = "Woody Wetlands", "30" = "Grasslands", "40" = "Shrublands", "50" = "Herbaceous Wetlands", "51" = "Tundra", "255" = "Unclassified"),
  `Land_Water_Mask`= c("1" = "Water", "2" = "Land"),
  `Quality` = c("0" = "Classified land", "1" = "Unclassified land", "2" = "Classified water", "3" = "Unclassified water", "4" = "Classified sea ice", "5" = "Misclassified water", "6" = "Omitted snow/ice", "7" = "Misclassified snow/ice", "8" = "Backfilled label", "9" = "Forest type changed", "10" = "No data")
)
landcover_mode <- lapply(landcover_mode, function(df) {
  for (col_name in names(df)) {
    if (col_name %in% names(translation_list)) {
      for (i in seq_along(df[[col_name]])) {
        if (!is.na(df[[col_name]][i])) {
          translated_value <- translation_list[[col_name]][as.character(df[[col_name]][i])]
          if (!is.na(translated_value)) {
            df[[col_name]][i] <- translated_value
          }
        }
      }
    }
  }
  return(df)
})

#load landcover data
landcover_all <- list()
for (year in 2013:2022) {
  landcover_data <- landcover_mode[[year - 2012]]  
    landcover_data$Year <- year
    landcover_all[[year - 2012]] <- landcover_data
}
landcover_all2 <- do.call(rbind, landcover_all)

landcover_plottingdata <- inner_join(df_abidjan1, landcover_all2, by = c("NOM" = "HealthDistrict"))

#####PLOTS
#land cover type classifications
ggplot()+
  geom_sf(data = landcover_plottingdata, aes(geometry = geometry, fill = Quality)) + #replace fill = with classification of interest
  facet_wrap(~Year, ncol = 3)+#
  labs(title = "Land Cover Types in Abidjan", fill = "Quality", x = NULL, y = NULL) +
  map_theme() 


## proportion of confidences

confidence_proportions <- landcover_plottingdata %>%
  group_by(NOM, LCCS1_Layer_confidence) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(confidence_proportions, aes(x = NOM, y = proportion, fill = factor(LCCS1_Layer_confidence))) +
  geom_bar(stat = "identity") +
  labs(title = "Proportions of LCCS1 Layer Confidence in Health Districts",
       x = "Health District",
       y = "Proportion",
       fill = "Confidence Level") +
  scale_fill_discrete(name = "Confidence Level") +
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


