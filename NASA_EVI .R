source("load_path.R", echo=FALSE) 
#source("C:/Users/hp/Abidjan/load_path.R", echo=FALSE) #leave commented#


NASAdata <- file.path(AbidjanDir, "Autonome D_Abidjan")
Earthdata <- file.path(NASAdata, "EarthData")
EVIkm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_1km_Monthly_2013-23")
EVIm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_500m_16d_2013-23")
NDVIkm <- file.path(Earthdata,"MODIS-TERRA_VegetationIndex_NDVI_1km_Monthly_2013-23")
Rainfall2013_23 <- file.path(NASAdata, "Rainfall 2013-2023")
Climatedata <- file.path(NASAdata, "ClimateSERV")
Abidjanmap1 <- file.path(NASAdata, "Autonome D_Abidjan2.geojson")


Abidjanmap <- st_read(Abidjanmap1)


Abidjan = Abi_shapefile[[3]] %>%
  filter(NAME_1 == "Abidjan")

df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)

install.packages("reshape")
library(reshape)


##############################################################################################################################################################
# EVI and NDVI Analysis (Enhanced Vegetation Index and Normalized Difference Vegetation Index)
###############################################################################################################################################################

####### combined 1km EVI values from 2013-2023

files_names10  = list.files( file.path(EVIkm), 
                           pattern = ".tif", full.names = TRUE)

raster_data10 = lapply(seq_along(files_names10), 
                     function(x) raster::raster(files_names10[[x]]))

EVI_data10 = raster_data10 %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))


#all the EVI data summary
df_abidjan1$meanEVI <- rowMeans(EVI_data10[, 2:133], na.rm=TRUE)

evi_plottingdata10 <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanEVI, c(0, 0.05, 0.1, 0.16, 0.2,
                                0.25, 0.3, 0.4), include.lowest = T ))

ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = evi_plottingdata10, aes(geometry = geometry, fill = meanEVI)) +
  scale_fill_continuous(name="enhance vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(subtitle = '', fill = "", x = NULL, y = NULL) +
  map_theme() 


combined_data <- df_abidjan1 %>% 
  st_drop_geometry() %>% 
  dplyr::select(NOM, meanEVI)


#write.csv(combined_data, file.path("Abidjan Data Variables.csv"), row.names = FALSE)


##########################################################################################
####### Analysis
##########################################################################################

column_names <- expand.grid(mon = month.abb, yr = 2013:2023) %>% 
  mutate(column_name = paste0(mon, "_", yr))
  

names(EVI_data10) <- c("ID", column_names$column_name)

EVI_data <- combined_data %>% 
  cbind(EVI_data10) %>% 
  reshape::melt(id = c("NOM", "ID")) %>% 
  filter(variable != "meanEVI") %>% 
  tidyr::separate(variable, into = c("month", "year"), sep = "_") %>% 
  group_by(NOM, year) %>% 
  mutate(yearly_meanEVI = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(NOM) %>%
  mutate(overall_meanEVI = mean(value, na.rm = T))
  

# write.csv(EVI_data, file.path(EVIkm , "overall_EVI_data.csv"), row.names = FALSE)

EVI_plottingdata <- inner_join(df_abidjan1, EVI_data )

##########################################################################################
####### Plots
##########################################################################################


ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = EVI_plottingdata, aes(geometry = geometry, fill = yearly_meanEVI )) +
  facet_wrap(~year)+
  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "EVI (1KM)", fill = "", x = NULL, y = NULL) +
  map_theme() 



ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = EVI_plottingdata, aes(geometry = geometry, 
                                       fill = overall_meanEVI )) +
  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "enhanced vegetation index (1KM)", fill = "", x = NULL, y = NULL) +
  map_theme()

##############################################
#### 500M Range EVI DATA###################
#############################################

raster_files  = list.files( file.path(EVIm), 
                            pattern = ".tif", full.names = TRUE)

raster_data500m = lapply(seq_along(raster_files), 
                         function(x) raster::raster(raster_files[[x]]))

EVI_data500m = raster_data500m %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))


#Summary of EVI data 500m
df_abidjan1$meanEVI500m <- rowMeans(EVI_data500m[, 2:254], na.rm=TRUE)

evi500m_plottingdata <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanEVI500m, c(0, 0.05, 0.1, 0.16, 0.2,
                                    0.25, 0.3, 0.4), include.lowest = T ))

ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = evi500m_plottingdata, aes(geometry = geometry, fill = meanEVI500m)) +
  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = 'EVI 500M 2013-2023', fill = "", x = NULL, y = NULL) +
  map_theme() 

combined_data500m <- df_abidjan1 %>% 
  st_drop_geometry() %>% 
  dplyr::select(NOM, meanEVI500m)

#Variables_summary <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv"))
#Variables_summary$meanEVI500m <- combined_data500m$meanEVI500m
#write.csv(Variables_summary, file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)
#write.csv(combined_data500m, file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)

column_names <- expand.grid(mon = month.abb, yr = 2013:2023) %>% 
  mutate(column_name = paste0(mon, "_", yr))

names(EVI_data500m) <- c("ID", column_names$column_name)


###clean NA COLUMNS

print(column_names$column_name)
#na_blank_cols <- which(colnames(EVI_data500m) == "" | is.na(colnames(EVI_data500m)))
#EVI_data500m_clean <- EVI_data500m[, -na_blank_cols]

###  replace column names
cleaned_colnames <- colnames(EVI_data500m)
cleaned_colnames[is.na(cleaned_colnames) | cleaned_colnames == ""] <- "valid_name"
colnames(EVI_data500m) <- cleaned_colnames

EVI500m_data <- combined_data500m %>% 
  cbind(EVI_data500m) %>% 
  reshape::melt(id = c("NOM", "ID")) %>% 
  filter(variable != "meanEVI500m") %>% 
  tidyr::separate(variable, into = c("month", "year"), sep = "_") %>% 
  group_by(NOM, year) %>% 
  mutate(yearly_meanEVI500m = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(NOM) %>%
  mutate(overall_meanEVI500m = mean(value, na.rm = T))

write.csv(EVI500m_data, file.path(EVIm , "overall_EVI500m_data.csv"), row.names = FALSE)

EVI500m_plottingdata <- inner_join(df_abidjan1, EVI500m_data )

################ EVI 500M Plots

ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = EVI500m_plottingdata, aes(geometry = geometry, fill = yearly_meanEVI500m )) +
  facet_wrap(~year)+
  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "EVI (500M)", fill = "", x = NULL, y = NULL) +
  map_theme() 


ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = EVI500m_plottingdata, aes(geometry = geometry, 
                                           fill = overall_meanEVI500m )) +
  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "enhanced vegetation index (500M)", fill = "", x = NULL, y = NULL) +
  map_theme()




#########################################################################################
##############NDVI
#########################################################################################

######combined NDVI values 2013-2023

ndvi1kmfiles <- list.files( file.path(NDVIkm), 
                            pattern = ".tif", full.names = TRUE)
 
ndviraster_data1km <- lapply(seq_along(ndvi1kmfiles), 
                      function(x) raster::raster(ndvi1kmfiles[[x]]))
 
ndvi_data1km <- ndviraster_data1km %>%
   purrr::map(~raster::extract(., df_abidjan1,
                               buffer = buffer,
                               fun = mean, df =TRUE)) %>%
   purrr::reduce(left_join, by = c("ID"))
 
df_abidjan1$meanNDVI <- rowMeans(ndvi_data1km[, 2:133], na.rm=TRUE)

ndvi1km_plottingdata <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanNDVI, c(0, 0.05, 0.1, 0.16, 0.2,
                                0.25, 0.3, 0.4), include.lowest = T ))

ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = ndvi1km_plottingdata, aes(geometry = geometry, fill = meanNDVI)) +
  scale_fill_continuous(name="normalized enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
  labs(title = 'NDVI 2013-2023', fill = "", x = NULL, y = NULL) +
  map_theme() 


combined_data2 <- df_abidjan1 %>% 
  st_drop_geometry() %>% 
  dplyr::select(NOM, meanNDVI)

#Variables_summary <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv"))
#Variables_summary$mean_NDVI1km <- combined_data2$meanNDVI
#write.csv(Variables_summary, file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)


column_names <- expand.grid(mon = month.abb, yr = 2013:2023) %>% 
  mutate(column_name = paste0(mon, "_", yr))


names(ndvi_data1km) <- c("ID", column_names$column_name)

NDVI_data <- combined_data2 %>% 
  cbind(ndvi_data1km) %>% 
  reshape::melt(id = c("NOM", "ID")) %>% 
  filter(variable != "meanNDVI") %>% 
  tidyr::separate(variable, into = c("month", "year"), sep = "_") %>% 
  group_by(NOM, year) %>% 
  mutate(yearly_meanNDVI = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(NOM) %>%
  mutate(overall_meanNDVI = mean(value, na.rm = T))

write.csv(NDVI_data, file.path(NDVIkm , "overall_NDVI_data.csv"), row.names = FALSE)

NDVI_plottingdata <- inner_join(df_abidjan1, NDVI_data )

####### NDVI (1KM) Plots

ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = NDVI_plottingdata, aes(geometry = geometry, fill = yearly_meanNDVI )) +
  facet_wrap(~year)+
  scale_fill_continuous(name="ndvi", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "Yearly NDVI(1km)", fill = "", x = NULL, y = NULL) +
  map_theme() 


ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = NDVI_plottingdata, aes(geometry = geometry, 
                                       fill = overall_meanNDVI )) +
  scale_fill_continuous(name="ndvi", low = "#F6E0b3", high = "darkgreen") +
  labs(title = "NDVI (1KM) 2013-2023", fill = "", x = NULL, y = NULL) +
  map_theme()























# ###########################################################################################################################################
# ######EVI for Built Areas 2023
# #########################################################################################################################
# BuiltupDir <-  file.path(AbidjanDir, "Built up area")
# Built_areas <- st_read(file.path(BuiltupDir, "Built up area.shp"))
# view(Built_areas)
# 
# pattern2023 <- paste0("MOD13A3.061__1_km_monthly_EVI_doy", "2023", "..._aid0001.tif")
# rasters_2023 <- list.files(file.path(EVIkm), pattern = pattern2023, full.names = TRUE)
# print(rasters_2023)
# raster_data23 = lapply(seq_along(rasters_2023), 
#                        function(x) raster::raster(rasters_2023[[x]]))
# EVI_data_built23 = raster_data23 %>%
#   purrr::map(~raster::extract(., Built_areas,
#                               buffer = buffer,
#                               fun = mean, df =TRUE)) %>%
#   purrr::reduce(left_join, by = c("ID"))
# view(EVI_data_built23)
# Built_areas$meanEVI23 <- rowMeans(EVI_data_built23, na.rm=TRUE)
# 
# evi_plotbuilt23 <- Built_areas %>%
#   sf::st_as_sf() %>%
#   mutate(class = cut(meanEVI23, c(0, 0.05, 0.1, 0.16, 0.2,
#                                   0.25, 0.3, 0.4), include.lowest = T))
# ggplot()+
#   geom_sf(data = df_abidjan1, aes(), color= "black", fill = "#ece9f7")+
#   geom_sf(data = Built_areas) +
#   geom_sf(color = "black", fill = "white") +
#   geom_sf(data = evi_plotbuilt23, aes(geometry = geometry, fill = meanEVI23)) +
#   geom_sf_text(data = evi_plotbuilt23, aes(geometry = geometry, label = meanEVI23,))+
#   scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") + # Uncomment if you want to label the slums  scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
#   labs(title = "2023 EVI (1KM) IN BUILT AREAS", fill = "", x = NULL, y = NULL) +
#   map_theme()
# 
# #################
# #####EVI INVERSE OF BUILT AREAS
# ##########
# 
# builtfile <- st_read(file.path(BuiltupDir, "Built up area.shp"))
# abidjanfile <- st_read(Abidjanmap1)
# 
# if (!identical(st_crs(builtfile), st_crs(abidjanfile))) {
#   builtfile <- st_transform(builtfile, st_crs(abidjanfile))
# }
# st_write(abidjanfile, file.path("~/abidjan.shp"), append=FALSE)
# citymap <- st_read(file.path("~/abidjan.shp"))
# 
# minus_built<- st_difference(citymap, builtfile)
# st_write(minus_built, "~/minus_built.shp", append = FALSE)
# non_built_areas <- st_read(file.path("~/minus_built.shp"))
# view(non_built_areas)
# 
# pattern2023 <- paste0("MOD13A3.061__1_km_monthly_EVI_doy", "2023", "..._aid0001.tif")
# rasters_2023 <- list.files(file.path(EVIkm), pattern = pattern2023, full.names = TRUE)
# print(rasters_2023)
# raster_data23 = lapply(seq_along(rasters_2023), 
#                        function(x) raster::raster(rasters_2023[[x]]))
# 
# EVI_data_inverse_built23 = raster_data23 %>%
#   purrr::map(~raster::extract(., non_built_areas,
#                               buffer = buffer,
#                               fun = mean, df =TRUE)) %>%
#   purrr::reduce(left_join, by = c("ID"))
# 
# view(EVI_data_inverse_built23)
# 
# non_built_areas$meanEVI23 <- rowMeans(EVI_data_inverse_built23, na.rm=TRUE)
# 
# evi_plotinversebuilt23 <- non_built_areas %>%
#   sf::st_as_sf() %>%
#   mutate(class = cut(meanEVI23, c(0, 0.05, 0.1, 0.16, 0.2,
#                                   0.25, 0.3, 0.4), include.lowest = T))
# ggplot()+
#   #geom_sf(data = df_abidjan1, aes(), color= "black", fill = "#ece9f7")+
#   geom_sf(data = citymap, aes(), color= "black", fill = "#ece9f7")+
#   geom_sf(data = non_built_areas) +
#   geom_sf(color = "black", fill = "white") +
#   geom_sf(data = evi_plotinversebuilt23, aes(geometry = geometry, fill = meanEVI23)) +
#   geom_sf_text(data = evi_plotinversebuilt23, aes(geometry = geometry, label = meanEVI23))+ 
#   scale_fill_continuous(name="enhanced vegetation index", low = "#F6E0b3", high = "darkgreen") +
#   labs(title = "2023 EVI (1KM) IN NON BUILT AREAS", fill = "", x = NULL, y = NULL) +
#   map_theme()
# 
# 
