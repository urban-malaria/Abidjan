
rm(list=ls())

#directories
user <- Sys.getenv("USERNAME")

if (user == "MGGVUS002" | user == ""){
  #directories
  AbidjanDir <- "C:/Users//MGGVUS002/Dropbox/abidjan"
  program_dat <- file.path(AbidjanDir, "program_data")
  
}else{
  
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
  Drive <- file.path(gsub("[//]", "/", Drive))
  DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
  DataDir <- file.path(DriveDir, "data")
  AbidjanDir <-  file.path(DataDir, "abidjan")
  program_dat <- file.path(AbidjanDir, "program_data")
  ProjectDir <- file.path(DriveDir, "projects", "urban_microstratification", "Abidjan")
  plots <- file.path(ProjectDir, "plots")
  
}

# other paths 

NASAdata <- file.path(AbidjanDir, "Autonome D_Abidjan")
Earthdata <- file.path(NASAdata, "EarthData")
EVIkm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_1km_Monthly_2013-23")
EVIm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_500m_16d_2013-23")
NDVIkm <- file.path(Earthdata,"MODIS-TERRA_VegetationIndex_NDVI_1km_Monthly_2013-23")
Rainfall2013_23 <- file.path(NASAdata, "Rainfall 2013-2023")
Climatedata <- file.path(NASAdata, "ClimateSERV")

Abidjanmap1 <- file.path(NASAdata, "Autonome D_Abidjan2.geojson")


list_of_packages <- c("RColorBrewer", "readr", "haven", "data.table",
                      "ggplot2", "labelled", "tidyverse", "janitor",
                      "readxl", "mapsf", "survey","srvyr",
                      "broom", "ggthemes", "ggrepel", "sjlabelled",
                      "ggplot2", "dplyr", "ggpubr", "sf", "viridis", "patchwork", 
                      "raster", "wordcloud", "ggwordcloud", "terra", "plotly")


read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}

read_install_pacakges()



Abi_shapefile <- readRDS(file.path(program_dat, "shapefilesCIV.rds"))
routine_dat <- readRDS(file.path(program_dat, "ts_retro_civ.rds"))
campign_dat <- read.csv(file.path(program_dat, "microplan_abidjan_brief.csv"))


Abidjanmap <- st_read(Abidjanmap1)


Abidjan = Abi_shapefile[[3]] %>%
  filter(NAME_1 == "Abidjan")

df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}
