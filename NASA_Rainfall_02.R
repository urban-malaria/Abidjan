
source("load_path.R", echo=FALSE) 
source ("C:/Users/hp/Abidjan/load_path.R", echo=FALSE) #leave commented

RainfallPlus <- file.path(Climatedata, "Extracted_ClimeServ_CHIRPS_")

install.packages("plotly")
library(plotly)

##############################################################################################################################################################
# RAINFALL
###############################################################################################################################################################

########## Yearly rainfall 2013 ############

pattern2013 <- paste0("2013[0-9]+.tif")



rainfall_2013 <- list.files(file.path(Rainfall2013_23), pattern = pattern2013, full.names = TRUE)

# print(rainfall_2013)


#################################################################################
#####################DATA FROM CLIMESERV FOLDER##################################
#################################################################################

tag <- c(3:9, 0:3)

patterns <- paste0(c(rep(201, 7), rep(202, 4)), tag, "[0-9]+.tif")


rainfall <- lapply(seq_along(patterns), 
                   function (x) list.files(file.path(RainfallPlus),
                                           pattern = patterns[x],
                                           full.names = TRUE))


# rainfall_data = lapply(seq_along(rainfall), 
#                           function(x) lapply(seq_along(rainfall[[x]]), 
#                                              function(i) raster::raster(rainfall[[x]][[i]])))

rainfall <- unlist(rainfall)

rainfall_data =  lapply(seq_along(rainfall), function(x) raster::raster(rainfall[x]))


raindata13b = rainfall_data %>%
  # CAVEAT:takes long to run
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))



long_rainfall_data <- df_abidjan1 %>% 
  # check is the sub-setting is correct and summarising 
  # makes sense and please find out the unit measurement 
  st_drop_geometry() %>% 
  dplyr::select(NOM) %>% 
  cbind(raindata13b) %>%
  reshape::melt(id = c("NOM", "ID")) %>% 
  mutate(year = substr(variable, 2, 5), 
         month = substr(variable, 6, 7))

# write.csv(long_rainfall_data, file.path(RainfallPlus, "all_daily_rainfall_data.csv"), row.names = FALSE)



summerised_rainfall <- long_rainfall_data %>% 
  group_by(NOM, ID, year, month) %>% 
  mutate(monthly_rainfall = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(NOM, ID, year) %>% 
  mutate(yearly_rainfall = mean(value, na.rm = T)) %>% 
  rename(daily_rainfall = value) %>% 
  ungroup() %>% 
  group_by(NOM, ID) %>% 
  mutate(overall_rainfall = mean(daily_rainfall, na.rm = T)) 


# write.csv(summerised_rainfall, file.path(RainfallPlus, "all_rainfall_data.csv"), row.names = FALSE)
# summerised_rainfall <-read.csv(file.path(RainfallPlus, "all_rainfall_data.csv"))




rainfall_plottingdata <- inner_join(df_abidjan1, summerised_rainfall)



##########################################################################################
####### Plots
##########################################################################################


ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = rainfall_plottingdata, aes(geometry = geometry, fill = overall_rainfall )) +
  facet_wrap(~year)+
  scale_fill_continuous(name = "Average Rainfall", low = "grey", high = "darkblue") +
  labs(title = "Average Rainfall", fill = "", x = NULL, y = NULL) +
  map_theme() 





ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = rainfall_plottingdata, aes(geometry = geometry, fill = overall_rainfall)) +
  scale_fill_continuous(name = "Average Rainfall", low = "grey", high = "darkblue") +
  labs(title = "Average Rainfall", fill = "", x = NULL, y = NULL) +
  map_theme() 


################################################################################
########### TIME SERIES PLOTS##########################################
##########################################################################

summerised_rainfall <-read.csv(file.path(RainfallPlus, "all_rainfall_data.csv"))

#plot time series with yearly rainfall

ggplot(rainfall_data, aes(x = year, y = yearly_rainfall, color = NOM)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(rainfall_data$year), max(rainfall_data$year), by = 1)) +
  labs(title = "Rainfall Trend 2013-2023",
       x = "Year",
       y = "Yearly Rainfall",
       color = "Health District") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))

#plot time series with monthly data

rainfall_data$date <- as.Date(paste(rainfall_data$year, rainfall_data$month, "01", sep = "-"))

ggplot(rainfall_data, aes(x = date, y = monthly_rainfall, color = NOM)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  #facet_wrap(~NOM)+  
  labs(title = "Monthly Rainfall by Health District",
       x = "Year",
       y = "Average Monthly Rainfall",
       color = "Health District") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))

##plot time series with daily rainfall data

rainfall_data <- summerised_rainfall %>%
  mutate(date = ymd(substr(variable, 2, 9)))

ggplot(rainfall_data, aes(x = date, y = daily_rainfall, color = NOM)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(rainfall_data$year), max(rainfall_data$year), by = 1)) +
  #facet_wrap(~NOM)+
  labs(title = "Daily Rainfall Trends in each Health District",
       x = "Year",
       y = "Rainfall",
       color = "Health District") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))

# Interactive daily rainfall plot
plot_ly(rainfall_data, x = ~date, y = ~daily_rainfall, color = ~NOM, text = ~paste("Date: ", date, "<br>Rainfall: ", daily_rainfall)) %>%
  add_lines() %>%
  layout(title = "Daily Rainfall Trends in each Health District",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Rainfall"),
         hovermode = "closest")


################ individual health districts plot with monthly data
rainfall_data$date <- as.Date(paste(rainfall_data$year, rainfall_data$month, "01", sep = "-"))
plots_folder <- file.path(plots, "Rainfall", "27-02-2024")

health_districts <- unique(rainfall_data$NOM)
for (district in health_districts) {
  district_data <- subset(rainfall_data, NOM == district)
  
  plot <- ggplot(data = district_data, aes(x = date, y = monthly_rainfall)) +
    geom_line() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 month") +
    labs(title = paste("Rainfall Trends for", district),
         x = "Date",
         y = "Average Monthly Rainfall") +
    map_theme()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  
          axis.text.y = element_text(angle = 45, hjust = 1))
  print(plot)
  #plot_filename <- paste0(plots_folder, "/", gsub("\\s", "_", district), "_rainfall_plot.png")
  #ggsave(filename = plot_filename, plot = plot, width = 10, height = 6)
}



