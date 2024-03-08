
source("load_path.R", echo=FALSE) 
source("~/Abidjanf/load_path.R", echo=FALSE)

RainfallPlus <- file.path(Climatedata, "Extracted_ClimeServ_CHIRPS_")

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
rainfall_data <- summerised_rainfall %>%
  mutate(date = ymd(substr(variable, 2, 9)))
rainfall_data <- rainfall_data %>%
  drop_na()
flood_data <-read.csv(file.path(AbidjanDir, "Flood list.csv"))
flood_data$date <- ymd(paste(flood_data$year, flood_data$month, flood_data$day, sep = "-"))

#yearly rainfall and flood points

ggplot() +
  geom_line(data = rainfall_data, aes(x = year, y = yearly_rainfall, color = NOM)) +
  geom_point(data = flood_data, aes(x = year, y = Rainfall, shape = HealthDistrict), size =2)+ 
  scale_x_continuous(breaks = seq(min(rainfall_data$year), max(rainfall_data$year), by = 1)) +
  labs(title = "Rainfall Trend 2013-2023",
       caption = "Highlighted points indicate rainfall amount resulting in floods in a specific regions", 
       x = "Year",
       y = "Rainfall (inches)",
       color = "Health District",
       shape = "Flood locations") +
   scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6)) +  
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#monthly rainfall

rainfall_data$date <- as.Date(paste(rainfall_data$year, rainfall_data$month, "01", sep = "-"))
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(rainfall_data, aes(x = month, y = monthly_rainfall, color = NOM)) +
  geom_line() +
  geom_point(data = flood_data, aes(x = month, y = Rainfall, color = HealthDistrict), size = 2) + # comment to remove flood points
  facet_wrap(~year)+
  labs(title = "Rainfall in Abidjan 2013-2023",
       x = "Month",
       #y = "Average Monthly Rainfall",
       y = "Rainfall (inches)",
       caption = "Highlighted points indicate floods in a specific region",
       color = "Health District") +
  scale_x_continuous(breaks = 1:12, labels = month_labels)+
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### individual plots of rainfall and plots through the year
unique_years <- unique(rainfall_data$year)
for (year_val in unique_years) {
  # Subset data for the current year
  subset_rainfall_data <- subset(rainfall_data, year == year_val)
  subset_flood_data <- subset(flood_data, year == year_val)

  plot <- ggplot(subset_rainfall_data, aes(x = month, y = monthly_rainfall, color = NOM)) +
    geom_line() +
    geom_point(data = subset_flood_data, aes(x = month, y = Rainfall, color = HealthDistrict), size = 2) + 
    labs(title = paste("Rainfall in Abidjan", year_val),
         x = "Month",
         y = "Rainfall (inches)",
         caption = "Highlighted points indicate floods in a specific region",
         color = "Health District") +
    scale_x_continuous(breaks = 1:12, labels = month_labels) +
    theme_manuscript() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot)
  
}

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

##Rainfall plot per district





