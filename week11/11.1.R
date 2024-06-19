library(tidyverse)
library(sf)
# Reading a shapefile -----
path <- "C:/Users/chewei/Documents/GitHub/112-2-econDV-practice/data/mapdata202301070205"
filePath <- file.path(path)
shp_data <- st_read(path)


# Adjusting bounding box -----
bbox <- st_bbox(shp_data)

bbox["ymin"] <- 21
bbox["ymax"] <- 26
bbox["xmin"] <- 120
bbox["xmax"] <- 123

taiwanShape <- st_crop(shp_data, bbox)

# check the countryname
print(names(taiwanShape))
print(unique(taiwanShape$COUNTYNAME))


# Adding the Region of Taiwan
taiwanShape$Region <- factor(case_when(
  taiwanShape$COUNTYNAME %in% c("臺北市", "新北市", "基隆市", "宜蘭市", "桃園市", "新竹縣", "新竹市") ~ "Northern",
  taiwanShape$COUNTYNAME %in% c("苗栗縣", "臺中市","彰化縣", "南投縣", "雲林縣") ~ "Central",
  taiwanShape$COUNTYNAME %in% c("嘉義市", "嘉義縣", "臺南市", "高雄市", "屏東縣") ~ "Southern",
  TRUE ~ "Eastern"
), levels = c("Northern", "Central", "Southern", "Eastern"))

# plotting
ggplot(taiwanShape) +
  geom_sf(aes(fill = Region), color = "white", size = 1) +
  geom_sf(data = taiwanShape %>% group_by(Region) %>% summarise(), 
          fill = NA, color = "black", lwd = 0.8) +  
  theme_minimal() +
  scale_fill_manual(values = c("Northern" = "#2196F3", "Central" = "#4CAF50", "Southern" = "#E53935", "Eastern" = "#FF9800")) +
  scale_x_continuous(breaks = seq(120, 122, by = 1)) +
  labs(title = "Simplified Shape with Region Coloring", 
       subtitle = "Taiwan's Regions",
       caption = "Data Source: National Development Council") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
