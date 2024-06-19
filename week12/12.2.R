# Load necessary libraries
library(sf)
library(ggplot2)
library(osmdata)

taiwan_cities <- st_read("C:/Users/USER/Downloads/mapdata202301070205/COUNTY_MOI_1090820.shp")
taiwan_Industry <- st_read("C:/Users/USER/Downloads/範圍圖資料集/2024產業管理局.shp")
temperature_data <- read.csv("C:/Arthur Yeh/論文/數據資料/DataSet 1130420/氣候變數/新增 Microsoft Excel 工作表.csv")
taiwan_citie <- merge(taiwan_cities, temperature_data, by.x = "COUNTYNAME")


# Plot boundaries of Taiwan's cities with temperature data
plot_taiwan_cities <- ggplot() +
  geom_sf(data = taiwan_citie, aes(fill = "red"), color = "black",alpha=taiwan_citie$Atem) +
  geom_sf(data = taiwan_Industry, fill = "green") +
  theme_minimal()+
  coord_sf(xlim = c(120, 122), ylim = c(21.5, 25.3)) +
  labs(title = "Average Temperature Shock in Taiwan",subtitle = "Data Source: Shapefile and Temperature Data",caption = "Map Data: © Taiwan Government")

plot_taiwan_cities
