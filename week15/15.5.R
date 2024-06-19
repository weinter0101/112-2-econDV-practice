# 製作EIB氣候調查圖表 -----

library(tibble)
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

data <- tibble(
  country = c("Portugal", "Spain", "France", "Ireland", "UK", "Belgium", "Netherlands", "Luxembourg", "Germany", 
              "Austria", "Switzerland", "Italy", "Greece", "Slovenia", "Croatia", "Hungary", "Slovakia", "Czechia", 
              "Poland", "Romania", "Bulgaria", "Estonia", "Latvia", "Lithuania", "Finland", "Sweden"),
  support = c(74, 74, 67, 65, 58, 69, 63, 68, 67, 62, 59, 71, 65, 68, 65, 61, 54, 52, 52, 65, 61, 51, 51, 52, 59, 52)
)

# 獲取歐洲國家的地理數據
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>% filter(region_un == "Europe")

# 合併數據框
europe_data <- europe %>%
  left_join(data, by = c("name" = "country"))

# 計算每個國家的幾何中心
europe_data <- europe_data %>%
  mutate(centroid = st_centroid(geometry),
         longitude = st_coordinates(centroid)[,1],
         latitude = st_coordinates(centroid)[,2],
         fill_color = cut(support, 
                          breaks = c(-Inf, 55, 60, 65, 70, Inf), 
                          labels = c("#FFD700", "#C6A300", "#9ACD32", "#32CD32", "#006400"),
                          include.lowest = TRUE,
                          right = FALSE))

# 繪製地圖
ggplot(europe_data) +
  geom_sf(aes(fill = fill_color)) +
  scale_fill_manual(values = c("#FFD700", "#C6A300", "#9ACD32", "#32CD32", "#006400"),
                    na.value = "grey50", 
                    name = "Support (%)",
                    labels = c("<55%", "55-60%", "60-65%", "65-70%", ">70%")) +
  geom_text(data = europe_data[!is.na(europe_data$support), ], aes(x = longitude, y = latitude, label = paste0(support, "%")), color = "white", size = 3, check_overlap = TRUE) +
  labs(
    title = "EIB Climate Survey",
    subtitle = "66% of Europeans are in favour of stricter government measures\nimposing changes in people's behaviour to tackle climate change",
    caption = "Source: BVA for the EIB"
  ) +
  coord_sf(
    xlim = c(-25, 45),   # 設定經度範圍
    ylim = c(34, 72)     # 設定緯度範圍
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10),
    legend.position = "bottom"
  )