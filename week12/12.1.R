D# 載入必要的套件 -----
library(tidyverse)
library(sf)
library(cowplot)

path <- "C:/Users/chewei/Documents/GitHub/112-2-econDV-practice/data/mapdata202301070205"
filePath <- file.path(path)
shp_data <- st_read(path)

# 調整邊界框 -----
bbox <- st_bbox(shp_data)
bbox["ymin"] <- 21
bbox["ymax"] <- 26
bbox["xmin"] <- 120
bbox["xmax"] <- 123
taiwanShape <- st_crop(shp_data, bbox)

# 創建 CV 圖例資料 -----
cv_legend <- data.frame(
  x = 0.5,
  y = seq(0.1, 0.9, length.out = 100),
  color = colorRampPalette(c("#000080", "#B0E0E6"))(100)
)

# 將縣市名稱轉換為顏色向量 -----
county_colors <- c(
  "臺中市" = "#000080", "桃園市" = "#000080", "新北市" = "#000080", "臺北市" = "#000080",
  "高雄市" = "#1E90FF", "臺南市" = "#1E90FF", "彰化縣" = "#1E90FF", "新竹縣" = "#1E90FF",
  "新竹市" = "#1E90FF", "苗栗縣" = "#87CEFA", "雲林縣" = "#87CEFA", "屏東縣" = "#87CEFA",
  "花蓮縣" = "#87CEFA", "宜蘭縣" = "#87CEFA", "南投縣" = "#B0E0E6",
  "基隆市" = "#B0E0E6", "嘉義市" = "#B0E0E6", "嘉義縣" = "#B0E0E6", "臺東縣" = "#B0E0E6",
  "澎湖縣" = NA, "金門縣" = NA, "連江縣" = NA
)
taiwanShape$color <- county_colors[taiwanShape$COUNTYNAME]

# 繪製台灣地圖 -----
taiwan_map <- ggplot(taiwanShape) +
  geom_sf(aes(fill = color), color = "white", size = 0.3) +
  scale_fill_identity() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(120, 122, by = 1)) +
  labs(
    title = "全國鮮奶品牌價格變異性分析",
    subtitle = "探討各縣市鮮奶品牌價格勾結的證據",
    caption = "資料來源：公平會發票資料"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 繪製 CV 圖例 -----
cv_legend_plot <- ggplot(cv_legend, aes(xmin = 0, xmax = 1, ymin = y - 0.01, ymax = y + 0.01)) +
  geom_rect(aes(fill = color)) +
  scale_fill_identity() +
  coord_fixed(ratio = 10) +
  theme_void() +
  annotate(
    geom = "text",
    x = 0.5,
    y = c(0.95, 0.05),
    label = c("CV High", "CV Low"),
    color = "black",
    size = 3
  )

# 組合地圖和 CV 圖例 -----
combined_plot <- ggdraw() +
  draw_plot(taiwan_map, 0, 0, 0.9, 1) +
  draw_plot(cv_legend_plot, 0.55, 0.2, 0.4, 0.6)
combined_plot