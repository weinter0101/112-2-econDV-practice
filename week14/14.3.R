# 繪製速食店Revenue折線圖 -----
library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(ggrepel)

# 讀取資料 -----
file_path <- "C:/Users/wen/OneDrive/Desktop/資料視覺化作業/速食店銷量冠軍.xlsx"
data <- read_excel(file_path)

# 整理資料 -----
data_long <- data %>%
  rename(
    Year = 年份,
    McDonald = `McDonald Revenue`,
    Starbucks = `Starbucks Revenue`,
    YUM = `YUM Revenue`,
    Domino = `Domino Revenue`,
    Wendy = `Wendy Revenue`
  ) %>%
  pivot_longer(
    cols = -Year, # 選擇除"年份"列外的所有列
    names_to = "Company", # 新的變量名稱
    values_to = "Revenue" # 新的值名稱
  ) %>%
  mutate(
    Revenue = as.numeric(gsub("[\\$,]", "", Revenue))
  )

# 設定顏色 -----
colors <- c("McDonald" = "#FF6961", "Starbucks" = "#4682B4", "YUM" = "#90EE90", "Domino" = "#FFD700", "Wendy" = "#D3D3D3")


# 找出每個公司的最後一個觀察值
last_data <- data_long %>%
  group_by(Company) %>%
  slice_max(order_by = Year)

#繪圖
plot <- ggplot(data_long, aes(x = Year, y = Revenue, color = Company)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  geom_text_repel(data = last_data, aes(label = Company, x = Year, y = Revenue), size = 3, color = "black", direction = "y", segment.size = 0.2) +
  geom_segment(aes(x = Year, xend = Year, y = min(Revenue), yend = min(Revenue) - (max(Revenue) - min(Revenue)) * 0.05), color = "grey", size = 0.5) + # 添加線段表示年份區間
  labs(title = "Revenue Trends of Major Fast Food Chains (2013-2023)",
       subtitle = "Exploring Revenue Changes of McDonald's, Starbucks, YUM, Domino's, and Wendy's",
       caption = "Data Source: Online Statistical Data",
       x = "Year",
       y = "Revenue (Million USD)") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.title.y.right = element_text(size = 12, angle = 0, vjust = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), # 移除Y軸的網格線
    panel.grid.minor.x = element_blank(), # 移除Y軸的次網格線
    panel.grid.major.y = element_line(color = "grey"), # 只保留X軸的網格線
  ) +
  scale_x_continuous(breaks = seq(2013, 2023, 1)) + # 設置 x 軸刻度
  scale_y_continuous(labels = dollar_format(scale = 1e-6, prefix = "$", suffix = "M"), sec.axis = sec_axis(~ ., name = "Revenue (Million USD)", labels = dollar_format(scale = 1e-6, prefix = "$", suffix = "M")))

print(plot)
# 保存圖表 -----
ggsave(
  filename = "week14_HW.svg",
  plot = plot,
  width = 700,
  height = 500,
  units = "px",
  dpi = 72,
  device = "svg"
)