# 加載所需的庫
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

# 數據
data <- data.frame(
  Country = c("U.S.", "Japan", "South Korea", "Taiwan", "Philippines", "Vietnam", "India", "China"),
  正面 = c(78, 68, 62, 56, 37, 37, 33, 20),
  負面 = c(11, 6, 7, 6, 12, 10, 23, 52)
)
# 按正面數值進行排列
data <- data %>%
  arrange(desc(正面)) %>%
  mutate(Country = factor(Country, levels = Country))

# 創建反向的負面百分比，以便它們在中間對齊
data <- data %>%
  mutate(負面 = -負面)

# 轉換數據為長格式
data_long <- data %>%
  pivot_longer(cols = c("正面", "負面"),
               names_to = "Opinion", values_to = "Percentage")
#A6513E
#A2B4AC
# 繪製圖表
ggplot(data_long, aes(x = Percentage, y = fct_rev(Country), fill = Opinion)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black", size = 1) +  # 添加垂直基準線
  scale_fill_manual(values = c("正面" = "#D7A49A", "負面" = "#CACDD3"),
                    labels = c("正面", "負面")) +
  labs(title = "亞裔美國人對其祖籍地之觀感",
       subtitle = "美國亞裔成年人對其祖籍地的認同比率",
       x = "Percentage", y = "") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0, face = "bold"),  # 靠左對齊且粗體
        plot.subtitle = element_text(hjust = 0),  # 靠左對齊
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold", hjust = 1),
        plot.caption = element_text(hjust = 0)) +  # 靠左對齊
  guides(fill = guide_legend(override.aes = list(size = 1))) +
  labs(caption = "資料來源：Pew Research Center")


