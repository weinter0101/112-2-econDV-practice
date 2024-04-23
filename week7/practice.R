library(tidyverse)
library(readxl)
library(forcats)

# 讀取Excel檔案
data_students <- read_excel("C:/Users/chewei/Documents/GitHub/112-2-econDV-practice/data/w7/大專院校各學制學生數-112.xlsx")
data_aboriginal_students <- read_excel("C:/Users/chewei/Documents/GitHub/112-2-econDV-practice/data/w7/大專院校各學制原住民學生數-112.xlsx")

# 預處理數據
data_students <- data_students %>%
  mutate(Type = if_else(str_detect(學校名稱, "國立"), "Public", "Private"))

data_aboriginal_students <- data_aboriginal_students %>%
  mutate(Type = if_else(str_detect(學校名稱, "國立"), "Public", "Private")) %>%
  mutate(Degree = case_when(
    等級別 == "學士" ~ "Bachelors",
    等級別 == "碩士" ~ "Masters",
    等級別 == "博士" ~ "PhD"
  ))

# 聚合原住民學生數據
aboriginal_students_aggregated <- data_aboriginal_students %>%
  group_by(Type, Degree) %>%
  summarise(AboriginalStudentCount = sum(總計加總, na.rm = TRUE))

# 計算每種學位的平均原住民學生數
aboriginal_students_avg <- aboriginal_students_aggregated %>%
  group_by(Degree) %>%
  summarise(AvgAboriginalStudentCount = mean(AboriginalStudentCount, na.rm = TRUE)) %>%
  mutate(Type = "Avg")

# 合併公立、私立和平均數據
final_data <- bind_rows(
  aboriginal_students_aggregated,
  aboriginal_students_avg %>% 
    select(Type, Degree, AboriginalStudentCount = AvgAboriginalStudentCount)
)

# 計算總學生人數
total_students <- sum(data_students$學士) + sum(data_students$碩士) + sum(data_students$博士)

# 將原住民學生人數轉換為百分比
final_data <- final_data %>%
  mutate(Percentage = (AboriginalStudentCount / total_students) * 100)

# 轉化Degree為因子類型並指定順序
final_data$Degree <- factor(final_data$Degree, levels = c("Bachelors", "Masters", "PhD"))

# 繪製圖表
dodge_width <- 0.9
ggplot(final_data, aes(x = Degree, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = dodge_width), width = dodge_width * 0.9) +
  scale_fill_manual(values = c("Public" = "blue", "Private" = "green", "Avg" = "red")) +
  labs(
    title = "Aboriginal Student Share by Degree and School Type (%)",
    x = "Degree",
    y = "Percentage (%)",
    fill = "School Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format())

# 保存圖形
#ggsave("fixed_stacked_bar_chart.png", width = 10, height = 6)
