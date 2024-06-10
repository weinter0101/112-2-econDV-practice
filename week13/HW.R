# Scatter plot of Mean vs CV, highlighting collusion levels -----
library(tidyverse)
library(readxl)
path <- "C:\\Users\\chewei\\Documents\\GitHub\\112-2-econDV-practice\\data\\HW13.xlsx"
c_data <- read_excel(path) %>%
  mutate(Collusion = factor(Collusion, levels = c("0", "1", "2", "3"),
                            labels = c("No Collusion", "Mild Collusion", "Moderate Collusion", "Severe Collusion")))
  #mutate(Collusion = factor(Collusion, levels = c("3", "2", "1", "0"),
  #                        labels = c("Severe Collusion", "Moderate Collusion", "Mild Collusion", "No Collusion")))
severe_collusion <- c_data %>% filter(Collusion == "Severe Collusion")
point_to_label <- severe_collusion[1, ] 
ggplot(c_data, aes(x = jitter(Mean), y = jitter(CV), color = Collusion, size = as.numeric(Collusion) + 1)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("No Collusion" = "gray", "Mild Collusion" = "blue",
                                "Moderate Collusion" = "orange", "Severe Collusion" = "red")) +
  labs(title = "Gaussian Mixture Clustering Model 高斯混合分群模型",
       subtitle = "各家廠商勾結行為的識別(四四配對)",
       caption = "Data Source: Fair Trade Commission",
       x = "Mean", y = "Coefficient of Variation (CV)",
       size = "Collusion Severity",
       color = "Collusion Level") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        legend.key = element_blank()) +
  guides(size = guide_legend(title = "Collusion Severity", override.aes = list(color = "black"))) +
  annotate("text", x = max(c_data$Mean), y = max(c_data$CV),
           label = paste(rep("●", 5), collapse = "\n"), color = "red", hjust = 0, vjust = 1, size = 3) +
  annotate("text", x = max(c_data$Mean), y = max(c_data$CV),
           label = paste(c("光泉乳香世家全脂鮮乳&林鳳營全脂鮮乳&福樂一番鮮全脂鮮乳&統一瑞穗全脂鮮乳",
                           "光泉乳香世家全脂鮮乳&林鳳營全脂鮮乳&統一瑞穗全脂鮮乳&義美全脂鮮乳",
                           "光泉乳香世家全脂鮮乳&福樂一番鮮全脂鮮乳&統一瑞穗全脂鮮乳&萬丹酪農戶限定鮮乳",
                           "林鳳營全脂鮮乳&福樂一番鮮全脂鮮乳&統一瑞穗全脂鮮乳&萬丹酪農戶限定鮮乳",
                           "林鳳營全脂鮮乳&統一瑞穗全脂鮮乳&義美全脂鮮乳&萬丹酪農戶限定鮮乳"), collapse = "\n"),
           hjust = 1, vjust = 1, size = 3, color = "black", nudge_x = 0.1)