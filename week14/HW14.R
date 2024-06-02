library(tidyverse)
library(readxl)
library(ggfortify)

path <- "C:\\Users\\chewei\\Documents\\GitHub\\112-2-econDV-practice\\data\\HW13.xlsx"

c_data <- read_excel(path) %>%
  mutate(Collusion = factor(Collusion, levels = c("0", "1", "2", "3"),
                            labels = c("No Collusion", "Mild Collusion", "Moderate Collusion", "Severe Collusion")))

pca_result <- prcomp(c_data[, c("Mean", "CV")], scale. = TRUE)

autoplot(pca_result, data = c_data, colour = 'Collusion', shape = 'Collusion', label = FALSE, 
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, frame = TRUE) +
  scale_color_manual(values = c("No Collusion" = "gray", "Mild Collusion" = "blue",
                                "Moderate Collusion" = "orange", "Severe Collusion" = "red")) +
  labs(title = "各家廠商勾結行為的主成分分析",
       subtitle = "PCA of Mean and CV with Collusion Levels",
       caption = "Data Source: Fair Trade Commission",
       x = "Principal Component 1", y = "Principal Component 2",
       color = "Collusion Level", shape = "Collusion Level") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        legend.key = element_blank())

