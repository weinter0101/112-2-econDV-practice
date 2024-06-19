library(tidyverse)
library(ggHoriPlot) 
library(ggthemes)

utils::data(climate_CPH)

cutpoints <- climate_CPH %>%
  mutate(
    outlier = between(
      AvgTemperature,
      quantile(AvgTemperature, 0.25, na.rm = TRUE) -
        1.5 * IQR(AvgTemperature, na.rm = TRUE),
      quantile(AvgTemperature, 0.75, na.rm = TRUE) +
        1.5 * IQR(AvgTemperature, na.rm = TRUE)
    )
  ) %>%
  filter(outlier)
http://127.0.0.1:20113/graphics/a4dea305-9bdf-4150-8a76-ae97d683df91.png
custom_colors <- c("#254336", "#6B8A7A", "#B7B597", "#C7B7A3", "#DAD3BE", "#F1F1F1")

climate_CPH %>%
  ggplot() +
  geom_horizon(aes(date_mine, AvgTemperature, fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_manual(values = custom_colors) +
  facet_grid(Year ~ .) +
  theme_few() +
  theme(
    panel.spacing.y = unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand = c(0, 0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('哥本哈根的每日平均氣溫', 
          '西元1995年至2019年') +
  labs(caption = "資料來源: climate_CPH dataset")
