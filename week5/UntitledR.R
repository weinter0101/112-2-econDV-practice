# Cost of Loving Index ----

## create results list -----
results <- list()

## example data ----
# Load required packages
library(tidyverse)

# Read data from image into a data frame
cost_of_loving <- data.frame(
  City = c("Shanghai", "New York", "Bahrain*", "Los Angeles", "Paris", 
           "Amman", "Caracas", "Moscow", "Milan", "Beijing", 
           "Barcelona", "St Petersburg", "Abu Dhabi", "Düsseldorf", "Zurich"),
  `Two-course meal for two people*` = c(548, 408, 388, 328, 308, 288, 288, 288, 288, 268, 248, 248, 248, 228, 228),
  `Bottle of fine wine†` = c(108, 108, 88, 88, 88, 68, 68, 68, 68, 48, 48, 48, 48, 48, 48),
  `Two drinks at a hotel bar` = c(60, 60, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40),
  `Two cinema tickets` = c(28, 32, 24, 28, 32, 24, 16, 24, 24, 24, 28, 16, 28, 28, 32),
  `Initial taxi meter charge` = c(8, 8, 6.5, 6, 8, 4, 2, 4, 8, 4, 8, 4, 8, 8, 12)
)

# Calculate total cost
cost_of_loving <- cost_of_loving %>%
  mutate(Total = rowSums(.[2:6]))

# Sort by total cost in descending order
cost_of_loving <- cost_of_loving %>%
  arrange(desc(Total))

# Store results in a list
results <- list(cost_of_loving = cost_of_loving)

df <- cost_of_loving %>%
  pivot_longer(cols = -c(City, Total), 
               names_to = "Category", 
               values_to = "Cost") 
## create bar chart ----
# Create a bar chart
bar_chart <- df %>%
  ggplot(aes(x = reorder(City, Total), y = Cost, fill = Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cost-of-Loving Index: Top 15 Most Expensive Cities in 2023",
       x = "City",
       y = "Cost (USD)") +
  scale_fill_discrete(name = "Category") +
  theme_minimal()

# Store bar chart in results list
results$bar_chart <- bar_chart

# 動態 
plotly::ggplotly(bar_chart)


education <- factor(c("高中", "大學", "研究所"))

edu <- factor(c("高中", "大學", "研究所"))
sampleEdu <- factor(edu, levels=c("高中", "大學", "研究所"))