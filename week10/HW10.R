library(ggplot2)

data <- data.frame(
  category = rep(c("Economics", "Finance", "Computer Science", "Leisure", "Test Preparation"), times = 4),
  value = c(30, 20, 0, 50, 0, 18, 20, 50, 12, 0, 50, 10, 0, 40, 0, 10, 12, 0, 0, 78),
  grade = rep(c("Grade 1", "Grade 2", "Grade 3", "Grade 4"), each = 5)
)

data$category <- factor(data$category, levels = c("Test Preparation", "Leisure", "Computer Science", "Finance", "Economics"))

ggplot(data, aes(x = grade, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "Test Preparation" = "orange", 
    "Leisure" = "pink", 
    "Computer Science" = "lightblue", 
    "Finance" = "blue", 
    "Economics" = "darkblue"
  )) +
  labs(
    title = "My College Life",
    subtitle = "711261124 CHEWEI",
    x = "Grade",
    y = "Value",
    caption = "Data source: Chewei College Life"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8),
  )
