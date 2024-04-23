# 建立日期序列
dates <- seq(as.Date("1960/1/1"), as.Date("1962/2/1"), by = "month")

# 建立國家向量
countries <- c("美元", "日元", "英鎊")

# 建立隨機基準匯率
set.seed(123)
base_rates <- runif(3, 20, 40)

# 建立匯率和匯率升值率
rates <- data.frame(
  date = rep(dates, 3),
  country = rep(countries, each = length(dates)),
  rate = rep(base_rates, each = length(dates)),
  rate_change = runif(length(dates) * 3, -0.05, 0.05)
)

for (i in 2:nrow(rates)) {
  if (rates$country[i] == rates$country[i - 1]) {
    rates$rate[i] <- rates$rate[i - 1] * (1 + rates$rate_change[i])
  }
}

# 繪製折線圖
library(ggplot2)

ggplot(rates, aes(x = date, y = rate_change, color = country)) +
  geom_line() +
  labs(
    x = "日期",
    y = "對台幣匯率升值率",
    color = "國家"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y/%m") +
  theme_bw()

view(rate_change)