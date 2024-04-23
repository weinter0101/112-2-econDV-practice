library(tidyr)
library(ggplot2)

# 引入資料 ----
exchangeData <- read.csv("week4\\exchangeData.csv")
glimpse(exchangeData)

exchangeData <- exchangeData %>%
  mutate(期間 = as.Date(paste0(gsub("M", "-", 期間), "-01"), format = "%Y-%m-%d"))

#  匯率轉換 ----
exchangeData <- exchangeData %>%
  mutate(`韓元KRW.USD` = as.numeric(`韓元KRW.USD`))

## 計算新台幣對美元的匯率 ---- 
ntd_usd_rate <- exchangeData %>% pull(`新台幣NTD.USD`)

## 計算其他貨幣的匯率對新台幣的匯率，並儲存到新的欄位
exchangeData <- exchangeData %>%
  mutate(`日圓` = ntd_usd_rate / `日圓JPY.USD`,
         `英鎊` = ntd_usd_rate / (1/`英鎊USD.GBP`),
         `港幣` = ntd_usd_rate / `港幣HKD.USD`,
         `韓元` = ntd_usd_rate / `韓元KRW.USD`,
         `美元` = `新台幣NTD.USD`) %>%
  select(`期間`, `日圓`:`美元`)

# 計算成長率 ----
exchangeData <- exchangeData %>%
  arrange(期間) %>%
  mutate(across(c("美元", "日圓", "英鎊", "港幣", "韓元"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "成長率_{.col}"))

## 改成長格式 -----

exchangeData <- exchangeData %>% 
  pivot_longer(cols = starts_with("成長率"),
               names_to = "country",
               values_to = "升值率")

## 作圖 ------

exchangeData %>%
  ggplot(aes(x = 期間, y = 升值率, color = country)) +
  geom_line() +
  labs(x = "日期（1960年1月到2024年2月）",
       y = "對台幣匯率升值率", 
       color = "國家") +
  theme_minimal()
