library(tidyverse)
library(ggplot2)
library(lubridate)

TSCE<-read.csv("C:/Users/USER/OneDrive/桌面/新增 Microsoft Excel 工作表.csv",header = TRUE)

a <- ggplot(data = TSCE, aes(x = Date)) +
  geom_ribbon(aes(ymin = scale(Min10or30)-2, ymax = scale(Max10or30)+2), fill = "gray", alpha = 0.5) +
  geom_line(aes(y = scale(ROA), group = 1), color = "red", size = 1) +
  geom_line(aes(y=0),color="black",size=1)+
  geom_point(aes(y = scale(ROA)), color = "red", size = 2) +
  labs(title = "極端高低溫對ROA影響",subtitle = "Standardization", x = "Date",y = "極端氣溫(Days) / ROA (%)",caption = "資料來源：TEJ & 交通部氣象局") +
  theme_minimal()+
  theme(legend.position = "none")