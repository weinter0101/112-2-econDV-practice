ggplot()    # 建立畫布
  +geom_point()   # 將圖形'點'疊加上去
  +geom_line()    # 將圖形'線'疊加上去 (線會在點上)

geom_point(fill, size, x, y)    # 美學元素

# geom函數後的抉擇

geom_jitter()   # 當點過度密集，會將各點用亂數打亂，易看出密集度
geom_point()

geom_path()   # 依照時間
geom_line()   # 依照資料順序
geom_step()   # 不將資料連續化


# 美學元素

ggplot(data=df)+
  geom_point(
    mapping=aes(
      fill=COT, x=G, y=CO2
    )
  )
# 會變得寫在mapping，固定的再最後再加上, ........

# 顏色通透度設定 alpha = (0,1)


#plotly

#ggplotly() # 互動式圖片 點可以顯示data 