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