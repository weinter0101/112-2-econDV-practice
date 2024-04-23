library(ggplot2)

str(diamonds)
# carat: 鑽石的重量
# cut: 鑽石的切割品質
# color: 鑽石的顏色
# clarity: 鑽石的純淨度

# 以data作圖
ggplot(data=diamonds, aes(x=carat, y=price)) +
  geom_point()

# 取log
ggplot(data=diamonds, aes(x=log(carat), y=log(price)))+
  geom_point()

# 以體積對重量作圖
ggplot(data=diamonds, aes(x=carat, y=x*y*z))+
  geom_point()

# 做子集合的圖
set.seed(20)
diamondsSubset <- diamonds[sample(nrow(diamonds), 100),]

ggplot(data=diamondsSubset, aes(x=carat, y=price, color=color)) +
  geom_point()

ggplot(data=diamondsSubset, aes(x=carat, y=price, shape=cut)) +
  geom_point()

ggplot(data=diamondsSubset, aes(x=carat, y=price))+
  geom_point()+
  geom_smooth()   

library(ggplot2)

ggplot(data=diamonds, aes(x=clarity)) +
  geom_bar()

ggplot(data=diamonds, aes(x=clarity, fill=cut))+
  geom_bar()

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram()

# 畫布
base <- ggplot(data=diamonds, aes(x=carat, y=price, colour=cut))

pointExanple <- base+
  layer(
    geom = "point",
    stat = 'identity',
    position = 'identity',
    params = list(na.rm=FALSE)
  )

barExample <- ggplot(data=diamonds, aes(x=carat)) +
  layer(geom='bar', stat='bin', position='identity',
        params=list('steelblue', binwidth=0.2))

        