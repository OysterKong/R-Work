##### 기본 함수 #####

### plot()
### plot(y축 데이터, 옵션)
### plot(x축 데이터, y축 데이터, 옵션)

y <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
plot(y)

x <- 1:10
y <- 1:10

plot(x, y)

# x축 limit를 0부터 20까지, y축 limit를 0부터 30까지, main은 제목
# type="p", "l", "b", "o", "n"
# pch=숫자  - 숫자에 따라서 점의 모양이 바뀜
# cex=숫자 - 숫자에 따라 점의 크기가 바뀜
# col=글자 또는 숫자 - 점의 색상을 지정가능
# lty="solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
plot(x, y, xlim=c(0, 20), ylim=c(0, 30), main="Graph", type="l", pch=2, cex=2.0, col="red", lty="dashed")


str(cars)
plot(cars, type="o")

# 같은 속도일때 제동거리가 다를 경우 대체적인 추세를 알기 어렵다.
# 속도에 대한 평균 제동거리를 구해서 그래프로 그려보자
# tapply - 그룹별로 묶어서 처리
plot(tapply(cars$dist, cars$speed, mean), type="o", xlab="speed", ylab="dist")



### points()
head(iris)
# plot(iris$Sepal.Width, iris$Sepal.Length)
with(iris, plot(Sepal.Width, Sepal.Length))
# plot(iris$Petal.Width, iris$Petal.Length)
with(iris, plot(Petal.Width, Petal.Length))

with(iris, {
  plot(Sepal.Width, Sepal.Length) 
  plot(Petal.Width, Petal.Length)
  })

with(iris, points(Petal.Width, Petal.Length))



### lines()
plot(cars)
lines(cars)


### barplot(), hist(), pie(), mosaicplot(), pair(), persp(), contour(), ...


##### 그래프 배열 #####
head(mtcars)

# 4개의 그래프를 동시에 그리기
par(mfrow=c(2, 2))
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)
boxplot(mtcars$wt)


par(mfrow=c(1, 1))     # 다시 초기화 (화면 전체에 그래프 채우기)
plot(mtcars$wt, mtcars$mpg)


# 행또는 열마다 그래프 개수를 다르게 설정가능
?layout
layout(matrix(c(1, 2, 1, 3), 2, 2, byrow=T))
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)

par(mfrow=c(1, 1))

##### 특이한 그래프 #####

### arrows
x <- c(1, 3, 6, 8, 9)
y <- c(12, 56, 78, 32, 9)

plot(x, y)
arrows(3, 56, 1, 12)
text(4, 40, "이것은 샘플", srt=60)


### 꽃잎 그래프
x <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 6)
y <- c(2, 1, 4, 2, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)

plot(x, y)


?sunflowerplot
sunflowerplot(x, y)


### 별 그래프
# 데이터의 전체적인 윤곽을 살펴보는 그래프
# 데이터 항목에 대한 변화의 정도를 한눈에 파악
str(mtcars)

# 범례 - key.loc=c(X축위치, Y축위치)  / 횡 단위 정렬 = flip.labels=F / 원형시각화 = draw.segments=T
stars(mtcars[1:4], key.loc=c(13, 2.0), flip.labels=T, draw.segments=T )





##### ggplot2 #####
### http://www.r-graph-gallery.com/ggplot2-package.html
### 레이어 지원
###   1) 배경 설정
###   2) 그래프 추가(점, 선, 막대, ...)
###   3) 설정 추가(축 범위, 범례, 색, 표식, ...)




# install.package("ggplot2")
library(ggplot2)
library(dplyr)

### 산포도
head(mpg)

# aes(x축, y축)
ggplot(data=mpg, aes(x=displ , y=hwy )) + geom_point() + xlim(3, 6) + ylim(10, 30)
ggplot(data=mpg, aes(displ, hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)


### 막대그래프 : geom_col(), 히스토그램 : geom_bar()
# 구동방식(drv)별로 고속도로 평균연비를 조회하고 그 결과를 그래프로 표현

df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))
df_mpg

ggplot(data=df_mpg, aes(drv, mean_hwy)) + geom_col()
ggplot(data=df_mpg, aes(reorder(drv, mean_hwy), mean_hwy)) + geom_col()

ggplot(data=mpg, aes(drv)) + geom_bar()
ggplot(data=mpg, aes(hwy)) + geom_bar()








# install.packages("ggplot2")

