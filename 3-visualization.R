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
plot(x, y, xlim=c(0, 20), ylim=c(0, 30), main="Graph", type="p")

























# install.packages("ggplot2")

