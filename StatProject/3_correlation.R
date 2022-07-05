##### 실습1 #####
### 주제 : 담배값 인상 전의 월별 매출액과 인상 후의 월별 매출액이 관계가 있는가 ?

x <- c(70, 72, 62, 64, 71, 76, 0, 65, 75, 72)
y <- c(70, 74, 65, 68, 72, 74, 61, 66, 76, 75)

# 정규분포일 때 pearson 의 상관계수
cor(x, y, method="pearson")    # 0.7767669 로 강한관계

# 정규분포가 아닐때 spearman 의 상관계수
cor(x, y, method="spearman")   # 0.929878 로 강한관계

# 정규분포가 아닐때 kendal의 tau
cor(x, y, method="kendal")   # 0.8409091 로 강한관계

cor.test(x, y, method="pearson")    # 관계여부 외에 t-test 여부까지 함께 나오는 cor.test



##### 실습2 #####
# pop_growth : 인구 증가율
# eldery_rate : 65세 이상 노령인구 비율
# finance : 재정 자립도
# cultural_center : 인구 10만명당 문화기반 시설 수

df <- read.csv("../data/cor.csv")
View(df)
str(df)

cor(df$pop_growth, df$elderly_rate)   # -0.4069218 로 음의관계 (인구증가율이 올라갈수록 노령인구는 줄어드는 관계)
plot(df$pop_growth, df$elderly_rate)  # 그래프로도 - 음의상관관계를 확인해보자

# 많은 변수에 대해 상관분석을 해야 한다면
x <- cbind(df$pop_growth, df$birth_rate, df$elderly_rate, df$finance, df$cultural_center)
cor(x)

# 노령인구수와 인구10만명당 문화기반 시설 수의 상관관계 확인
plot(df$cultural_center, df$elderly_rate)

# plot으로 만든 그래프에 선을 생성하기
out <- lm(df$elderly_rate ~ df$cultural_center, data=df)
out
abline(out, col="red")



##### 실습3 #####
install.packages("UsingR")
library(UsingR)

str(galton)

plot(galton$child, galton$parent)
plot(jitter(galton$child, 5), jitter(galton$parent, 5))    # 모여있던 점을 분산

cor.test(galton$child, galton$parent)  # 0.4587624 - 부모의 키가 크다고 자식도 다 큰 것은 아니다.

out <- lm(galton$child ~ galton$parent, data=galton)
out
abline(out, col="red")


sunflowerplot(galton$child, galton$parent)



##### 실습4 #####
install.packages("SwissAir")
library(SwissAir)

View(AirQual)

# 세 관측소에서 관측한 오존농도, 일산화질소, 이산화질소를 30분마다 측정한 결과의 합
Ox <- AirQual[ , c("ad.O3", "lu.O3", "sz.O3")] + 
  AirQual[ , c("ad.NOx", "lu.NOx", "sz.NOx")] -
  AirQual[ , c("ad.NO", "lu.NO", "sz.NO")]

names(Ox) <- c("ad", "lu", "sz")
head(Ox)

plot(lu ~ sz, data=Ox)

install.packages("hexbin")
library(hexbin)

bin <- hexbin(Ox$lu, Ox$sz, xbins=50)
bin
plot(bin)

smoothScatter(Ox$lu, Ox$sz)

install.packages("IDPmisc")
library(IDPmisc)
iplot(Ox$lu, Ox$sz)
