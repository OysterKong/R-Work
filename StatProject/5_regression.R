##### 단순(단일) 회귀 분석 #####   ------- x : 데이터가 하나를 의미
### y = ax + b                 ------> 직선의 방정식

str(women)  # 미국 여성을 대상으로 키와 몸무게를 조사(30~39세)
women

plot(weight ~ height, data=women)

fit <- lm(weight ~ height, data=women)
fit   # Intercept = -87.52 절편 을 의미, height = 3.45 기울기를 의미

abline(fit, col="blue")  # 파랑 선 긋기
summary(fit)  # Multiple R-squared (설명계수) :  0.991 

cor.test(women$weight, women$height)

# 상관분석에서 측정값은 R계수라고 하고 이를 상관계수라고 함(관계가 있는지 없는지)
# 회귀분석에서는 R에다가 제곱(^2)을 해서 나온 값을 설명계수라고 함

0.9954948^2

# y = ax + b
62*3.45 - 87.52  # 몸무게가 62일때 기울기인 3.45를 곱하고 절편인 -87.52를 더하면 키를 알 수 있음(126.38)

# 4가지 조건(그래프)을 확인하기 위한 방법
plot(fit)

##########################################################################################################
# 그래프 여러개를 한 화면에 그려보기 - 다중배열
par(mfrow=c(2, 2))   # 4칸으로 설정
plot(fit) # 나눠진 4칸에 다시 그래프 띄우기

# Residuals vs Fitted ( 선형성 )
# 잔차의 평균은 0이고 분산은 일정해야함.
# 선에 모여있는게 아니라 자연스럽게 흩어져 있어야 선형성을 만족하는 것이다.


# Normal Q-Q 도 ( 정규성 )
# 그려진 선을 기준으로 선에서 점들이 떨어져 있으면 정규분포가 아니다.

# Scale- Location - 등분산선 ( 등분산을 확인하는 선 )
# 데이터가 자연스럽게 흩어져있어야 등분산을 만족하는 것

# Residuals vs Leverage ( 이상치 ) - 다중회귀분석에서 사용
##########################################################################################################

# 정규성 확인
shapiro.test(resid(fit))     # p-value = 0.1866 로 정규분포이다

### 다항 회귀 분석
par(mfrow=c(1,1))

plot(weight ~ height, data=women)
fit <- lm(weight ~ height, data=women)
abline(fit, col="blue")

fit2 <- lm(weight ~ height + I(height^2), data=women)
fit2

plot(weight ~ height, data=women)
lines(women$height, fitted(fit2), col="red")

shapiro.test(resid(fit2))    # p-value = 0.506 로 정규분포이다.

par(mfrow=c(2, 2))
plot(fit2)




##### 실습1 #####
# social_welfare : 사회 복지 시설
# active_firms : 사업체 수
# urban_park : 도시 공원
# kris : 폐수 배출 업소
# kindergarten : 유치원

# 종속 변수 : birth_rate
# 독립 변수 : kindergarten

# 가설 : 유치원 수가 많은 지역에 합계 출산율도 높은가 ?
# 합계 출산율이 유치원수에 영향을 받는가 ?


df <- read.csv("../data/regression.csv")
View(df)
str(df)

fit <- lm(birth_rate ~ kindergarten, data=df)
summary(fit)   # 유치원때문에 영향은 있으나 좀 약함 0.0142 *  ( 0.05보다 작으므로 영향은 있다로 보이나... )
              # 영향은 있으나 인과관계를 따지기는 어렵다. # Multiple R-squared (설명계수) :  0.03945 로 낮다.
par(mfrow=c(2, 2))
plot(fit)

shapiro.test(resid(fit))

fit2 <- lm(log(birth_rate) ~ log(kindergarten), data=df)   # log를 통해 약간의 보정...
summary(fit2)

plot(fit2)

shapiro.test(resid(fit2))


### 시, 군, 구와 관계가 있을까 ?
fit3 <- lm(birth_rate ~ dummy, data=df)
summary(fit3)  # 시, 군, 구와 출산율이 영향은 있으나 좀 약함. 0.0159 * ( 0.05보다 작으므로 영향은 있다로 보이나... )

shapiro.test(resid(fit3))


##### 실습2 #####
### 출처 : www.kaggle.com : House sales price in Kings county, USA
# 가설 : 거실의 크기와 집 가격이 서로 관계가 있는가 ?
# 종속 변수 : price
# 독립 변수 : sqft_living

house <- read.csv("../data/kc_house_data.csv")
str(house)

fit4 <- lm(price ~ sqft_living, data=house)
summary(fit4)       # <2e-16 *** 로 거실의 크기와 집가격이 관계가 있다.  Multiple R-squared:  0.4929 (40%) 설명력

plot(fit)

par(mfrow=c(1, 1))
plot(house$sqft_living, house$price)     # 양의 상관관계

# 예를 들어 거실이 770 일시 집 값을 절편과 기울기를 통해 구해볼 수 있음
770 * 280 - 43580.743     # 거실 평수 * 절편 + 기울기 = 집값  ( 집 값은 172019.3 )
