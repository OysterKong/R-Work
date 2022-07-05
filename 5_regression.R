##### 단순(단일) 회귀 분석 #####
### y = ax + b

str(women)  # 미국 여성을 대상으로 키와 몸무게를 조사(30~39세)
women

plot(weight ~ height, data=women)

fit <- lm(weight ~ height, data=women)
fit   # Intercept = -87.52 절편 을 의미, height = 3.45 기울기를 의미

abline(fit, col="blue")  # 파랑 선 긋기
summary(fit)  # R-squared:  0.991 

cor.test(women$weight, women$height)

# 상관분석에서 측정값은 R계수라고 하고 이를 상관계수라고 함(관계가 있는지 없는지)
# 회귀분석에서는 R에다가 제곱(^2)을 해서 나온 값을 설명계수라고 함

0.9954948^2

# y = ax + b
62*3.45 - 87.52  # 몸무기게 62일때 기울기인 3.45를 곱하고 절편인 -87.52를 더하면 키를 알 수 있음(126.38)

