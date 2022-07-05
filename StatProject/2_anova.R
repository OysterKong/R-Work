##### One Way ANOVA #####
#LDLC : 저밀도 콜레스테롤 수치 -> 종속(결과) 변수
# Dx(진단결과) : STMI(급성심근경색), NSTEMI(만성심근경색), unstable angina(협심증) -> 독립변수 (결과에 영향을 미치는)

library(moonBook)
str(acs)

# 3개의 진단결과에 대한그래프
moonBook::densityplot(LDLC ~ Dx, data=acs)

# 정규분포 검정 ( 정규분포에서는 하나라도 정규분포가 아니면 결과는 정규분포가 아닌 점 기억하자 )
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"]))    # 정규분포가 아니다   (p-value = 1.56e-08)
with(acs, shapiro.test(LDLC[Dx=="STEMI"]))     # 정규분포다  (p-value = 0.6066)
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"]))     # 정규분포가 아니다 (p-value = 2.136e-07)

# 정규분포를 확인하는 또 다른 방법
out = aov(LDLC ~ Dx, data=acs)
out
shapiro.test(resid(out))  # p-value = 1.024e-11 - 정규분포가 아니다.  3개 중 하나라도 정규분포가 아니면 아니다.

# 등분산 여부  ( 위에서 결과가 정규분포가 아니지만 정규분포라고 가정하고 등분산 여부를 확인해보자 )
bartlett.test(LDLC ~ Dx, data=acs)    # p-value = 0.1857 로 등분산 이다.

### 최종 결론 : anova 사용 (위에서 정규분포 확인하면서 aov 함수를 사용했으므로 여기선 생략 - 주석처리)
# out = aov(LDLC ~ Dx, data=acs)
summary(out)     # Pr(>F) 0.00377 로 p.value 가 0.05보다 작으므로 차이가 있다 - 대립가설 (** - 별이 많을수록 차이가 크다)

### 최종 결론 : Kruskal-wallis 사용
kruskal.test(LDLC ~ Dx, data=acs)   # p-value = 0.004669 로 차이가 있다 - 대립가설

### 최종 결론 : welch's anova 사용
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)   # p-value = 0.007471 로 차이가 있다 - 대립가설



### 사후 검정
# avo() 사용했을 경우 : TukeyHSD()
TukeyHSD(out)


# kruskal.test() 사용했을 경우
install.packages("pgirmess")
library(pgirmess)

kruskalmc(acs$LDLC, acs$Dx)       # 사용법은 종속변수와 독립변수를 넘겨주고 TRUE, FALSE 로 차이를 나타냄


# oneway.test() 사용했을 경우
install.packages("nparcomp")
library(nparcomp)

result <- mctp(LDLC ~ Dx, data=acs)
summary(result)


##### 실습1 : iris샘플 #####
### 주제 : 품종별로 sepal.Width의 평균차이가 있는가 ?
### 만약 있다면 어느 품종과 차이가 있는가 ?
head(iris)


moonBook::densityplot(Sepal.Width ~ Species, data=iris)


# 정규분포 검정
# with(iris, shapiro.test(Sepal.Width[Species=="setosa"]))   # 정규분포, p-value = 0.2715
# with(iris, shapiro.test(Sepal.Width[Species=="versicolor"]))   # 정규분포, p-value = 0.338
# with(iris, shapiro.test(Sepal.Width[Species=="virginica"]))   # 정규분포, p-value = 0.1809

out <- aov(Sepal.Width ~ Species, data=iris)  # p-value = 0.323 , 정규분포
shapiro.test(resid(out))


# 등분산 여부
bartlett.test(Sepal.Width ~ Species, data=iris)  # p-value = 0.3515 으로 등분산이다.

# 최종결론
# out = aov(Sepal.Width ~ Species, data=iris)
summary(out)   # Pr(>F) 가 <2e-16 으로 0.05보다 작으므로 차이가 있다 - 대립가설 (*** 로 차이가 엄청많다.)

TukeyHSD(out)  # 셋 다 차이가 있다. (p adj 확인 - 셋 다 0.05보다 작음)
# 세 품종 중에서도 setosa 와 차이가 있다.



##### 실습2 #####
### 주제 : 시, 군, 구별로 합계 출산율의 차이가 있는가 ?
### 있다면 어느것과 차이가 있는가 ?
  
mydata <- read.csv("../data/anova_one_way.csv")
View(mydata)
str(mydata)

# 정규분포 여부 확인
out1 <- aov(birth_rate ~ ad_layer, data=mydata)  # p-value = 5.788e-07 로 정규분포 아님
shapiro.test(resid(out1))

# 정규분포가 아니니 Kruskal-wallis H test 사용
kruskal.test(birth_rate ~ ad_layer, data=mydata)   # p-value < 2.2e-16 로 차이가 있다 - 대립가설

# 정규분포일때를 가정하고 summary로 확인
summary(out1)  # 정규분포일때로 가정하고 결과를 확인해도 Pr(>F)가 <2e-16 *** 로 차이가 많다

moonBook::densityplot(birth_rate ~ ad_layer, data=mydata)

# 사후검정
kruskalmc(mydata$birth_rate, mydata$ad_layer)   # 구와 군, 구와 시  가 차이가 있다 (TRUE 값)

TukeyHSD(out1)  # 정규분포일때를 가정하고 Tukey로 확인 (정규분포일때 값과도 같으면 결과의 정확도가 더 높다)



##### 실습3 #####
### 주제 : 지불방식별로 총 지불금액이 차이가 있는가 ?
### 종속 변수 : TotalCharges
### 독립 변수 : PaymentMethod (Bank transfer, Credit card, Electronic check, Mailed check)
telco <- read.csv("../data/Telco-Customer-Churn.csv")
View(telco)

str(telco)
table(telco$PaymentMethod)
unique(telco$PaymentMethod)

# 평균을 확인해보기
library(dplyr)
telco %>% select(PaymentMethod, TotalCharges) %>%
  group_by(PaymentMethod) %>%
  summarise(count=n(), mean=mean(TotalCharges, na.rm=T))

# 데이터의 수가 방대할땐 그래프가 정규분포 모형이 아니어도 정규분포라고 봐도 무방하다.
moonBook::densityplot(TotalCharges ~ PaymentMethod, data=telco)


# 정규분포 여부 확인
out2 <- aov(TotalCharges ~ PaymentMethod, data=telco)
shapiro.test(resid(out2)) # shapiro.test는 데이터가 5000개가 넘어가면 검증할 수 없다.

with(telco, shapiro.test(TotalCharges[PaymentMethod=="Bank transfer (automatic)"])) # p-value < 2.2e-16 -정규분포아님
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Credit card (automatic)"])) # p-value < 2.2e-16 -정규분포아님
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Electronic check"])) # p-value < 2.2e-16 -정규분포아님
with(telco, shapiro.test(TotalCharges[PaymentMethod=="Mailed check"])) # p-value < 2.2e-16 -정규분포아님

# 데이터의 양이 방대해지면 정규분포테스트를 할 필요가 없다. - 모양은 정규분포가 아니나 정규분포라고 봐도 무방


# 앤더슨 달링 테스트


# 등분산 여부
bartlett.test(TotalCharges ~ PaymentMethod, data=telco)  # p-value = 2.2e-16 으로 등분산이 아니다.

# 최종결론 - 등분산이 아니므로 Welch's anova 의 oneway.test 시행
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)  # p-value < 2.2e-16  = 차이가 있다.

# oneway.test() 사용했을 경우
# install.packages("nparcomp")
library(nparcomp)

result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

# 결과를 보면 2 - 1 은 Bank transfer (automatic) 와 Credit card (automatic) 의 차이는 p.Value = 0.9507703로
# 차이가 없다.  실제 평균도 3079 와 3071 로 차이가 없다.

plot(result)

ggplot(telco, aes(PaymentMethod, TotalCharges)) + geom_boxplot()

TukeyHSD(out2)

kruskal.test(TotalCharges ~ PaymentMethod, data=telco)
kruskalmc(telco$TotalCharges, telco$PaymentMethod)

#####################################################################################################################

##### Two Way ANOVA #####

mydata <- read.csv("../data/anova_two_way.csv")
View(mydata)

out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data=mydata)
shapiro.test(resid(out))   # p-value = 2.862e-06 로 정규분포가 아니다

# 정규분포가 아니지만 편의상 정규분포라고 가정하고 결과를 사용
summary(out)

TukeyHSD(out)


# 군의 경우 실시한 곳과 실시하지 않은 곳 간에 차이가 나고 특히 실시한 군은 다른 행정구역에 비해 출산율이 
# 높은 것으로 보입니다. 반면에 실시하는 시, 실시하지 않는 시와 실시하는 구, 실시하지 않는 구 간에는 출산율에 
# 차이가 없는 것 같습니다. 효과가 나타난 군에서 다자녀 정책을 강화하거나 시,구의 다자녀 정책을 수정할 필요가 
# 있을 것 같습니다.
# 다자녀정책이 효과가 없는 시, 구에서는 다른 정책방향을 검토해 볼 필요가 있다.





##### 실습1 #####
### 결과 변수 : TotalCharges
### 원인 변수 : PaymentMethod, Contract

telco <- read.csv("../data/Telco-Customer-Churn.csv")
View(telco)

out0 <- aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract, data=telco)
summary(out0)

result <- TukeyHSD(out0)
plot(result)

ggplot(data=telco, aes(PaymentMethod, TotalCharges, col=Contract)) + geom_boxplot()
# 그래프결과 : 지불방식과는 상관없이 2년마다 계약하는 방법이 가장 많은 돈을 내고 있다.
# Mailed check 방식은 contract 계약기간마다 큰 변화가 없으나 나머지 방식들은 계약기간마다 금액차이가 난다.
# 텔레마케터 영업하는 입장에서는 고객들에게 2년마다 계약하는 방식으로 영업을 하도록 유도하는 것이 좋겠다.


####################################################################################################################

##### RM ANOVA #####
### 구형성(Sphericity) : 이미 독립성이 깨졌으므로 최대한 독립성과 무작위성을 확보하기 위한 조건을 의미
### 가정 : 반복 측정된 자료들의 시차에 따른 분산이 동일
###       1) Mouchly의 단위행렬 검정 : p-value 값이 0.05보다 커야 한다.
###       2) 만약 0.05보다 작다면 Greenhouse를 사용한다. : 값이 1에 가까울수록 구형성 타당




# id pre three_month six_month
# 1  1  45          50        55
# 2  2  42          42        45
# 3  3  36          41        43
# 4  4  39          35        40
# 5  5  51          55        59
# 6  6  44          49        56


df = data.frame()
df = edit(df)
df

means <- c(mean(df$pre), mean(df$three_month), mean(df$six_month))
means

plot(means, type="o", lty=2, col="red")


# 또는
out <- aov(value ~ variable, data=rmlong)
shapiro.test(resid(out))

TukeyHSD(out)



install.packages("car")
library(car)

multimodel <- lm(cbind(df$pre, df$three_month, df$six_month) ~ 1)
multimodel

trials <- factor(c("pre", "three_month", "six_month"))
trials

?Anova
model1 <- Anova(multimodel, idata=data.frame(trials), idesign=~trials, type="III")
summary(model1, multivariate=F)



### 사후 검정
library(reshape2)
rmlong <- melt(df, id.vars="id")
rmlong

pairwise.t.test(rmlong$value, rmlong$variable, paired = T, p.adjust.method = "bonferroni")


##### 실습1 #####
### 주제 : 7명의 학생이 총 4번의 시험을 보았다. 평균 차이가 있는가 ?
### 있다면 어디에서 차이가 있는가 ?
mydata <- read.csv("../data/onewaysample.csv")
mydata

# x - index 삭제하고 다시 정렬
mydata <- mydata[ ,2:6]
mydata

means <- c(mean(mydata$score0), mean(mydata$score1), mean(mydata$score3), mean(mydata$score6))
means

plot(means, type="o", lty=2, col="red")

# 선형모델 만들기
multimodel <- lm(cbind(mydata$score0, mydata$score1, mydata$score3, mydata$score6) ~ 1)
multimodel

trials <- factor(c("score0", "score1", "score3", "score6"))
trials

model1 <- Anova(multimodel, idata=data.frame(trials), idesign=~trials, type="III")
summary(model1, multivariate=F)   #  Pr(>F) 가 1.473e-14 *** 로 차이가 있다.

### 사후 검정
library(tidyr)


rmlong <- gather(mydata, key="ID", value="score")
rmlong <- rmlong[8:35, ]
rmlong

pairwise.t.test(rmlong$score, rmlong$ID, paired = T, p.adjust.method = "bonferroni")

# 313 코드 실행 후 결과
#        score0  score1  score3 
# score1 0.00085 -       -      
# score3 5.9e-06 0.00031 -      
# score6 4.8e-06 5.0e-05 0.00038

# 각 스코어끼리 차이가 있다. 0.05보다 작다.














