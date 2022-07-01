##### one sample t-test #####

### A회사의 건전지 수명이 1000시간일때, 무작위로 뽑아 10개의 건전지 수명에 대해 샘플이 모집단과 다르다고 할 수 있는가 ?

# 귀무가설 : 표본의 평균은 모집단의 평균과 같다.
# 대립가설 : 표본의 평균은 모집단의 평균과 다르다.

a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

t.test(a, mu=1000, alt="less")
t.test(a, mu=1000, alt="greater")
t.test(a, mu=1000, alt="two.sided")


##### independent two sample t-test #####
install.packages("moonBook")
library(moonBook)

# 경기도 소재 대학병원에서 2년동안 내원한 급성 관상동맥증추군 환자 데이터
?acs
head(acs)
str(acs)
summary(acs)

### 주제 : 두 집단(남성, 여성)의 나이 차이를 알고 싶다.

mean.man <- mean(acs$age[acs$sex=="Male"])
mean.woman <- mean(acs$age[acs$sex=="Female"])
cat(mean.man, mean.woman)


### 가설 검정
#귀무 가설 : 남성과 여성의 나이에 대해 차이가 없다.
#대립 가설 : 남성과 여성의 나이에 대해 차이가 있다.

### 정규분포 테스트
# 귀무가설 : 정규분포와 같다
# 대립가설 : 정규분포와 다르다

moonBook::densityplot(age ~ sex, data=acs)    # age는 종속변수, sex는 독립변수, data 순서

shapiro.test(acs$age[acs$sex=="Male"])
shapiro.test(acs$age[acs$sex=="Female"])


### 등분산 테스트
# 두 집단의 등분산 여부를 알고 싶다.
# 귀무 가설 : 두 집단은 등분산이다.
# 대립 가설 : 두 집단은 등분산이 아니다.

var.test(age ~ sex, data=acs)

### 가설 검정

# MWW
wilcox.test(age ~ sex, data=acs)

# t-test
t.test(age ~ sex, data=acs, alt="two.sided", var.equal=T)

# welch's test
t.test(age ~ sex, data=acs, alt="two.sided", var.equal=F)   # t-test에서 var.equal을 F로 바꿔주면 welch's test


##### paired sample t-test #####

str(sleep)
sleep          # 같은 집단을 2번 비교하는


### 주제 : 같은 집단에 대해 수면시간의 증가량 차이가 나는지 알고 싶다.
#귀무 가설 : 수면시간의 증가량 차이가 없다.
#대립 가설 : 수면시간의 증가량 차이가 있다.

mean(sleep$extra[sleep$group==1])
mean(sleep$extra[sleep$group==2])

# 정규분포 여부
#귀무 가설 : 정규분포와 같다.
#대립 가설 : 정규분포와 다르다.
moonBook::densityplot(extra ~ group, data=sleep)
# with(sleep, shapiro.test(extra[group==1]))
shapiro.test(sleep$extra[sleep$group==1])  # 정규분포와 같다
shapiro.test(sleep$extra[sleep$group==2])  # 정규분포와 같다

# 등분산 여부
# 귀무 가설 : 두 집단은 등분산이다.
# 대립 가설 : 두 집단은 등분산이 아니다.
var.test(extra ~ group, data=sleep)


# t-test   ( paired sample t-test 에서 paired라는 옵션을 사용, 기본값은 F )

t.test(extra ~ group, data=sleep, alt="two.sided", var.equal=T, paired=T)
#대립 가설 : 수면시간의 증가량 차이가 있다 - 채택



### ID를 제거하여 서로 다른 두 집단으로 테스트를 해보자.
sleep2 <- sleep[, -3]   # ID제거 코드
sleep2

shapiro.test(sleep2$extra[sleep2$group==1])  # 정규분포와 같다
shapiro.test(sleep2$extra[sleep2$group==2])  # 정규분포와 같다

var.test(extra ~ group, data=sleep2) # 두 집단은 등분산이다

# 두 그룹을 구분하는 ID를 지우니 paired 옵션을 F로 지정
t.test(extra ~ group, data=sleep2, alt="two.sided", var.equal=T, paired=F)
#귀무 가설 : 수면시간의 증가량 차이가 없다 - 채택


### 그래프(시각화)
before <- subset(sleep, group==1, extra)
before

after <- subset(sleep, group==2, extra)
after

install.packages("PairedData")
install.packages("gld")
library(PairedData)

ab <- paired(before, after)
ab

plot(ab, type="profile") + theme_bw()



##### Power Analysis #####
# 적정한 표본의 갯수를 산출
# cohen's d (effective size)


ky <- read.csv("../data/KY.csv")
View(ky)

table(ky$group)

install.packages("pwr")
library(pwr)

# 두 집단의 평균
mean.1 <- mean(ky$score[ky$group == 1])
mean.2 <- mean(ky$score[ky$group == 2])
cat(mean.1, mean.2)

# 두 집단의 표준편차차
sd.1 <- sd(ky$score[ky$group == 1])
sd.2 <- sd(ky$score[ky$group == 2])
cat(sd.1, sd.2)

effect_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2) / 2)
 
# d에 들어갈 cohen의 공식은 두 집단의 평균차이를 표준편차로 나눈 값값
pwr.t.test(d=effect_size, type="two.sample", alt="two.sided", power=0.8, sig.level=0.05)






##### 실습1 #####

### 주제 : 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보려고 한다.
### dummy : 0은 군을 나타내고, 1은 시를 나타낸다.
## 귀무 가설 : 차이가 없다.
## 대립 가설 : 차이가 있다.

mydata <- read.csv("../data/independent.csv")
View(mydata)

# 군과 시의 출산율 평균을 먼저 알아보기
gun.mean = with(mydata, mean(birth_rate[dummy == 0]))
si.mean = with(mydata, mean(birth_rate[dummy == 1]))
cat(gun.mean, si.mean)

# 정규분포 여부를 확인하기기 ( 정규분포가 해당이 되야 등분산 테스트가 필요한 것 )
with(mydata, shapiro.test(birth_rate[dummy == 0]))  # 정규분포가 아니다.
with(mydata, shapiro.test(birth_rate[dummy == 1]))  # 정규분포가 아니다.


wilcox.test(birth_rate ~ dummy, data=mydata)  # p-value 가 0.04152 로 0.05 보다 작으므로 차이가 있다

# 등분산 테스트
var.test(birth_rate ~ dummy, data=mydata) # p-value = 6.118e-05 로 등분산이 아니다

# 등분산이 아니므로 welch's test
t.test(birth_rate ~ dummy, data=mydata, alt="two.sided", var.equal=F)



##### 실습2 #####
### 주제 : 오토나 수동에 따라 연비가 같을까 ? 다를까 ?
### am:0은 오토, 1이 수동, mpg는 연비
## 귀무 가설 : 차이가 없다.
## 대립 가설 : 차이가 있다.

str(mtcars)
head(mtcars)

# 오토와 수동의 연비평균 구하기
auto.mean = with(mtcars, mean(mpg[am == 0]))
manual.mean = with(mtcars, mean(mpg[am == 1]))
cat(auto.mean, manual.mean)

# 정규분포 여부를 확인하기
with(mtcars, shapiro.test(mpg[am == 0]))  # 정규분포
with(mtcars, shapiro.test(mpg[am == 1]))  # 정규분포

# 등분산 테스트
var.test(mpg ~ am, data=mtcars)  # p-value = 0.06691 로 0.05보다 크므로 등분산이다.

t.test(mpg ~ am, data=mtcars, alt="two.sided", var.equal=T)  # p-value = 0.000285 로 차이가 있다. (대립가설채택)




##### 실습 3 #####
### 주제 : 쥐의 몸무게가 전과 후의 차이가 있는지 알고 싶다.
## 귀무 가설 : 차이가 없다.
## 대립 가설 : 차이가 있다.

mydata <- read.csv("../data/pairedData.csv")
mydata

# wide 형을 long형으로 변환 (종속변수와 독립변수를 구분하기위해 데이터 가공)
library(reshape2)
data1 <- melt(mydata, id=("ID"), variable.name = "GROUP", value.name = "RESULT")
data1

install.packages("tidyr")
library(tidyr)

?gather
data2 <- gather(mydata, key="GROUP", value="RESULT", -ID)     # ID까지 같이 합쳐지므로 -ID를 통해 빼내기기
data2

# 전과 후의 몸무게 평균을 알아보기
before.mean = with(mydata, mean(before))
after.mean = with(mydata, mean(After))
cat(before.mean, after.mean)

# 정규분포 여부
#귀무 가설 : 정규분포와 같다.
#대립 가설 : 정규분포와 다르다.
shapiro.test(mydata$before)
shapiro.test(data1$RESULT[data1$GROUP=="before"])   # 정규분포와 같다
shapiro.test(data1$RESULT[data1$GROUP=="After"])    # 정규분포와 같다


# 등분산 여부
# 귀무 가설 : 두 집단은 등분산이다.
# 대립 가설 : 두 집단은 등분산이 아니다.
var.test(RESULT ~ GROUP, data=data1)     # 등분산이다

# 최종결론
t.test(RESULT ~ GROUP, data=data1, alt="two.sided", var.equal=T, paired=T)   # 대립가설 채택 - 차이가 있다
t.test(mydata$before, mydata$After, paired=T)

# 시각화 - 그래프
before <- subset(data1, GROUP=="before", RESULT)   # before , After 를 분리해서 담기기
before

after <- subset(data1, GROUP=="After", RESULT)
after

g_data <- paired(before, after)     # 분리한 before, After 데이터를 합치기기
g_data

plot(g_data, type="profile") + theme_bw()

moonBook::densityplot(RESULT ~ GROUP, data=data1)



##### 실습4 #####
### 주제 : 시 별로 2010년도와 2015년도의 출산율의 차이가 있나 ?

mydata <- read.csv("../data/paired.csv")
mydata

# gather 를 사용해서 wide를 long형으로 바꾸기
mydata1 <- gather(mydata, key="GROUP", value="RESULT", -c(ID, cities))
mydata1

# melt 를 사용해서 wide를 long형으로 바꾸기
mydata2 <- melt(mydata, id.vars=c(1, 4), variable.name = "GROUP", value.name = "RESULT")

# 정규분포
with(mydata1, shapiro.test(RESULT[GROUP=="birth_rate_2010"]))  # 정규분포가 아니다
with(mydata1, shapiro.test(RESULT[GROUP=="birth_rate_2015"]))  # 정규분포가 아니다다

# 등분산
wilcox.test(RESULT ~ GROUP, data=mydata1, paired=T)  # 등분산이 아니다

# 최종결론
t.test(RESULT ~ GROUP, data=mydata1, paired=T)  # 차이가 없다. 대립가설





##### 실습5 #####
### https://www.kaggle.com/kappernielsen/independent-t-test-example
### 주제1 : 남녀별로 각 시험에 대해 평균차이가 나는지 알고 싶다.
### 주제2 : 같은 사람에 대해서 성적의 차이가 있는지 알고 싶다. (paired t.test)
###     -첫번째 시험(g1)과 세번째 시험(g3)를 사용


mat <- read.csv("../data/student-mat.csv")
mat

#첫번째 시험인 G1의 평균 살펴보기
summary(mat$G1)
#두번째 시험인 G2의 평균 살펴보기
summary(mat$G2)
#세번째 시험인 G3의 평균 살펴보기
summary(mat$G3)

# 성별에 따른 인원수(빈도수)는 어떤가
table(mat$sex)   # 남자 208 , 여자 187


### 남녀별로 세번의 시험 평균을 비교해보자.
library(dplyr)
# 내가 원하는 데이터만 가져오기 - 성별과 각 시험점수의 평균   
# (여기서 cnt_grade...부분은 시험본 인원,  sd+g1=sd(G1) 부분은 표준편차 )
mat %>% select(sex, G1, G2, G3) %>% group_by(sex) %>%
  summarise(mean_g1=mean(G1), mean_g2=mean(G2), mean_g3=mean(G3), cnt_grade=n(),
                                                                sd_g1=sd(G1), sd_g2=sd(G2), sd_g3=sd(G3))


# 정규분포 - shapiro.test
shapiro.test(mat$G1[mat$sex=="M"])   # 정규분포가 아님
shapiro.test(mat$G1[mat$sex=="F"])   # 정규분포가 아님

shapiro.test(mat$G2[mat$sex=="M"])   # 정규분포가 아님
shapiro.test(mat$G2[mat$sex=="F"])   # 정규분포가 아님

shapiro.test(mat$G3[mat$sex=="M"])   # 정규분포가 아님
shapiro.test(mat$G3[mat$sex=="F"])   # 정규분포가 아님

# 등분산
var.test(G1 ~ sex, data=mat)   # 등분산이 맞다
var.test(G2 ~ sex, data=mat)   # 등분산이 맞다
var.test(G3 ~ sex, data=mat)   # 등분산이 맞다

# 최종결론
t.test(G1 ~ sex, data=mat, var.equal=T)   # p-value = 0.06825 으로 차이가 없다.
t.test(G2 ~ sex, data=mat, var.equal=T)   # p-value = 0.07051 으로 차이가 없다.
t.test(G3 ~ sex, data=mat, var.equal=T)   # p-value = 0.03987 으로 차이가 있다.


# wilcox test 확인해보기
wilcox.test(G1 ~ sex, data=mat)   # 차이가 없다.
wilcox.test(G2 ~ sex, data=mat)   # 차이가 있다.
wilcox.test(G3 ~ sex, data=mat)   # 차이가 있다.

# 단측 검정
t.test(G1 ~ sex, data=mat, var.equal=T, alt="greater")   # 여학생이 남학생보다 잘했는지 비교 - 0.9로 의미없는 수치
t.test(G2 ~ sex, data=mat, var.equal=T, alt="greater")   # 여학생이 남학생보다 잘했는지 비교 - 0.9로 의미없는 수치
t.test(G3 ~ sex, data=mat, var.equal=T, alt="greater")   # 여학생이 남학생보다 잘했는지 비교 - 0.9로 의미없는 수치

t.test(G1 ~ sex, data=mat, var.equal=T, alt="less")   # 근소한 차이지만 남학생이 더 잘했다 - 0.03
t.test(G2 ~ sex, data=mat, var.equal=T, alt="less")   # 근소한 차이지만 남학생이 더 잘했다 - 0.03
t.test(G3 ~ sex, data=mat, var.equal=T, alt="less")   #  남학생이 더 잘했다 - 0.01


### 같은 학생 입장에서 G1과 G3에 대해 변화가 있었는지 확인

mat %>% select(G1, G3) %>% summarise(mean_g1=mean(G1), mean_g3=mean(G3))

mydata <- gather(mat, key="GROUP", value="RESULT", "G1", "G3")    # G1 과 G3를 합치기기
View(mydata)

t.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T)   # p-value = 0.0004291 로 차이가 있다.
t.test(mat$G1, mat$G3, paired=T)  # 바로 위 코드랑 동일

# 정규분포가 아니므로
wilcox.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T)   # p-value = 0.3153 로 차이가 없다.



# 단측 검정
# 가설 : 첫번째 시험이 세번째 시험보다 더 좋을 것이다. (G1 > G3)
t.test(mydata$RESULT ~ mydata$GROUP, data=mydata, paired=T, alt="greater")   # p-value = 0.0002145

