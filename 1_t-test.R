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




