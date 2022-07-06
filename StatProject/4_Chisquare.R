##### 실습1 #####
### 주제 : 자동차의 실린더 수와 변속기의 관계가 있는지 알고 싶다.
str(mtcars)
result <- table(mtcars$cyl, mtcars$am)

chisq.test(result)   # p-value = 0.01265 로 관계가 있으나 데이터가 충분하지 않아 정확하지 않을수도 있다.

fisher.test(result) # 정확하지 않으니 fisher.test 로 다시 확인 = p-value = 0.009105 로 관계가 있다.

barplot(result)

addmargins(result)  # addmargins 는 자동적으로 행과 열의 합계를 생성해 보여준다.



##### 실습2 #####
### 주제 : ad_layer와 multichild가 관계가 있는가 ?
df <- read.csv("../data/anova_two_way.csv")
str(df)

result <- table(df$ad_layer, df$multichild)

chisq.test(result)  # p-value = 0.7133 로 관계가 없으나 데이터가 충분하지 않아 정확하지 않을수도 있다.

fisher.test(result)  # 정확하지 않으니 fisher.test 로 다시 확인 = p-value = 0.7125 로 관계가 없다.



##### 실습3 #####
### 주제 : 흡연여부와 고혈압의 유무가 서로 관계가 있는가 ?
library(moonBook)
str(acs)
View(acs)

tbl <- table(acs$HBP, acs$smoking)
tbl

chisq.test(tbl)   # p-value = 6.19e-10 로 관계가 있다.

### Chochran-Amitage Trend Test  (속성(열) 순서 바꾸기)
acs$smoking <- factor(acs$smoking, levels=c("Never", "Ex-smoker", "Smoker"))
tbl <- table(acs$HBP, acs$smoking)
tbl

chisq.test(tbl)   # 변수 하나를 factor로 만들어서 chisq.test 가 불가

# prop.trend.test(x, y)
# x : 사건이 발생한 횟수
# y : 시도한 횟수

# 고혈압이 발생한 사람의 수 (x에 해당)
tbl[2, ]

# smoking 시도 횟수(n에 해당)    - 열의 합계 구하는 colSums()
colSums(tbl)

prop.trend.test(tbl[2, ], colSums(tbl))

# 모자이크 그래프
mosaicplot(tbl, col=c("tan1", "firebrick2", "coral2"))

# 색상표 확인
colors()
demo("colors")

# 행과 열의 위치를 변경
mosaicplot(t(tbl), col=c("tan1", "firebrick2", "coral2"), xlab = "Smoking", ylab="Hypertension")

# 흡연이 나이와 관계가 있는지
mytable(smoking ~ age, data=acs)   # p = 0.000 으로 0.05보다 작으므로 관계가 있다로 봄
# 흡연이 몸무게와 관계가 있는지
mytable(smoking ~ weight, data=acs)   # p = 0.000 으로 0.05보다 작으므로 관계가 있다로 봄

# 데이터분석에서 관계여부를 확인하는 것이지 원인과 결과로 묶어서는 오류에 빠지기 쉽다.
# 여기 결과와 같이 흡연자가 고혈압에 걸린 확률이 더 적다고 흡연하면 고혈압에 걸릴확률이 낮다로 해석하면 큰일난다.
# mytable 통해 각 분포의 평균연령과 표준편차를 알아볼 수 있다.
