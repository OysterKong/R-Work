##### 데이터 탐색 #####

### 변수명 바꾸기
df_raw <- data.frame(var1=c(1, 2, 3), var2=c(2, 3, 2))
df_raw

# 기본 함수(내장 함수)
df_raw1 <- df_raw
names(df_raw1) <- c("v1", "v2")
df_raw1

# dplyr
df_raw2 <- df_raw
df_raw2 <- rename(df_raw2, v1=var1, v2=var2)
df_raw2


##### 결측치 처리 #####
ds1 <- read.csv("../data/dataset.csv")
ds1

str(ds1)
View(ds1)

# resident : 1 ~ 5까지의 값을 갖는 명목변수로 거주지를 나타낸다.
# gender : 1 ~ 2까지의 값을 갖는 명목변수로 남/녀를 나타냄
# job : 1 ~ 3까지의 값을 갖는 명목변수. 직업을 나타냄
# age : 양적변수(비율) : 2 ~ 69
# position : 1 ~ 5까지의 값을 갖는 명목변수. 직위를 나타냄
# price : 양적변수(비율) : 2.1 ~ 7.9
# survey : 만족도 조사 : 1 ~ 5까지 명목변수


# 결측치 확인
summary(ds1$price)

# 결측치 삭제
sum(ds1$price, na.rm=T)     #실제 결측치를 완전히 삭제하는건 아니지만 SUM 합계를 구할 때 결측치를 빼고 구해줌

price2 <- na.omit(ds1$price)    #na.omit(컬럼명) - 컬럼안의 결측치를 완전히 삭제
summary(price2)

### 결측치 대체 : 0으로 대체
price3 <- ifelse(is.na(ds1$price), 0, ds1$price)
summary(price3)
sum(price3)

### 결측치 대체 : 평균으로 대체
price4 <- ifelse(is.na(ds1$price), round(mean(ds1$price, na.rm=T), 2), ds1$price)
summary(price4)


##### 이상치 확인 및 처리 #####

### 질적 변수 : 도수분포표, 분할표 => 막대 그래프, 원, ...
table(ds1$gender)
pie(table(ds1$gender))

### 양적 변수 : 산술평균, 조화평균, 중앙값 => 히스토그램, 상자그림, 산포도, 시계열도표, ...
summary(ds1$price)

plot(ds1$price)
boxplot(ds1$price)

### 처리
ds2 <- subset(ds1, price >=2 & price <= 8)
summary(ds2$price)
length(ds2$price)
str(ds1)

plot(ds2$price)
boxplot(ds2$price)

summary(ds2$age)
plot(ds2$age)
boxplot(ds2$age)



##### Feature Engineering #####
View(ds2)

### 가독성을 위해 데이터 변경(1:서울, 2:인천, 3:대전, 4:대구, 5:시구군)
ds2$resident2[ds2$resident == 1] <- "1.서울특별시"
head(ds2)
ds2$resident2[ds2$resident == 2] <- "2.인천광역시"
ds2$resident2[ds2$resident == 3] <- "3.대전광역시"
ds2$resident2[ds2$resident == 4] <- "4.대구광역시"
ds2$resident2[ds2$resident == 5] <- "5.시구군"
head(ds2)

### Binning : 척도 변경 (양적 -> 질적)
### 나이 변수를 청년층(30세 이하), 중년층(31 ~ 55이하), 장년층(56~)

ds2$age2[ds2$age<=30] <- "청년층"
ds2$age2[ds2$age>30 & ds2$age<=55] <- "중년층"
ds2$age2[ds2$age>50] <- "장년층"

View(ds2)


### 역코딩       1을 5로 2를 4로 변경경
table(ds2$survey)
t_survey <- ds2$survey
t_survey

ds2$survey2 <- 6-t_survey
ds2$survey2

### Dummy : 척도 변경(질적 -> 양적)
# 거주 유형 : 단독주택(1), 다가구주택(2), 아파트(3), 오피스텔(4)
# 직업 유형 : 자영업(1), 사무직(2), 서비스(3), 전문직(4), 기타

# 더미데이터에 단독주택은 0001, 다가구주택은 0100, 아파트는 0010, 오피스텔은 0001 처럼 1의 위치로 표기
# 단독주택과 다가구주택을 합쳐서 0으로 표기, 아파트랑 오피스텔을 합쳐서 1로 표기

ds3 <- read.csv("../data/user_data.csv")
ds3

table(ds3$house_type)

# house_type2컬럼을 새로 추가해서 단독과 다가구는 0으로, 아파트와 오피스텔은 1로 변환
ds3$house_type2[ds3$house_type ==1 | ds3$house_type ==2] <- 0
ds3$house_type2[ds3$house_type ==3 | ds3$house_type ==4] <- 1
head(ds3)


ds3$house_type2 <- ifelse(ds3$house_type==1 | ds3$house_type==2, 0, 1)
table(ds3$house_type2)

### 데이터 구조 변경(wide type, long type)
### melt() => long형으로 변경, cast() => wide 형으로 변경
### reshape, reshape2, tidyr, ...
install.packages("reshape2")
library(reshape2)

str(airquality)
head(airquality)

# wide 형을 long형으로
m1 <- melt(airquality, id.vars=c("Month", "Day"), variable.name="climate_name", value.name="climate_value")
View(m1)

# long형을 wide형으로    (데이터, 바꾸지않을 기준컬럼 ~ 바꿀컬럼)
?dcast
dcast(m1, Month+Day ~ climate_name)

### 에제1
df1 <- read.csv("../data/data.csv")
View(df1)

# 날짜별로 컬럼을 wide하게 변경
df2 <- dcast(df1, Customer_ID ~ Date)

# 다시 long형으로 변경
melt(df2, id.vars="Customer_ID", variable.name = "Date", value.name = "Buy")


##### 실습 : 극단적 선택의 비율은 어느 연령대가 가장 높은가 ? (사망원인 통계) #####
### 부제 : 자살 방지를 위한 도움의 손길은 누구에게 ? #####

data <- read.csv("../data/2019_suicide.csv")
str(data)

man <- data[21:39, c(3, 4, 5)]
man

woman <- data[40:58, c(4, 5)]
woman

total <- cbind(man, woman)

names(total) <- c("연령", "남자사망자수", "남자사망률", "여자사망자수", "여자사망률률")
total

total$age <- 0
step <- 5

for(i in 1:19){
  total[i, "age"] <- step
  step <- step + 5
}

total

# 20 살 이하는 10, 21~30은 20, 31~40은 30, ...
total$age2[total$age<=20] <- 10
total$age2[total$age>20 & total$age<=30] <- 20
total$age2[total$age>30 & total$age<=40] <- 30
total$age2[total$age>40 & total$age<=50] <- 40
total$age2[total$age>50 & total$age<=60] <- 50
total$age2[total$age>60 & total$age<=70] <- 60
total$age2[total$age>70 & total$age<=80] <- 70
total$age2[total$age>80 & total$age<=90] <- 80
total$age2[total$age>90] <- 90

total
