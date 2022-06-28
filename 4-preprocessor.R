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
















