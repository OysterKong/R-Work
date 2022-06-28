#### 변수 ####
goods = "냉장고"

# 변수 사용 시 객체 형태로 사용하는 것을 권장

goods.name = "냉장고"
goods.code = "rf001"
goods.price = 1000000

goods.name

goods.name <- "Television"      # R 에서는 = 대신에 <- 를 사용하는걸 권장 (교재 60 페이지)
goods.code <- "tv001"
goods.price <- 2000000

# 데이터 타입 확인

class(goods.name)
class(goods.price)
mode(goods.name)
mode(goods.price)



#### vector ####
# 1. 기본 자료 구조
# 2. 1차원 배열
# 3. 인덱스 접근
# 4. 동일한 자료형만 사용
# 5.  c(), seq(), rep(), ...

#### vector를 만드는 방법
v <- c(1, 2, 3, 4, 5)
v
class(v)
mode(v)

(v <- c(1, 2, 3, 4, 5))      # 전체를 ()로 묶어주면 바로 출력이 가능하다

c(1:5)    # R에서 기본 시작은 1부터

c(1, 2, 3, 4, "5")     # 여기서 "5"로 인해서 같은 타입으로

seq(from=1, to=10, by=2)   # 1부터 10까지 2씩 떨어진 숫자자
?seq
seq(1, 10, 2)      # seq(from=1, to=10, by=2) 와 표현이 같다

?rep
rep(1:4, 2)      # rep는 반복 ,  1부터 4까지 2번 반복 , 이렇게 나온 결과값도 배열이다

#### vector의 접근
v <- (1:50)     # 1부터 50까지 vector 로 만들고
v[10:45]      # 그 중 10부터 45까지 슬라이싱
length(v)

v1 <- c(13, -5, 20:23, -2:3)
v1[1]    # 13
v1[c(2, 4)]      # -5  21
v1[c(4, 5:8, 7)]       # 21 22 23 -2 -1 -2
v1[-1]   # 첫번째 자리 빼고 다 가져오기 ,  -5 20 21 22 23 -2 -1  0  1  2  3
v1[-2]   # 두번째 자리 빼고 다 가져오기 ,   13 20 21 22 23 -2 -1  0  1  2  3
v1[c(-2, -4)]    # 두번째와 네번째 자리 빼고 다 가져오기 ,  13 20 22 23 -2 -1  0  1  2  3
v1[-c(2, 4)]    # v1[c(-2, -4)] 와 동일한 표현
v1[length(v1)]    # R에서 끝자리를 표현하려면 이렇게 활용

#### 집합 연산

x <- c(1, 3, 5, 7)
y <- c(3, 5)

union(x, y); setdiff(x, y); intersect(x, y)

#### 컬럼명 지정
age <- c(30, 40, 45)
names(age) <- c("홍길동", "유비", "관우")
age


#### 특정 변수의 데이터 제거  ( 데이터에 NULL을 넣으면 기존 데이터 삭제 )
age <- NULL
age


#### 벡터 생성의 또 다른 표현
x <- c(2, 3)     # 이렇게 각각 지정할 때에는 c를 붙여줘야 함
x

x <- (2:5)      # 이렇게 범위를 지정할 때에는 c는 생략가능
x







#### factor ####
# 1. 범주형 데이터 타입

gender <- c("man", "woman", "woman", "man", "man")
gender
class(gender)
mode(gender)
is.factor(gender)      #factor (교재 326page)

ngender <- as.factor(gender)
ngender
is.factor(ngender)
class(ngender)        #class 함수는 해당 데이터가 어떤 클래스로 만들어져있는지 알려주는 함수
mode(ngender)        #mode 함수는 원초적인 데이터 형식
plot(ngender)       # R에서는 plot 이라는 기본함수를 통해 막대그래프로 표현할 수 있다

gfactor <-factor(gender, levels = c("woman", "man"))
gfactor
plot(gfactor)



#### matrix ####
#1. 행과 열의 2차원 배열
#2. 동일한 데이터 타입만 사용 가능
#3. matrix(), rbind(), cbind(), ...

matrix(c(1:5))       # 1차원 배열인 c(1:5) 를 matrix() 로 감싸면 2차원 배열로 만들어준다

matrix(c(1:5), nrow=2)       # nrow 로 행 갯수를 지정할 수 있다.

matrix(c(1:12), nrow=2) 

matrix(c(1:12), nrow=2, byrow=TRUE)      # 배열에 숫자가 열방향으로 채워지는 것이 아닌 행방향으로 채워지게 지정

matrix(c(1:12), nrow=2, byrow=T)    # TRUE 를 T 로 간략하게 사용할 수 있다

x1 <- c(3, 4, 50:52)
x2 <- c(30, 5, 6:8, 7, 8)
x1
x2

rbind(x1, x2)      # rbind()  - 행으로 합쳐주는 기능 (갯수가 안맞을땐 앞에서부터 반복)
cbind(x1, x2)      # cbind() - 열로 합쳐주는 기능

#### 차수 확인
x <- matrix(c(1:9), ncol=3)    # ncol 로 열 갯수를 지정할 수 있다.
x
length(x); nrow(x); ncol(x); dim(x);     # 전체길이, 행의 갯수, 열의 갯수, 몇행 몇열인지

#### 컬럼명 지정
colnames(x) <- c("one", "two", "three")
x

colnames(x)


#### apply
?apply
apply(x, 1, max)               # apply( 데이터, 각 행의 최대값, 최대값)   1은 각 행의 최대값을 의미
apply(x, 2, max)               # 2는 각 열의 최대값을 의미
apply(x, 1, sum)               # 1로 지정되있으니 각 행의 합
apply(x, 2, sum)               # 2로 지정되있으니 각 열의 합




#### data.frame ####
#1. DB의 테이블과 유사
#2. R에서 가장 많이 사용되는 구조
#3. 컬럼 단위로 서로 다른 데이터타입 사용 가능
#4. data.frame(), read.csv(), read.delim(), read.table(), ...

no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150.25, 250.18, 300.34)

data.frame(No=no, NAME=name, PAYMENT=pay)

#### read.csv(), read.delim(), read.table()      - 파일을 읽어와서 데이터프레임 형태로..
getwd()                # getwd() - 현재 내가 작업하고있는 폴더위치 읽어오기,  wd 는 working directory 약자

read.csv("../data/emp.csv")    # 상위폴더로 가서 data 폴더 속 emp.csv 파일을 읽어오기

setwd("../data")    # 내가 현재 경로를 자주 사용할 경우 지정해놓을 수 있다.
getwd()

read.csv("emp.csv")

read.table("emp.txt", header=T, sep=" ")
# 오류시 multibyte 문제면 인코딩 문제임 (ANSI 타입을 UTF-8로 변경하고 저장후 다시 실행)
# header=T  이 기능은 첫번째 줄이 제목이라는 것을 알려주는 기능, 없으면 v1, v2, v3 같이 지정됨
# sep=" "  뭐로 데이터가 구분되어있는지 설정, 단 여기서는 sep=" " 는 기본으로 설정되어있어 안써도 문제는 없다

read.table("emp.csv", header=T, sep=",")

#### 실습
aws = read.delim("AWS_sample.txt", sep="#")
head(aws)                              # head() 는 6개만 뽑아서 보여준다
View(aws)                              # View() 는 전체데이터를 형식에 맞춰 정렬해 보여준다.

aws[1, 1]          # 1행 1열 데이터 확인

x1 <- aws[1:3, 2:4]    # 행은 1행부터 3행까지,  열은 2열부터 4열까지
x1

x2 <- aws[9:11, 2:4]   # 행은 9행부터 11행까지, 열은 2열부터 4열까지
x2

cbind(x1, x2)    # cbind을 이용해 열로 합치기
rbind(x1, x2)    # rbind를 이용해 행으로 합치기

aws[,1]   # aws 에 담아놓은 데이터 중 1열에 있는 데이터를 전부 꺼내보기기
aws$AWS_ID    #  $를 붙여서 어떤 열의 데이터를 꺼내올건지 선택가능

# 구조 확인
str(aws)    # 총 데이터가 몇개고 형식이 무엇이고 구조를 알 수 있는 str()

# 기본 통계량
summary(aws)   

### 주요 함수들
# apply
df <- data.frame(x=c(1:5), y=seq(1, 10, 2), z=c("a", "b", "c", "d", "e"))
df
apply(df[,c(1, 2)], 1, sum)
apply(df[,c(1, 2)], 2, sum)

# subset

x1 <- subset(df, x>=3)
x1

x2 <- subset(df, x>=2 & y<=6)
x2


# 병합 : merge
height <- data.frame(id=c(1, 2), h=c(180, 175))
weight <- data.frame(id=c(1, 2), h=c(80, 75))
merge(height, weight, by.x="id", by.y="id")


#### array ####
#1. 행, 열, 면의 3차원 배열 형태의 객체 생성
#2. array()

(v <- c(1:12))

arr <- array(v, c(4, 2, 3))       # 4행 2열 3면으로 만들어진 3차원 데이터

arr[,,1]    # 행, 열은 비워져있고 면이 1 이니 첫번째 면에 해당하는 값들에 접근
arr[,,2]    # 두번째 면에 접근

arr[2, 1, 2]     # 두번째 면의 2행 1열에 위치한 데이터인 10을 가져옴
arr[,,2][2, 1]    # 두번째 면에 접근해서 2행 1열에 위치한 데이터인 10을 가져옴





#### list ####
#1. key와 value
#2. python의 dict와 유사
#3. list()

x1 <- 1
x2 <- data.frame(var1=c(1, 2, 3), var2=c('a', 'b', 'c'))
x3 <- matrix(c(1:12), ncol=2)
x4 <- array(1:20, dim=c(2, 5, 2))

x5 <- list(c1=x1, c2=x2, c3=x3, c4=x4)
x5

x5$c2     # $ 기호를 이용해 c2 로 접근
x5$c1     # $ 기호를 이용해 c1 로 접근


list1 <- list(c("lee", "kim"), "유비", 95)
list1

list1[1]     # "lee" "kim"
list1[[1]]   # "lee" "kim"
list1[[1]][1]   # "lee"
list1[[1]][2]   # "kim"


list2 = list("lee", "이순신", 95)
list2

un = unlist(list2)    # unlist()는 list로 묶인 데이터를 vector 형태로 풀어준다
un     # list는 풀어서 사용하는 것 권장, 기억


### apply(), lapply(), sapply()
# lapply : apply는 2차원 이상의 데이터를 입력받기 때문에 vector를 입력받기 위한 방법으로 사용, 리턴값이 list형이다.

(a1 <- c(1:5))

apply(a1, max)    # 일반 apply는 2번째 인자인 행,열 구분을 안해주면 사용불가 (vector는 사용불가)

lapply(a1, max)    # lapply 는 사용가능, 결과는 list로 묶어서 출력됨 - lapply 의 l 이 list를 의미


a2 <- list(c(1:5))
a3 <- list(c(6:10))
a4 <- c(a2, a3)
a4

apply(a4, max)    # apply는 vector형을 받아 사용불가능

x <- lapply(a4, max)
(x1 <- unlist(x))


sapply(a4, max)    # sapply는 unlist로 자동으로 풀어서 결과를 돌려줌
