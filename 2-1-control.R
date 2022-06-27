##### 조건문 #####

a <- scan()                 # python 에서 input 와 같은 입력받을 수 있게끔 도와주는 함수
a

b <- scan(what=character())
b

df <- data.frame()
df <- edit(df)
df


a <- scan()
if(a < 10){
  print("10보다 작다")
} else {
  print("10보다 크다")
}

if(a >= 10){
  print("A")
} else if(a >= 9){
  print("B")
} else if(a >= 8){
  print("C")
}


### switch(), which(), ifelse(), case_when()