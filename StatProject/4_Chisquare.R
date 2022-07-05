##### 실습1 #####
### 주제 : 자동차의 실린더 수와 변속기의 관계가 있는지 알고 싶다.

str(mtcars)
result <- table(mtcars$cyl, mtcars$am)

chisq.test(result)   # p-value = 0.01265 로 관계가 있으나 데이터가 충분하지 않아 정확하지 않을수도 있다.

fisher.test(result) # 정확하지 않으니 fisher.test 로 다시 확인 = p-value = 0.009105 로 관계가 있다.

barplot(result)
