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
shapiro.test(resid(out))  # p-value = 1.024e-11 - 정규붆포가 아니다.  3개 중 하나라도 정규분포가 아니면 아니다.

# 등분산 여부  ( 위에서 결과가 정규분포가 아니지만 정규분포라고 가정하고 등분산 여부를 확인해보자 )
bartlett.test(LDLC ~ Dx, data=acs)    # p-value = 0.1857 로 등분산 이다.

### 최종 결론 : anova 사용 (위에서 정규분포 확인하면서 aov 함수를 사용했으므로 여기선 생략 - 주석처리)
# out = aov(LDLC ~ Dx, data=acs)
summary(out)     # Pr(>F) 0.00377 로 p.value 가 0.05보다 작으므로 차이가 있다 - 대립가설 (** - 별이 많을수록 차이가 크다)

### 최종 결론 : Kruskal-wallis 사용
kruskal.test(LDLC ~ Dx, data=acs)   # p-value = 0.004669 로 차이가 있다 - 대립가설

### 최종 결론 : welch's anova 사용
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)   # p-value = 0.007471 로 차이가 있다 - 대립가설
