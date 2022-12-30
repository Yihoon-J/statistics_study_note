##################
## 2.2 확률분포 ##
##################

# 이항분포
# 확률밀도함수, 확률분포함수
dbinom(7, size=10, prob=0.5) #이항분포 확률밀도함수: 동전을 열  번 던져 앞면이 7번 나올 확률
pbinom(7, size=10, prob=0.5) #이항분포 확률분포함수: 동전을 열  번 던져 앞면이 7번 이하 나올 확률
sum(dbinom(0:7, size=10, prob=0.5)) #dbinorm으로 따로 집계해서 합한 것과 동일하다.

pbinom(7, size=10, prob=0.5, lower.tail=FALSE) #이항분포 확률분포함수(2): 동전을 열 번 던져 앞면이 8번 이상 나올 확률
pbinom(7, size=10, prob=0.5) - pbinom(3, size=10, prob=0.5) #이항분포 확률분포함수(3): 동전을 열 번 던져 앞면이 4~7번 나올 확률
diff(pbinom(c(3, 7), size=10, prob=0.5)) # 두 개 확률분포함수 구해서 그 차이를 구하는 방식으로 수행해도 동일함.

#이항분포 난수생성
set.seed(1) #set seed
rbinom(1, size=10, prob=0.5) 
rbinom(5, size=10, prob=0.5) #동전을 열 번 던졌을 때 앞면이 나타나는 시행



#정규분포
pnorm(110, mean=100, sd=15) #mean=100, sd=15이하에서 110 이하의 확률
pnorm(110, mean=100, sd=15, lower.tail=FALSE) #110 초과 확률

pnorm(0) #mean, sd 생략하면 표준정규분포를 상정
pnorm(0, mean=0, sd=1) 

dnorm(110, mean=100, sd=15)

pnorm(110, mean=100, sd=15) - pnorm(90, mean=100, sd=15) #90~110사이 확률 출력
diff(pnorm(c(90, 110), mean=100, sd=15)) #동일

#정규분포 백분위수
qnorm(0.05, mean=100, sd=15) #(100,15)에서 하위 5%에 대응하는 관측값
qnorm(0.95, mean=100, sd=15) #동일 분포에서 95%(상위 5%)에 대응하는 관측값
qnorm(c(0.05, 0.95), mean=100, sd=15) #벡터로 같이 표시

#마찬가지로, mean sd 생략하면 표준정규분포 상정
qnorm(0.025)
qnorm(0.975)
qnorm(c(0.025, 0.975))

#정규분포 난수생성
rnorm(1, mean=100, sd=15)
rnorm(5, mean=100, sd=15)
rnorm(1)
rnorm(5)
rnorm(3, mean=c(-10, 0, 10), sd=1)
rnorm(6, mean=c(-10, 0, 10), sd=1)

?Binomial
?Normal
?TDist
?FDist
?Chisquare
?Uniform

#shapiro-wilk 정규성 검정: 자료의 정규성 검정
set.seed(123)
shapiro.test(rnorm(100, mean=100, sd=15)) #정규분포
shapiro.test(runif(100, min=2, max=4)) #일양분포

#정규q-q도표: 정렬된 두 분포를 쌍지어 시각화
# 대각선에 가까울수록 정규분포를 충족한다.
set.seed(123)
qqnorm(rnorm(100, mean=100, sd=15), col="blue", 
       main="Sample from Normal Distribution")
qqline(rnorm(100, mean=100, sd=15)) #정규분포를 따르는 직선을 참조선으로 추가

#cf. 일양분포 q-q도표: 상대적으로 직선에서 많이 벗어남
qqnorm(runif(100, min=2, max=4), col="red", 
       main="Sample from Uniform Distribution")
qqline(runif(100, min=2, max=4))
par(old.par)