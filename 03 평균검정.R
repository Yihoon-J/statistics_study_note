####################
## 제3장 평균검정 ##
####################

###############
## 3.1 t검정 ##
###############

#t값 계산
t <- (135-115)/(25/sqrt(20))
t

#pt: 특정 t값에 대응되는 누적확률(p-value) 계산
pt(3.58, df=19, lower.tail=FALSE)*2 #t값, 자유도, 꼬리쪽 계산 여부. 양측검정이므로 2배 #0.05보다 낮으므로 귀무가설 기각
(1 - pt(3.58, df=19))*2

#qt: 특정 확률에 대응하는 t값 반환
qt(0.025, df=19, lower.tail=FALSE) #좌측끝단
qt(0.975, df=19)#우측끝단

135-2.09*(25/sqrt(20))
135+2.09*(25/sqrt(20))
 
qt(0.005, df=19, lower.tail=FALSE)

#########################
## 3.2 일표본 평균검정 ##
#########################

library(MASS)
str(cats)
#ex. 고양이의 몸무게 평균이 2.6kg이 맞는가?
## 귀무가설=맞다, 대립가설=아니다
t.test(x=cats$Bwt, mu=2.6) #검정할 데이터, 검정할 값 순. p value<0.05이므로 귀무가설을 기각한다.
t.test(cats$Bwt, mu=2.7) #2.7로 바꾸면 기각하지 못함

?t.test #도움말
t.test(cats$Bwt, mu=2.6, alternative="greater") #'2.6kg보다 크다'를 검정: 귀무가설을 기각

cats.t <- t.test(cats$Bwt, mu=2.6) #평균검정 결과는 리스트 형태로 반환됨
str(cats.t)

cats.t$p.value
cats.t$conf.int

t.test(cats$Bwt, mu=2.6, conf.level=0.99) #신뢰수준을 99%로 변경. 95% as default

#하나의 집단에 대해 통계적 비율이 특정 값과 같은지 검정
#ex. 18/30경기 이긴 팀: 모집단 비율(팀 승률)이 0.5보다 큰가?
prop.test(x=18, n=30, p=0.5, alternative="greater")

###########################
## 3.3 독립표본 평균검정 ##
###########################

#독립표본 평균검정: 고양이의 성별로 몸무게에 차이가 있는가?
#귀무가설: 차이가 없다, 대립가설: 차이가 있다
t.test(formula=Bwt ~ Sex, data=cats) #방법 1: formula=종속변수~독립변수, data=데이터

#방법 2: 두 개의 독립표본을 추출하여 t test 수행
Bwt.f <- cats$Bwt[cats$Sex=="F"]
Bwt.m <- cats$Bwt[cats$Sex=="M"]
t.test(Bwt.f, Bwt.m)

# [그림 3-6]  독립표본검정 시각화
bars <- tapply(cats$Bwt, cats$Sex, mean)
lower <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[1])
upper <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[2])


#install.packages("gplots")
#library(gplots)
barplot2(bars, space=0.4, ylim=c(0, 3.0),
         plot.ci=TRUE, ci.l=lower, ci.u=upper, ci.color="maroon", ci.lwd=4, 
         names.arg=c("Female", "Male"), col=c("coral", "darkkhaki"),
         xlab="Cats", ylab="Body Weight (kg)", 
         main="Body Weight by Sex\nwith Confidence Interval")


#독립표본검정: 환자 수 대비 흡연자의 비율의 동일 여부검정
smokers  <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
prop.test(x=smokers, n=patients) #모집단에서 발생한 네 개의 표본비율이 동일한가?
#귀무가설: 동일하다, 대립가설: 동일하지 않다

###########################
## 3.4 대응표본 평균검정 ##
###########################

str(sleep)
sleep[seq(1, 20, 2), ]

#대응표본 평균검정: 수면제 group별로 extra에 차이가 얼마나 되는지 검정
t.test(extra ~ group, data=sleep, paired=TRUE)

#long format - wide format 데이터 변형
install.packages("tidyr")
library(tidyr)
sleep.wide <- spread(sleep, key=group, value=extra) #tidyr의 spread 사용
sleep.wide

install.packages("reshape2")
library(reshape2)
sleep.wide <- dcast(sleep, ID ~ group, value.var="extra") #reshape2의 dcast 사용
sleep.wide

t.test(sleep.wide$'1', sleep.wide$'2', paired=TRUE) #wide format으로 t test 수행