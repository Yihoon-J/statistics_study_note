##########################
## 범주형 변수 요약 ##
##########################

library(MASS)
str(survey) #데이터의 자료형 파악. Factor로 나타난 컬럼은 자료형 데이터
levels(survey$Smoke) #범주형 데이터의 레벨을 파악
frqtab<-table(survey$Smoke) # 데이터 분포를 파악
frqtab
frqtab[2]#두번째 값 출력
frqtab[frqtab==max(frqtab)]#최빈값 출력
names(frqtab[frqtab==max(frqtab)])#이름만 출력
which.max(frqtab)#이름 출력(2)
prop.table(frqtab)#빈도 계산

#담배를 피우지 않는 사람의 비율 구하기
mean(survey$Smoke=="Never", na.rm=TRUE) #na가 포함되어 있을 경우 NA가 반환되므로 옵션 지정.

#치료 후 몸무게가 늘어난 사람의 비율
mean(anorexia$Postwt>anorexia$Prewt)

#증시 상승한 날의 비율
mean(diff(SP500)>0)

#교차표 생성
library(vcd)
#방법 1: crosstab. 행 - 열 순
crosstab<-table(Arthritis$Improved, Arthritis$Treatment) 
crosstab
crosstab['Marked','Treated'] #인덱싱
##방법 2: xtabs
crosstab<-xtabs(~Improved+Treatment, data=Arthritis)
crosstab

#행 또는 열의 합 계산하기
margin.table(crosstab, margin=1) #row for 1, col for 2

#비율 계산하기
prop.table(crosstab, margin=1)

#빈도합을 교차표에 추가하기
addmargins(crosstab, margin=2) #margin을 지정하지 않으면 행 열 전체에 대한 sum을 표시함
##addmargins, proptalbe 함께 사용
addmargins(prop.table(crosstab, margin=1),2)


#gmodels: 보다 많은 정보를 포함한 crosstable 생성
library(gmodels)
CrossTable(Arthritis$Improved,Arthritis$Treatment,prop.chisq=FALSE, dnn=c("Improved","Tratment")) #chisq 표시 않음, 행-열 이름 표시

#3개 이상의 컬럼을 사용한 다차원 crosstable
multtab<-table(Arthritis$Improved, Arthritis$Sex, Arthritis$Treatment)
multtab<-xtabs(~Improved+Sex+Treatment,data=Arthritis)

#더 깔끔하게...
ftable(multtab, row.vars=c(2,3)) #행으로 지정될 변수를 지정

#성별로 치료 교화가 나타난 비율
ftable(prop.table(multtab,c(2,3))) #명시하지 않은 Treatment 기준 집계
ftable(addmargins(prop.table(multtab,c(2,3)),1)) #합계 표시 추가




##########################
## 연속형 변수 요약 ##
##########################

library(MASS)
median(survey$Pulse) #na 포함되어 NA를 반환함
median(survey$Pulse, na.rm=TRUE)

quantile(survey$Pulse, probs=0.05, na.rm=TRUE) #백분위수 구하기

quantile(survey$Pulse, 0.5, na.rm=TRUE) #중위값

quantile(survey$Pulse, c(0.05, 0.95), na.rm=TRUE) #두 개 이상의 백분위 출력

quantile(survey$Pulse, na.rm=TRUE) #probs 지정하지 않으면 0, 25, 50, 75, 100번째 사분위값이 도출됨

mean(survey$Pulse <= 80, na.rm=TRUE) #특정 값을 기준으로 백분위수(비율) 구하기

mean(survey$Pulse, na.rm=TRUE) #평균
median(survey$Pulse, na.rm=TRUE) #중위값

str(iris)

summary(iris$Sepal.Width)#연속형 summary: 최소값, 사분위값, 평균, 최대값 반환

summary(iris$Species) )) #범주형 summary: 빈도표 반환

summary(as.character(iris$Species)) #문자형 summary: 길이 반환

summary(iris) #datframe summary: 열 단위 통계 출력

iris.lst <- as.list(iris) #리스트 데이터 summary: 원소의 크기 및 형식 반환
summary(iris.lst)

lapply(iris.lst, summary) #리스트 함수에서 통계량 확인: lapply

range(survey$Pulse, na.rm=TRUE) #범위 출력

var(survey$Pulse, na.rm=TRUE) #분산
sd(survey$Pulse, na.rm=TRUE) #표준편차

str(mtcars)

install.packages("pastecs") #pastecs: 기술통계 패키지
library(pastecs)
stat.desc(mtcars[c("mpg", "hp", "wt")]) #특정 컬럼에 대한 통계량 산출

install.packages("psych") #pysch: 기술통계 패키지
library(psych)
describe(mtcars[c("mpg", "hp", "wt")]) #통계량 산출

#Exer 기준으로 맥박 평균값 집계
# 방법1: tapply 사용
tapply(survey$Pulse, INDEX=survey$Exer, FUN=mean, na.rm=TRUE) #집계할 대상, 집계 기준, 통계량
tapply(survey$Pulse, survey$Sex, mean, na.rm=TRUE) #cf. 성별 집계
tapply(survey$Pulse, list(survey$Exer, survey$Sex), mean, na.rm=TRUE) #두 개의 집계 기준 사용

# 방법 2: aggregate 사용
aggregate(survey$Pulse, by=list(Exercise=survey$Exer), FUN=mean, na.rm=TRUE)
aggregate(survey$Pulse, list(Exercise=survey$Exer, Sex=survey$Sex), 
          mean, na.rm=TRUE) # 두 개의 집계 기준 사용

aggregate(survey[c("Pulse", "Age")], 
          list(Exercise=survey$Exer), mean, na.rm=TRUE)

#커스텀 함수로 통계량 출력하기
myStats <- function(x, na.rm=FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)
  skew <- sum((x-mean)^3/sd^3)/n
  kurt <- sum((x-mean)^4/sd^4)/n - 3
  return(c(n=n, mean=mean, sd=sd, skewness=skew, kurtosis=kurt))
}
aggregate(survey[c("Pulse", "Age")], 
          list(Exercise=survey$Exer), myStats, na.rm=TRUE)

#방법 3: by사용
by(survey[c("Pulse", "Age")], INDICES=list(Exercise=survey$Exer), FUN=summary) #집계할 대상, 집계 기준, 확인할 통계량
aggregate(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), summary)

by(survey[c("Pulse", "Age")], list(Exercise=survey$Exer), 
   function(x) sapply(x, myStats, na.rm=TRUE)) #사용자 함수 사용 시 sapply와 결합하여 사용해야 함

library(psych)
describeBy(survey[c("Pulse", "Age")], group=list(Exercise=survey$Exer)) #지정한 기준에 따라 통계량을 그룹별 집계
describeBy(survey[c("Pulse", "Age")], group=list(Exercise=survey$Exer), summary) #기본 함수 외에 사용자 지정 함수 사용 불가
