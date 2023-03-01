#######################
## 제11장 시계열분석 ##
#######################

########################
## 11.1 시계열 데이터 ##
########################

#######TS########

url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url)
utility

utility.ts <- ts(data=utility[[7]], start=c(1990, 9), frequency=12)
utility.ts
class(utility.ts)

# [그림 11-1]
plot(utility.ts)
plot(utility.ts, col="salmon", lwd=2, xlab="Year", ylab="Electricity Usage")


start(utility.ts) #시작값
end(utility.ts) #끝값

frequency(utility.ts) #데이터 단위 (12개월)
deltat(utility.ts) #간격 (1/12년)

time(utility.ts) #시간 축 출력 (0~1)

cycle(utility.ts) #관측값의 주기 내 위치(1~12)

window(utility.ts, start=c(1991, 1), end=c(1992, 6)) #시작-종료 subset 정의

window(utility.ts, start=c(1991, 1), frequency=1) #주기까지 지정: 91년 1월부터 1년 간격
window(utility.ts, start=c(1991, 7), frequency=1) # 91년 7월부터 1년 간격

window(utility.ts, start=c(1990, 9), frequency=2) # 90년 9월부터 6개월 간격

window(utility.ts, start=c(1991, 1), frequency=4, end=c(1996,1))


#######XTS#######
date <- c("2030-01-02", "2030-01-03", "2030-01-04", "2030-01-05", 
          "2030-01-06", "2030-01-07", "2030-01-08", "2030-01-09")
time <- c("09:00:01", "10:05:13", "11:10:30", "12:00:01",
          "13:15:12", "14:20:01", "15:11:01", "16:20:03")
price <- c(200, 300, 420, 380, 490, 550, 600, 650)

datetime<-paste(date, time)

tsdata<-data.frame(datetime=datetime, price=price)
price.xts<-xts(x=tsdata$price, order.by=as.POSIXct(tsdata$datetime, format='%Y-%m-%d %H:%M:%S'))
class(price.xts)

#동일 데이터로 데이터 생성
library(xts)
url <- "http://jse.amstat.org/datasets/utility.dat.txt"
utility <- read.table(url)
library(lubridate)#lubridate로 날짜 포맷이 변경 가능
utility.xts<-xts(x=utility[c(3,7)],order.by=my(utility[[1]]))
names(utility.xts)<-c("Temperature","Electricity")
class(utility.xts)
utility.xts

plot(utility.xts)
addLegend('topleft',lwd=3, legend.names=(c("Temp(F)", "Elec(W)")))

tformat(utility.xts)<-"%Y-%m-%d" #시간 자료형 변경

# 시계열 데이터 시각화
plot(utility.xts, format.lables="%b-%Y", lty=c("dotted", "solid"), col=c("red","blue"),lwd=3, main="utility consumption")
addLegend('topleft',lty=c("dotted", "solid"), col=c("red","blue"),lwd=3, legend.names=(c("Temp(F)", "Elec(W)")))

# 데이터 추출 시 []사용, 기간 추출 시 / 인자 사용
utility.xts["1991-01"]
utility.xts["1997-01/1997-09"]
utility.xts["1994-02", "Electricity"]

#끝점 도출
ep<-endpoints(utility.xts, on="years")
ep

#그룹별 통계량 도출
period.apply(utility.xts, INDEX=ep, FUN=mean) #구분점 직접 지정
apply.monthly(utility.xts, FUN=mean) #연별, 월별, 분기별 등등...

roll1<-rollapply(utility.xts["1990-9/1991-8", "Electricity"], width=2, FUN=mean) # 커스텀 구간별 집계. width = 집계 간격(2개월)
roll1
roll2<-rollapply(utility.xts["1990-9/1991-8", "Electricity"], width=3, FUN=sum) # 3개월의 합을 매달 출력
roll2

# ts - xts 변환 작업
as.ts(utility.xts, start=c(1990, 9))
as.xts(utility.ts, start=c(1990,9))



#############################
## 11.2 시계열 데이터 분해 ##
#############################

nhtemp
frequency(nhtemp)
plot(nhtemp, col='dimgray',lwd=2, ylab='Temperature', main='Base Time Series')

install.packages("forecast")
library(forecast)
ma(nhtemp,3) # 이동평균 계산

# [그림 11-2] 이동평균 윈도우별 시각화
old.par <- par(mfrow=c(2,2))
ylim <- c(min(nhtemp), max(nhtemp))
plot(ma(nhtemp, 3), ylim=ylim, col="red", lwd=2, 
     main="Simple Moving Average (k=3)", ylab="Temperature")
plot(ma(nhtemp, 7), ylim=ylim, col="green3", lwd=2,
     main="Simple Moving Average (k=7)", ylab="Temperature")
plot(ma(nhtemp, 11), ylim=ylim, col="blue", lwd=2,
     main="Simple Moving Average (k=11)", ylab="Temperature")
plot(ma(nhtemp, 30), ylim=ylim, col="darkorange", lwd=2,
     main="Simple Moving Average (k=30)", ylab="Temperature")
par(old.par)

# [그림 11-3] 가법모델 vs 승법모델
old.par <- par(mfrow=c(2,1))
plot(window(co2, start=c(1985, 1), end=c(1996, 12)), col="salmon", lwd=2,
     main="Additive Trend, Seasonal, Irregular Components", 
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")
plot(AirPassengers, col="cornflowerblue", lwd=2,
     main="Multiplicative Trend, Seasonal, Irregular Components",
     xlab="Year", ylab="Air Passengers (Thousand Persons)")
par(old.par)

#co2 데이터 시계열분해
data(co2)
co2 <- window(co2, start=c(1985, 1), end=c(1996, 12))
co2
co2.decomp <- stl(co2, s.window="periodic")
co2.decomp

# [그림 11-4] 시계열분해 결과 시각화
plot(co2.decomp, col="darkcyan", col.range="skyblue", lwd=2,
     main="Decomposition of CO2 Concentration Time Series")

co2
co2.decomp$time.series #시계열분해값 저장: 세 성분을 더하면 관측값이 나옴


co2.adj <- co2 - co2.decomp$time.series[, "seasonal"] #계절 성분을 제외한 deseasonal data 출력
co2.adj2<-co2 - co2.decomp$time.series[, "trend"]
co2.adj3<-co2 - co2.decomp$time.series[, "remainder"]


# [그림 11-5] deseasonal data 시각화
old.par <- par(mfrow=c(3,1))
plot(co2.adj, col="tomato", lwd=2,
     main="CO2 Concentration Time Series without Seasonal Effect",
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")
plot(co2.adj2, col="skyblue", lwd=2,
     main="CO2 Concentration Time Series without Trend Effect",
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")
plot(co2.adj3, col="orchid", lwd=2,
     main="CO2 Concentration Time Series without Remainder",
     xlab="Year", ylab="CO2 Concentration (Parts Per Million)")


# [그림 11-6] 계절효과 상세 시각화 - monthplot, seasonplot
library(forecast)

monthplot(co2, col="slateblue", lwd=2, main="Month Plot", 
          xlab="Month", ylab="CO2 Concentration (Parts Per Million)")
seasonplot(co2, col="sienna", lwd=2, year.labels=TRUE, main="Season Plot", 
           ylab="CO2 Concentration (Parts Per Million)")


AirPassengers

# [그림 11-7] airpassengers 데이터 시각화
plot(AirPassengers, col="maroon", lwd=2,
     main="Air Passengers",
     xlab="Year", ylab="Air Passengers (Thousands)")

# 로그변환 후 시계열분해 (승법모델)
lair <- log(AirPassengers)
plot(lair, col="navy", lwd=2,
     main="Log Transformation of Air Passengers",
     xlab="Year", ylab="Air Passengers (Log(Thousands))") #로그변환: 변동성 완화
par(old.par)

lair.decomp <- stl(lair, s.window="periodic") #시계열분해 수행

# [그림 11-8] 시계열분해 후 시각화
plot(lair.decomp, col="chocolate", col.range="orange", lwd=2,
     main="Decomposition of Log Transformed Air Passengers")

lair.decomp$time.series
exp(lair.decomp$time.series) # 분해값에 지수함수 적용하여 season trend의 변동값 확인 가능함

#######################
## 11.3 지수예측모델 ##
#######################

## 단순지수평활법(simple exponential smoothing)

LakeHuron

plot(LakeHuron, col="royalblue", lwd=2, main="Annual Level of Lake Huron")
mean(LakeHuron)

library(forecast)
lake.ets <- ets(LakeHuron, model="ANN") #불규칙모델만 사용하는 단순평활법
lake.ets

#1년 후 값 예측
lake.ets.pred <- forecast(lake.ets, h=1)
lake.ets.pred

# [그림 11-10]
plot(lake.ets.pred, col="royalblue", lwd=2,
     main="Forecast for Annual Level of Lake Huron", 
     xlab="Year", ylab="Level (Feet)")

accuracy(lake.ets) #예측모델의 성능. ACF1은 0에 가까워야 함


## 홀트지수평활법(Holt exponential smoothing)

install.packages("fpp")
library(fpp)
elecsales
plot(elecsales, col="royalblue", lwd=2,  
     main="Forecast for Electricity Sales in South Australia",
     xlab="Year", ylab="Electricity Sales (GWh)")

library(forecast)
elecsales.ets <- ets(elecsales, model="AAN")
elecsales.ets
accuracy(elecsales.ets)

elecsales.ets.pred <- forecast(elecsales.ets, h=5)
elecsales.ets.pred

# [그림 11-11]
plot(elecsales.ets.pred, col="royalblue", lwd=2,
     flty=3, flwd=3, shadecols=c("lavender", "mistyrose"),  
     main="Forecast for Electricity Sales in South Australia",
     xlab="Year", ylab="Electricity Sales (GWh)")



## 홀트윈터스(Holt-Winters exponential smoothing)
library(forecast)
plot(AirPassengers, col="salmon", lwd=2,
     main="Forecast for Air Passengers", 
     xlab="Year", ylab="Air Passengers (Log(Thousand Persons))")

lair.ets <- ets(log(AirPassengers), model="AAA")
lair.ets
accuracy(lair.ets)

lair.ets.pred <- forecast(lair.ets, h=12)
lair.ets.pred

# [그림 11-12]
plot(lair.ets.pred, col="salmon", lwd=2, fcol="indianred1", flwd=3,
     main="Forecast for Air Passengers", 
     xlab="Year", ylab="Air Passengers (Log(Thousand Persons))")

air.mean <- exp(lair.ets.pred$mean) #예측값 복원
air.lower <- exp(lair.ets.pred$lower) #예측값 하한 복원
air.upper <- exp(lair.ets.pred$upper) #예측값 상한 복원
air.pred <- cbind(air.mean, air.lower, air.upper)
air.pred

## 완화추세(damped trend)와 모델자동선택

library(fpp)
austourists

austourists.ets <- ets(austourists)
austourists.ets

# [그림 11-13] 
plot(forecast(austourists.ets, h=12), col="cornflowerblue", lwd=2,
     flty=1, flwd=3, fcol="royalblue", shadecols=c("mistyrose", "salmon"),
     main="Forecast for International Tourists to Australia",
     xlab="Year", ylab="Total Visitor Nights")

########################
## 11.4 ARIMA예측모델 ##
########################

## 정상성과 자기상관(stationarity and autocorrelation)

# [그림 11-14]
old.par <- par(mfrow=c(2,2))
plot(AirPassengers, col="red", lwd=2,
     main="(a) Air Passengers", xlab="Year", ylab="Persons (1,000)")
install.packages("fpp2")
library(fpp2)
plot(goog200, col="blue", lwd=2,
     main="(b) Google Stock Prices", xlab="Day", ylab="Dollars")
plot(Nile, col="green3", lwd=2,
     main="(c) Flow of the River Nile", xlab="Year", ylab="Flow")
plot(nottem, col="mediumorchid4", lwd=2,
     main="(d) Temperatures at Nottingham", xlab="Year", ylab="Fahrenheit")
par(old.par)

Nile

lag(Nile, 0)
lag(Nile, 1)
lag(Nile, 2)

library(fpp2)
library(forecast)
head(goog200)
ndiffs(goog200)#적합한 차분 수 결정
dgoog200 <- diff(goog200)
head(dgoog200)

plot(goog200, col="cornflowerblue", lwd=2,
     main="(a) Google Stock Prices", xlab="Day", ylab="Dollars")

# [그림 11-16]
old.par <- par(mfrow=c(2,2))
plot(goog200, col="cornflowerblue", lwd=2,
     main="(a) Google Stock Prices", xlab="Day", ylab="Dollars")
plot(dgoog200, col="salmon", lwd=2,
     main="(b) Google Stock Prices\nTransformed by Differencing", 
     xlab="Day", ylab="Dollars")
Acf(goog200, lwd=2, main="Original Data")
Acf(dgoog200, lwd=2, main="Differenced Data")
par(old.par)

library(tseries)
adf.test(goog200)
adf.test(dgoog200)

# [그림 11-17]
windows(width=7.0, height=5.5)
old.par <- par(mfrow=c(2,2))
plot(AirPassengers, col="darkolivegreen4", lwd=2, main="(a) Non-stationary Time Series")
plot(log(AirPassengers), col="firebrick", lwd=2, main="(b) Constant Variance")
plot(diff(AirPassengers), col="darksalmon", lwd=2, main="(c) Constant Mean")
plot(diff(log(AirPassengers)), col="goldenrod", lwd=2, main="(d) Stationary Time Series")
par(old.par)

## ARMA모델과 ARIMA모델

#1. 정상성 평가
library(tseries)
adf.test(Nile) #adf검정 수행 - 비정상시계열임.

library(forecast)
ndiffs(Nile) #1회 차분이 필요

dNile <- diff(Nile) #1회 차분 수행
adf.test(dNile)

# [그림 11-18]원본 및 차분한 데이터 시각화
old.par <- par(mfrow=c(2,1))
plot(Nile, col="darkviolet", lwd=2, main="Flow of the River Nile: Original",
     xlab="Year", ylab="Flow")
plot(dNile, col="dodgerblue", lwd=2, main="Flow of the River Nile: Differenced",
     xlab="Year", ylab="Differenced Flow")
par(old.par)

# [그림 11-19] ACF, PACF 시각화
old.par <- par(mfrow=c(2,1))
Acf(dNile, lwd=2, main="Autocorrelation for the River Nile")
Pacf(dNile, lwd=2, main="Partial Autocorrelation for the River Nile")
par(old.par)

Acf(dNile, plot=FALSE)
Pacf(dNile, plot=FALSE)

#모델 생성
Nile.arima <- arima(Nile, order=c(0, 1, 1))
Nile.arima
accuracy(Nile.arima) #평가

#qqplot 시각화
qqnorm(Nile.arima$residuals, pch=21, col="black", bg="gold", 
       main="Q-Q Plot of Residuals")

# [그림 11-21]
old.par <- par(mfrow=c(1,2))
hist(Nile.arima$residuals, col="mistyrose", prob=TRUE, 
     main="Histogram of Residuals", xlab="Residuals")
xfit <- seq(min(Nile.arima$residuals), max(Nile.arima$residuals), length.out=40)
yfit <- dnorm(xfit, mean=mean(Nile.arima$residuals), sd=sd(Nile.arima$residuals))
lines(xfit, yfit, col="tomato", lwd=2)
qqnorm(Nile.arima$residuals, pch=21, col="black", bg="gold", 
       main="Q-Q Plot of Residuals")
qqline(Nile.arima$residuals, col="royalblue", lwd=2)
par(old.par)

Box.test(Nile.arima$residuals, type="Ljung-Box")

Nile.arima.pred <- forecast(Nile.arima, h=5)
Nile.arima.pred

# [그림 11-22]
plot(Nile.arima.pred, col="darkgreen", lwd=2, flty=1, flwd=3,
     fcol="royalblue", shadecols=c("mistyrose", "salmon"),
     main="Forecast for Flow of the River Nile",
     xlab="Year", ylab="Flow")

## 계절 성분(seasonal factor)과 모델자동선택

library(forecast)
gas

gas.arima <- auto.arima(gas) #예측모델 자동생성
gas.arima
accuracy(gas.arima)

arima(gas, order=c(2, 1, 1), seasonal=list(order=c(0, 1, 1), period=12)) #수동으로 명시

forecast(gas.arima, h=5*12)

# [그림 11-23]
plot(forecast(gas.arima, h=5*12), col="darkorange", lwd=2,
     flty=1, flwd=3, fcol="orangered", shadecols=c("lavender", "skyblue"),
     main="Australian Monthly Gas Production",
     xlab="Year", ylab="Monthly Production")
