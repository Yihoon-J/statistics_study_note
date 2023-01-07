####################
## 제4장 분산분석 ##
####################

###############
## 4.2 F검정 ##
###############

mstr <- ((101.6-108.1)^2*5+(114.6-108.1)^2*5)/(2-1)

mse <- (4.98^2*4+7.96^2*4)/(4+4)
mse <- ((95-101.6)^2+(105-101.6)^2+(98-101.6)^2+(103-101.6)^2+(107-101.6)^2+(110-114.6)^2+(125-114.6)^2+(105-114.6)^2+(113-114.6)^2+(120-114.6)^2)/(4+4)
F <- mstr/mse
F

pf(F, df1=1, df=8, lower.tail=FALSE) 

pf(9.59, df1=1, df=8, lower.tail=FALSE) #집단 간 분산 df1, 집단 내 분석 df2일 때 f값 이상이 발생할 확률
qf(0.05, df1=1, df=8, lower.tail=FALSE) # 유의수준 5%에서 대응되는 f값

adhd <- data.frame(score=c(95,105,98,103,107,110,125,105,113,120),
                   therapy=c(rep("A", 5), rep("B", 5)))


######################
## 4.3 일원분산분석 ##
######################

#실습: InsectSprays데이터 사용
str(InsectSprays)
InsectSprays

#살충제 종류별 통계값 확인
tapply(InsectSprays$count, InsectSprays$spray, mean) #평균
tapply(InsectSprays$count, InsectSprays$spray, sd) #표준편차
tapply(InsectSprays$count, InsectSprays$spray, length) #개수

# 데이터 시각화1: plotmeans (평균, 신뢰구간)
library(gplots)
plotmeans(count ~ spray, data=InsectSprays,
          barcol="tomato", barwidth=3, col="cornflowerblue", lwd=2,
          xlab="Type of Sprays", ylab="Insect Count", 
          main="Performance of Insect Sprays\nwith 95% CI of Mean")

# 데이터 시각화2: boxplot
boxplot(count ~ spray, data=InsectSprays, col="tomato",
        xlab="Type of Sprays", ylab="Insect Count",
        main="Performance of Insect Sprays")

#일원분산분석 수행
sprays.aov <- aov(count ~ spray, data=InsectSprays)
sprays.aov

summary(sprays.aov)

#model.tables: 집단별 평균 비교
model.tables(sprays.aov, type="means") #집단별 평균
model.tables(sprays.aov, type="effects") #집단별 전체 평균과의 차이
model.tables(sprays.aov)

#TukeyHSD: 다중분석 (평균 간 차이가 통계적으로 유의한지 검정)
sprays.compare <- TukeyHSD(sprays.aov)
sprays.compare
sprays.compare$spray['D-C',]#특정 집단 간 비교 결과만 출력
plot(sprays.compare) #다중분석 결과 시각화: 신뢰구간에 0을 포함하지 않으면 집단 간 차이가 통계적으로 유의함
plot(sprays.compare, col="blue",las=1) #option: 컬러 지정, 라벨 가로로 표시

#TukeyHSD 분석 결과를 boxplot으로 시각화
install.packages("multcomp")
library(multcomp)
tuk.hsd <- glht(model=sprays.aov, linfct=mcp(spray="Tukey")) #glht: linfct로 지정한 분석 수행
cld(tuk.hsd, level=0.05) #같은 알파벳으로 출력되는 것들은 차이가 유의하지 않음
plot(cld(tuk.hsd, level=0.05), col="orange")

# [그림 4-12]
install.packages("car")
library(car)
qqPlot(InsectSprays$count, pch=20, col="deepskyblue", id=FALSE,
       main="Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")

shapiro.test(InsectSprays$count) #Shapiro-Wilk test: 데이터의 정규성 가정을 검정(귀무가설=정규성을 띈다)

outlierTest(sprays.aov)

leveneTest(count ~ spray, data=InsectSprays) #levene test: 등분산성 검정
bartlett.test(count ~ spray, data=InsectSprays) #bartlett test: 등분산성 검정

oneway.test(count ~ spray, data=InsectSprays) #일원분산분석 수행
oneway.test(count ~ spray, data=InsectSprays, var.equal=TRUE) #등분산 가정




######################
## 4.4 이원분산분석 ##
######################

str(ToothGrowth)
#dose변수를 factor화
ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels=c(0.5, 1.0, 2.0), labels=c("low", "med", "high"))
str(ToothGrowth)
ToothGrowth[seq(1, 60, 5),]

#tapply로 통계값 출력: with를 사용하면 매번 데이터셋을 명시할 필요 없음
with(ToothGrowth, tapply(len, list(supp, dose), length)) #길이
with(ToothGrowth, tapply(len, list(supp, dose), mean)) #평균
with(ToothGrowth, tapply(len, list(supp, dose), sd)) #표준편차

ToothGrowth.aov <- aov(len ~ supp * dose, data=ToothGrowth)
ToothGrowth.aov <- aov(len ~ supp + dose + supp:dose, data=ToothGrowth)

#귀무가설: supp, dose 두 변수는 주효과 / 상호작용효과가 없다.
summary(ToothGrowth.aov) #각 행의 유의확률이 모두 0.05보다 낮으므로 귀무가설 기각
model.tables(ToothGrowth.aov, type="means") #각 변수별로 집단의 평균값 출력


# 이원분산분삭 결과 시각화하기
# [그림 4-13] 보충제-투여량 조합 별 boxplot 시각화
boxplot(len ~ supp * dose, data=ToothGrowth,
        col=c("deeppink", "yellowgreen"), las=1,
        xlab="Vitamin C Type", ylab="Tooth Growth",
        main="Effects of Vitamin C on Tooth Growth of Guinea Pigs")

# [그림 4-14] 보충제-투여량 조합 별로 tooth length에 미치는 영향을 interactionplot으로 시각화
interaction.plot(x.factor=ToothGrowth$dose, trace.factor=ToothGrowth$supp, 
                 response=ToothGrowth$len, las=1, type="b", 
                 pch=c(1, 19), col=c("blue", "red"), trace.label="Supplement",
                 xlab="Dose Level", ylab="Tooth Length",
                 main="Interaction Plot for Tooth Growth of Guinea Pigs")

# [그림 4-15] 조합별 관측값과 신뢰구간
# interaction: 각 데이터가 어떤 변수 조합에 해당하는지를 ㅏㅂㄴ환
install.packages("gplots")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth,
          connect=list(c(1,3,5), c(2,4,6)),
          col=c("red", "green3"),
          xlab="Supplement and Dose Combination", ylab="Tooth Length",
          main="Means Plot for Tooth Growth of Guinea Pigs\nwith 95% CI of Mean")

# [그림 4-16] 변수별 산점도 및 line 시각화 -> 두 종류의 영양제 모두 투여량이 증가할수록 영향이 커짐
coplot(len ~ dose | supp, data=ToothGrowth, 
       col="steelblue", pch=19, 
       panel=panel.smooth, lwd=2, col.smooth="darkorange",
       xlab="Dose Level", ylab="Tooth Length")

# [그림 4-17] interaction2wt: 주효과와 상호작용 효과를 모두 표현
# boxplot은 주효과, lineplot은 상호작용효과를 나타낸다.
install.packages("HH")
library(HH)
interaction2wt(len ~ supp * dose,  data=ToothGrowth) #주효과와 상호작용효과를 별도의 패널에서 시각화

#집단 간 차이가 확인된다면 사후분석 수행
TukeyHSD(ToothGrowth.aov)
TukeyHSD(ToothGrowth.aov, which=c("dose"), conf.level=0.99) #which: 비교하고자 하는 집단만

####################
## 4.5 공분산분석 ##
####################

install.packages("faraway")
library(faraway)
str(sexab)

#컬럼별 통계량 분석
tapply(sexab$ptsd, sexab$csa, mean)
tapply(sexab$ptsd, sexab$csa, sd)
tapply(sexab$ptsd, sexab$csa, length)
 
sexab.aov <- aov(ptsd ~ cpa + csa, data=sexab)
summary(sexab.aov)

#공변량을 제거한 후의 조정된 집단별 평균 확인
install.packages("effects")
library(effects)
effect("csa", sexab.aov)

# [그림 4-18] 공변량분산분석 결과 및 시각화
library(HH)
ancova(ptsd ~ cpa + csa, data=sexab)

###########################
## 4.6 반복측정 분산분석 ##
###########################

head(CO2, 3); tail(CO2, 3)
CO2sub <- subset(CO2, Treatment=="chilled") #저온 처리된 데이터만 추출하여 분석에 사용
CO2sub$conc <- factor(CO2sub$conc) #factor화

#서로 다른 두 지역의 나무들의 CO2 흡수율에  차이가 있는가?
CO2sub.aov <- aov(uptake ~ Type * conc + Error(Plant/conc), data=CO2sub) #분산분석
summary(CO2sub.aov) #결과

# [그림 4-19]
par(mar=c(6,4,4,2))
boxplot(uptake ~ Type * conc, data=CO2sub,
        col=c("deepskyblue", "violet"), las=2, cex.axis=0.75,
        ylab="Carbon dioxide uptake rate", xlab="",
        main="Effects of Plant Type and CO2 on Carbon Dioxide Uptake")
legend("topleft", inset=0.02, 
       legend=c("Quebec", "Mississippi"), fill=c("deepskyblue", "violet"))

# [그림 4-20] 주효과와 상호작용효과 시각화
library(HH)
interaction2wt(uptake ~ conc * Type, data=CO2sub)

#########################
## 4.7 다변량 분산분석 ##
#########################

install.packages("heplots")
library(heplots)
str(Skulls)
library(dplyr)
sample_n(Skulls, 10) #샘플 추출

#다변량분산분석: 두개골의 측정값들이 시대에 따라 다른가?
attach(Skulls) #작업경로에 추가 -> 컬럼 명시할 때 안 적어도 되도록
y <- cbind(mb, bh, bl, nh)
aggregate(y, by=list(epoch), FUN=mean) 

Skulls.manova <- manova(y ~ epoch) #종속변수 다변량분산분석
summary(Skulls.manova)
summary.aov(Skulls.manova)
detach(Skulls) #작업공간에서 삭제
