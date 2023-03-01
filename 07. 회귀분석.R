####################
## 제7장 회귀분석 ##
####################

######################
## 7.1 단순회귀분석 ##
######################

install.packages("car")
library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

# [그림 7-2] 회귀분석 시각화(산점도)
plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue", pch=19,
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
abline(Prestige.lm, col="salmon", lwd=2) #귀선은 abline으로 나타냄

summary(Prestige.lm)

Prestige.lm.summary <- summary(Prestige.lm)
coef(Prestige.lm.summary)

anova(Prestige.lm) #분산분석 시행

coef(Prestige.lm) #계수만 출력

confint(Prestige.lm)
confint(Prestige.lm, level=0.99) #신뢰수준 변경

fitted(Prestige.lm)[1:3] #회귀식 예측값 반환
resid(Prestige.lm)[1:3] #관측값 - 예측값 잔차를 출력

# 값 예측하기
Prestige.new <- data.frame(education=c(5, 10, 15))
predict(Prestige.lm, newdata=Prestige.new)

predict(Prestige.lm, newdata=Prestige.new, interval="confidence")

#평균값을 기준으로 회귀분석 나누어 수행
mean(Prestige$education)
lm(income ~ education, data=Prestige, subset=(education > mean(education)))
lm(income ~ education, data=Prestige, subset=(education <= mean(education)))

######################
## 7.2 다항회귀분석 ##
######################

# [그림 7-4] scatterplot: 직선 회귀선과 곡선 회귀선을 모두 표시
library(car)
scatterplot(income ~ education, data=Prestige, pch=19, col="orangered", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="royalblue"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="green3"), #smoother 옵션으로 생성할 추체선을 지정
            xlab="Education (years)", ylab="Income (dollars)",
            main="Education and Income")

Prestige.poly <- lm(income ~ education + I(education^2), data=Prestige)

summary(Prestige.poly)

# [그림 7-6] 2차항 회귀 시각화
plot(Prestige$income ~ Prestige$education, pch=19, col="darkorange",
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
library(dplyr)
lines(arrange(data.frame(Prestige$education, fitted(Prestige.poly)), 
              Prestige$education), col="cornflowerblue", lwd=2)

# [그림 7-7]
scatterplot(eruptions ~ waiting, data=faithful, pch=19, col="deepskyblue", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="blueviolet"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="coral"),
            xlab="Waiting (minutes)", ylab="Eruptions (minutes)",
            main="Waiting Time Between Eruptions and the Duration of the Eruption")

faithful.poly <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), 
                    data=faithful)
summary(faithful.poly) #다항회귀 결과

faithful.lm <- lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm) #단순회귀 결과

######################
## 7.3 다중회귀분석 ##
######################

data(mtcars)
str(mtcars)
mtcars <- mtcars[c("mpg", "hp", "wt", "disp", "drat")]

summary(mtcars)
cor(mtcars)

# [그림 7-8]
library(car)
scatterplotMatrix(mtcars, pch=19, col="royalblue", cex=1.2,
                  regLine=list(method=lm, lty=1, lwd=3, col="salmon"),
                  smooth=list(smoother=loessLine, spread=FALSE, 
                              lty.smooth=1, lwd.smooth=3, col.smooth="forestgreen"),
                  main="Car Performance") #scatterplot: 컬럼별로 산점도와 회귀선

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)
library(stargazer)
stargazer(mtcars.lm, type="text", no.space=TRUE)

#표준화된 다중회귀
mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), 
                data=mtcars)
summary(mtcars.lm)
install.packages("QuantPsyc")
devtools::install_github("cran/QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
lm.beta(mtcars.lm)



######회귀분석 가정 진단#####
# [그림 7-9]
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
windows(width=7.0, height=7.0)

# 중첩된 회귀모델: lm1 모델은 lm2에 포함됨
mtcars.lm1 <- lm(mpg ~ hp + wt, data=mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
anova(mtcars.lm1, mtcars.lm2) #중첩된 회귀모델의 적합성 검정 - lm2에서 추가된 변수가 설명력을 증가시키는지 검정

AIC(mtcars.lm1, mtcars.lm2)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
step(mtcars.lm, direction="backward")

install.packages("leaps")
library(leaps)
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars, nbest=4)

# [그림 7-11] 모든 회귀모델의 수정결정계수 출력
library(RColorBrewer)
plot(mtcars.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"),
     main="All Subsets Regression")

summary(mtcars.regsubsets)
names(summary(mtcars.regsubsets))
summary(mtcars.regsubsets)$adjr2
which.max(summary(mtcars.regsubsets)$adjr2) #수정회귀계수 최대값인 회귀모델 인덱스 출력
coef(mtcars.regsubsets, 9) #인덱스 입력하여 그 회귀모델의 회귀식 출력

#############더미변수 회귀분석##############
str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean) #살충제 종류별 count 평균값 출력

sprays.lm <- lm(count ~ spray, data=InsectSprays) #회귀분석 수행
summary(sprays.lm)

contrasts(InsectSprays$spray) #더미변수 출력

sprays.aov <- aov(count ~ spray, data=InsectSprays) #일원분산분석 수행 -> 독립변수 차이가 유의한지 파악할 수 있음
summary(sprays.aov)
TukeyHSD(sprays.aov) #사후분석 수행 - 회귀분석 결과와 동일하다.

respray <- relevel(InsectSprays$spray, ref=6) #기준변수 6번재 값(F)로 변경
sprays.lm <- lm(count ~ respray, data=InsectSprays)
summary(sprays.lm)
contrasts(relevel(InsectSprays$spray, ref=6))

#####################################
## 7.4 매개효과분석과 조절효과분석 ##
#####################################

## 매개효과분석

data(mtcars)
model.total <- lm(mpg ~ disp, data=mtcars) #1단계: 전체 모델
summary(model.total)

model.M <- lm(wt ~ disp, data=mtcars) #2단계: X의 M에 대한 유의성 검정
summary(model.M)

model.Y <- lm(mpg ~ disp + wt, data=mtcars) #3단계: X+M의 Y에 대한 유의성 검정
summary(model.Y)

0.007*-3.351

install.packages("multilevel")
library(multilevel)
model.sob <- sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg)
model.sob

pnorm(abs(model.sob$z.value), lower.tail=FALSE)*2  #z값을 표준화하여 양쪽 꼬리의 면적 계산

install.packages("bda")
library(bda)
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

install.packages("mediation")
library(mediation)
set.seed(123)
model.M <- lm(wt ~ disp, data=mtcars)
model.Y <- lm(mpg ~ disp + wt, data=mtcars)
model.mediation <- mediate(model.m=model.M, model.y=model.Y, 
                           treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.mediation)


# [그림 7-16] 매개효과 시각화
plot(model.mediation, cex=1.2, col="royalblue", lwd=2,
     main="Mediation Effect Analysis")




############ 조절효과분석 ###############

mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

# [그림 7-19]
install.packages("effects")
library(effects)
m <- round(mean(mtcars$wt), 1); m
s <- round(sd(mtcars$wt), 1); s
plot(effect(term="hp:wt", mod=mtcars.lm, xlevels=list(wt=c(m-s, m, m+s))), 
     lines=list(multiline=TRUE, lwd=2, lty=c(3, 2, 1), 
                col=c("royalblue", "violet", "maroon")),
     main="Interaction Plot for Horsepower and Weight")

# [그림 7-20]
install.packages("rockchalk")
library(rockchalk)
plotSlopes(model=mtcars.lm, plotx="hp", modx="wt", modxVals="std.dev.", 
           pch=21, col=rainbow(3), cex=1, bg="dimgray",
           main="Interaction Plot for Horsepower and Weight")

####### 조절매개효과분석#######

#조절매개효과분석: 배기량~연비에 매개변수(무게), 조절변수(변속기)의 영향 검정
## 무게의 영향이 변속기에 따라 다른지 검정
data(mtcars)

model.M <- lm(wt ~ disp*am, data=mtcars) #매개변수모델 +  상호작용 변수
model.Y <- lm(mpg ~ disp*am + wt*am, data=mtcars) #종속변수모델 + 상호작용 변수

library(mediation)
set.seed(12)
model.med1 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=0),
                      treat="disp", mediator="wt", boot=TRUE, sims=500) #am=0(자동변속기)일 때 조절매개효과
summary(model.med1)
plot(model.med1) #결과 시각화

set.seed(12)
model.med2 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=1),
                      treat="disp", mediator="wt", boot=TRUE, sims=500) #am=1(수동변속기)일 때 조절매개효과
summary(model.med2)
plot(model.med2) #결과 시각화

set.seed(12)
model.med <- mediate(model.m=model.M, model.y=model.Y,
                     treat="disp", mediator="wt", sims=500)
set.seed(12)
test.modmed(object=model.med, 
            covariates.1=list(am=0), covariates.2=list(am=1), sims=500)



########################
## 7.5 페널티회귀분석 ##
########################

library(MASS)
str(Boston)

library(caret) #데이터 분할할 때 사용
set.seed(123)
train <- createDataPartition(y=Boston$medv, p=0.7, list=FALSE) #list=True일 경우 list형, False일 경우 df형
Boston.train <- Boston[train,] #train set
Boston.test <- Boston[-train,] #test set

x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv

library(glmnet)

## 릿지회귀분석

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0) #k fold validation 수행 (Ridge)

plot(Boston.cv) #결과 시각화

Boston.cv$lambda.min #mse를 최소화하는 lambda 값
log(Boston.cv$lambda.min)

#최적의 lambda값 확인한 후 이 값을 활용한 규제회귀 수행
Boston.gnet <- glmnet(x, y, family="gaussian", alpha=0, lambda=Boston.cv$lambda.min)
coef(Boston.gnet)

#완성된 모델을 바탕으로 예측값 수행
Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)

head(Boston.pred) #예측값 출력
as.vector(unname(Boston.pred))

postResample(pred=Boston.pred, obs=Boston.test$medv)

## 라소회귀분석

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=1) #k fold validation with Lasso
plot(Boston.cv)
Boston.cv$lambda.min #mse를 최소화하는 lambda 값
log(Boston.cv$lambda.min)

# [그림 7-24]
windows(width=7.0, height=5.5)
plot(Boston.cv)

Boston.cv$lambda.1se #간명도와 손실함수 간 균형점
log(Boston.cv$lambda.1se)

coef(Boston.cv, Boston.cv$lambda.min) #min lambda에서의 회귀계수

coef(Boston.cv, Boston.cv$lambda.1se) #lse lambda에서의 회귀계수. .으로 표시된 것들은 제거됨

#lambda.min으로 모델 생성 및 검정
Boston.gnet1 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv)

#lambda.lse로 모델 생성 및 검정
Boston.gnet2 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv)

## 일래스틱넷회귀분석

library(caret)
set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                   trControl=trainControl(method="cv", number=10),
                   tuneLength=10) #elasticnet alpha, lambda 조합 탐색

Boston.cv$bestTune #최적의 파라미터값 반환

Boston.gnet <- glmnet(x, y, family="gaussian", 
                      alpha=Boston.cv$bestTune$alpha, 
                      lambda=Boston.cv$bestTune$lambda) #최적의 파라미터로 학습 수행
coef(Boston.gnet)

Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

## 페널티회귀분석 모델 비교
library(caret)
lambda <- 10^seq(-5, 5, length=100)

#Ridge
set.seed(123)
ridge <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=0, lambda=lambda)) #tuneGrid: 파라미터 조합
coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)

#Lasso
set.seed(123)
lasso <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=1, lambda=lambda)) 
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)

#ElasticNet
set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                 trControl=trainControl(method="cv", number=10),
                 tuneLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda) #람다값은 앞에서 찾은 최적의 값 사용
elastic.pred <- predict(elastic, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)


#생성된 세 모델을 리스트로 변환하고 출력
models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models), metric="RMSE")  #표본추출하여 RMSE 출력

summary(diff(resamples(models), metric="RMSE")) #차이에 대한 유의확률 출력
