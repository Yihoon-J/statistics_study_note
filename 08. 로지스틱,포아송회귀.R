#############################################
## 제8장 로지스틱회귀분석과 포아송회귀분석 ##
#############################################

###############################
## 8.2 이항 로지스틱회귀분석 ##
###############################

install.packages("modeldata")
library(modeldata)
data(mlc_churn)
str(mlc_churn) #통신사 고객이탈 여부 데이터

library(tibble)
churn <- mlc_churn
levels(churn$churn)
churn <- churn[-c(1, 3)] #일부 변수 제외하여 사용
churn$churn <- factor(ifelse(churn$churn=="no", 1, 2), #혼동 방지를 위해 원래와 반대로 인코딩 진행
                      levels=c(1, 2), labels=c("no", "yes"))
levels(churn$churn)

churn.train <- churn[1:3333,] #train set
churn.test <- churn[3334:5000,] #test set
churn.train
churn.test

# 두 표본의 분포가 비슷한지 확인하는 작업 꼭 거쳐야 함.
table(churn.train$churn)
prop.table(table(churn.train$churn))
table(churn.test$churn)
prop.table(table(churn.test$churn))

#이항로지스틱분석 수행
churn.logit <- glm(churn ~ ., data=churn.train, family=binomial(link="logit"))
summary(churn.logit) #범주형 변수가 자동으로 더미범주화 된것을 볼 수 있음.
exp(coef(churn.logit))

# 로지스틱회귀 유의성 검정
pchisq(q=2758.3-2158.7, df=3332-3315, lower.tail=FALSE)  #이탈도 검정(값 직접 입력)
pchisq(q=churn.logit$null.deviance-churn.logit$deviance, 
       df=churn.logit$df.null-churn.logit$df.residual, 
       lower.tail=FALSE) #이탈도 검정(값 불러오기)

churn.test <- churn[3334:5000,]
churn.logit.pred <- predict(churn.logit, newdata=churn.test, type="response") #예측 확률 출력
head(churn.logit.pred)

churn.logit.pred <- factor(churn.logit.pred > 0.5, 
                           levels=c(FALSE, TRUE), labels=c("no", "yes")) #0.5를 기준으로 이진값 변환
head(churn.logit.pred)
table(churn.logit.pred) #전체

table(churn.test$churn, churn.logit.pred, dnn=c("Actual", "Predicted")) #예측 - 실제 비교
mean(churn.test$churn==churn.logit.pred)

churn.logit2 <- step(churn.logit) #통계적으로 유의한 변수만 선택 - 보다 간명한 모델 생성하기
summary(churn.logit2)

churn.test$international_plan <- ifelse(churn.test$international_plan=="no", 0, 1)
churn.test$voice_mail_plan <- ifelse(churn.test$voice_mail_plan=="no", 0, 1)
table(churn.test$number_customer_service_calls)
testdata <- data.frame(international_planyes=mean(churn.test$international_plan),
                       voice_mail_planyes=mean(churn.test$voice_mail_plan),
                       number_vmail_messages=mean(churn.test$number_vmail_messages),
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge),
                       number_customer_service_calls=c(0:7))
testdata

z <- coef(churn.logit2)[1] + 
  (as.matrix(testdata) %*% coef(churn.logit2)[-1])
p <- 1/(1+exp(-z))
testdata$prob <- p
testdata[c("number_customer_service_calls", "prob")]

testdata <- data.frame(number_customer_service_calls=c(0:7),
                       international_plan="no",
                       voice_mail_plan="no",
                       number_vmail_messages=mean(churn.test$number_vmail_messages),
                       total_day_charge=mean(churn.test$total_day_charge),
                       total_eve_minutes=mean(churn.test$total_eve_minutes),
                       total_night_charge=mean(churn.test$total_night_charge),
                       total_intl_calls=mean(churn.test$total_intl_calls),
                       total_intl_charge=mean(churn.test$total_intl_charge))
testdata
testdata$prob <- predict(churn.logit2, newdata=testdata, type="response")
testdata[c("number_customer_service_calls", "prob")]

deviance(churn.logit2)/df.residual(churn.logit2)

fit.origin <- glm(formula=churn ~ international_plan + voice_mail_plan + 
                    number_vmail_messages + total_day_charge + total_eve_minutes + 
                    total_night_charge + total_intl_calls + total_intl_charge + 
                    number_customer_service_calls, family=binomial(), 
                  data=churn.train)
fit.overdis <- glm(formula=churn ~ international_plan + voice_mail_plan + 
                     number_vmail_messages + total_day_charge + total_eve_minutes + 
                     total_night_charge + total_intl_calls + total_intl_charge + 
                     number_customer_service_calls, family=quasibinomial(), 
                   data=churn.train)
pchisq(summary(fit.overdis)$dispersion * fit.origin$df.residual, 
       fit.origin$df.residual, lower.tail=FALSE)

#################################
## 8.3 페널티 로지스틱회귀분석 ##
#################################

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

PimaIndiansDiabetes3 <- na.omit(PimaIndiansDiabetes2) #결측치 제거

library(caret)
set.seed(123)
train <- createDataPartition(y=PimaIndiansDiabetes3$diabetes, p=0.7, list=FALSE)
diabetes.train <- PimaIndiansDiabetes3[train,]
diabetes.test <- PimaIndiansDiabetes3[-train,] #train test split

x <- model.matrix(diabetes ~ ., diabetes.train)[,-1]
y <- ifelse(diabetes.train$diabetes == "pos", 1, 0) #x, y 자료형 가공

library(glmnet)
set.seed(123)
diabetes.cv <- cv.glmnet(x=x, y=y, family="binomial", alpha=1) #cross validation 수행

diabetes.cv$lambda.min
diabetes.cv$lambda.1se

coef(diabetes.cv, diabetes.cv$lambda.min)
coef(diabetes.cv, diabetes.cv$lambda.1se) #회귀계수 출력

#예측 1: lambda.min
diabetes.gnet1 <- glmnet(x, y, family="binomial",
                         alpha=1, lambda=diabetes.cv$lambda.min) #모델 생성
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[,-1]
diabetes.pred1 <- predict(diabetes.gnet1, newx=diabetes.test.x, type="response") #데이터 예측
diabetes.pred1 <- ifelse(diabetes.pred1 > 0.5, "pos", "neg") #예측값에 대한 분류를 수행
table(diabetes.test$diabetes, diabetes.pred1, dnn=c("Actual", "Predicted"))
mean(diabetes.pred1==diabetes.test$diabetes)

#예측 2:  lambda.lse
diabetes.gnet2 <- glmnet(x, y, family="binomial",
                         alpha=1, lambda=diabetes.cv$lambda.1se)
diabetes.test.x <- model.matrix(diabetes ~ ., diabetes.test)[,-1]
diabetes.pred2 <- predict(diabetes.gnet2, newx=diabetes.test.x, type="response")
diabetes.pred2 <- ifelse(diabetes.pred2 > 0.5, "pos", "neg")
table(diabetes.test$diabetes, diabetes.pred2, dnn=c("Actual", "Predicted"))
mean(diabetes.pred2==diabetes.test$diabetes)

diabetes.logit <- glm(diabetes ~ ., data=diabetes.train, family=binomial(link="logit"))
diabetes.logit.pred <- predict(diabetes.logit, newdata=diabetes.test, type="response")
diabetes.logit.pred <- ifelse(diabetes.logit.pred > 0.5, "pos", "neg")
table(diabetes.test$diabetes, diabetes.logit.pred, dnn=c("Actual", "Predicted"))
mean(diabetes.logit.pred==diabetes.test$diabetes)

###############################
## 8.4 다항 로지스틱회귀분석 ##
###############################

install.packages("EffectStars")
library(EffectStars)
data(PID)
str(PID)
head(PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family=multinomial(), data=PID) #다항로지스틱회귀
summary(pid.mlogit) #회귀결과

exp(coef(pid.mlogit)) #회귀계수를 지수로 변환

pid.mlogit.pred <- fitted(pid.mlogit) #확률 반환
head(pid.mlogit.pred)

#학력의 영향 확인
testdata <- data.frame(Education=c("low", "high"),
                       TVnews=mean(PID$TVnews),
                       Income=mean(PID$Income),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) #학력의 영향 검증을 위해 학력 제외한 나머지는 평균값으로 고정한 가상 데이터 생성
testdata

pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")
cbind(testdata, pid.mlogit.pred) #결과 출력

#소득의 영향 확인
testdata <- data.frame(Education=rep("low", 5),
                       TVnews=mean(PID$TVnews),
                       Income=seq(20, 100, 20),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population)) #가상 데이터 생성
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")
cbind(testdata, pid.mlogit.pred)

##fgl데이터로 로지스틱회귀 수행##
library(MASS)
str(fgl)
head(fgl)

fgl.scaled <- cbind(scale(fgl[,1:9]), fgl[10]) #예측변수를 표준화하고 결과변수 붙임
head(fgl.scaled)
str(fgl.scaled)

set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,] #train test split
table(fgl.train$type); sum(table(fgl.train$type))
table(fgl.test$type); sum(table(fgl.test$type))

library(nnet)
fgl.mlogit <- multinom(type ~ ., data=fgl.train) #회귀모델 생성
summary(fgl.mlogit)

z <- summary(fgl.mlogit)$coefficients/summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1))*2
print(p, digits=3) #회귀계수 유의확률 출력

fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type="probs") #생성된 모델로 예측 수행
head(fgl.mlogit.pred)

cbind(round(fgl.mlogit.pred, 3), fgl.test["type"])

# [그림 8-2]
idx1 <- fgl.test$type == "WinF"
idx2 <- fgl.test$type == "WinNF"
idx3 <- fgl.test$type == "Veh"
idx4 <- fgl.test$type == "Con"
idx5 <- fgl.test$type == "Tabl"
idx6 <- fgl.test$type == "Head"
ys <- c(fgl.mlogit.pred[idx1, 1], fgl.mlogit.pred[idx2, 2],
        fgl.mlogit.pred[idx3, 3], fgl.mlogit.pred[idx4, 4],
        fgl.mlogit.pred[idx5, 5], fgl.mlogit.pred[idx6, 6])
xs <- c(fgl.test$type[idx1], fgl.test$type[idx2], fgl.test$type[idx3],
        fgl.test$type[idx4], fgl.test$type[idx5], fgl.test$type[idx6])
windows(width=7.0, height=5.5)
boxplot(ys ~ xs, names=levels(fgl.test$type), ylim=c(0,1), col=rainbow(6),
        xlab="Glass Type", ylab="Estimated Probabilities",
        main="Probabilities of Group Membership against True Group")

fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
table(fgl.test$type, 
      factor(fgl.mlogit.pred, levels=levels(fgl.test$type),
             labels=levels(fgl.test$type)), dnn=c("Actual", "Predicted"))
mean(fgl.test$type==fgl.mlogit.pred)

data(fgl)
fgl.scaled <- cbind(scale(fgl[,1:9]), fgl[10])
fgl.mlogit.cv <- numeric()
for (i in 1:100) {
  train <- sample(nrow(fgl), 0.7*nrow(fgl))
  fgl.train <- fgl.scaled[train,]
  fgl.test <- fgl.scaled[-train,]
  fgl.mlogit <- multinom(type ~ ., data=fgl.train)
  fgl.mlogit.pred <- predict(fgl.mlogit, newdata=fgl.test, type="probs")
  fgl.mlogit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
  fgl.mlogit.cv[i] <- mean(fgl.test$type==fgl.mlogit.pred)
}
fgl.mlogit.cv
summary(fgl.mlogit.cv)

# [그림 8-3]
windows(width=7.0, height=5.5)
boxplot(fgl.mlogit.cv, horizontal=TRUE, col="tomato", xlab="Accuracy",
        main="Accuracy for Forensic Glass (100 samples)")

########################
## 8.5 포아송회귀분석 ##
########################

install.packages("robust")
library(robust)
data(breslow.dat)
str(breslow.dat)

seizure <- breslow.dat[c("Base", "Age", "Trt", "sumY")] #사용할 변수 추출

summary(seizure)

# [그림 8-4] 데이터 분포 확인
hist(seizure$sumY, breaks=20, col="cornflowerblue", 
     xlab="Seizure Count", main="Distribution of Seizures")

seizure.poisson <- glm(sumY ~ Base + Age + Trt, data=seizure, family=poisson) #포아송회귀 수행
summary(seizure.poisson)

coef(seizure.poisson) #회귀계수

exp(coef(seizure.poisson)) #지수함수 취해서 원래 스케일에 맞도록 출력

deviance(seizure.poisson)/df.residual(seizure.poisson) #이탈도 비 확인: 1보다 크면 과산포 의심

install.packages("qcc")
library(qcc)
qcc.overdispersion.test(seizure$sumY, type="poisson") #과산포 검정

seizure.qpoisson <- glm(sumY ~ Base + Age + Trt, data=seizure, 
                        family=quasipoisson())
summary(seizure.qpoisson)

0.022740/0.013800
-0.152701/0.163943

###시간간격 일정하지 않은 경우 포아송회귀분석
library(MASS)
str(ships)
?ships

shipsinc <- subset(ships, service > 0)
shipsinc$year <- factor(shipsinc$year)
shipsinc$period <- factor(shipsinc$period)
levels(shipsinc$year); levels(shipsinc$period)

shipsinc.poisson <- glm(incidents ~ type + year + period, data=shipsinc, 
                        family=poisson(), offset=log(service)) #offset에 시간변수 추가
summary(shipsinc.poisson)

deviance(shipsinc.poisson)/df.residual(shipsinc.poisson) #이탈도 비
library(qcc)
qcc.overdispersion.test(shipsinc$incidents, type="poisson")

shipsinc.qpoisson <- update(shipsinc.poisson, family=quasipoisson())
summary(shipsinc.qpoisson)
exp(coef(shipsinc.qpoisson))