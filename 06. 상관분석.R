####################
## 제6장 상관분석 ##
####################

##################
## 6.1 상관관계 ##
##################

library(MASS)
str(cats)

# [그림 6-1] 데이터 산점도 시각화
plot(cats$Hwt ~ cats$Bwt,
     col="forestgreen", pch=19,
     xlab="Body Weight (kg)", ylab="Heart Weight (g)",
     main="Body Weight and Heart Weight of Cats")

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))

with(cats, cor.test(Bwt, Hwt))

with(cats, cor.test(Bwt, Hwt, alternative="greater", conf.level=0.99)) #(상관계수 0 기준) greater, less로단측검정 수행

with(cats, cor.test(~ Bwt + Hwt)) #formula로 표현

cor.test(~ Bwt + Hwt, data=cats)

cor.test(~ Bwt + Hwt, data=cats, subset=(Sex=="F")) #formula + subset 조건

str(iris)
cor(iris[-5]) #여러 데이터의 상관계수 행렬 생성

iris.cor <- cor(iris[-5])
class(iris.cor)
str(iris.cor)

iris.cor["Petal.Width", "Petal.Length"] #원하는 값만 추출

install.packages("psych")
library(psych)
corr.test(iris[-5]) #상관계수와 유의수준 모두 출력

print(corr.test(iris[-5]), short=FALSE)

old.op <- options(digits=2)
cor(state.x77)
options(old.op)

# [그림 6-2] 상관계수 시각화 1
library(psych)
pairs.panels(state.x77, bg="red", pch=21, hist.col="gold", 
             main="Correlation Plot of US States Data") #상관계수 시각화

# [그림 6-3] 상관계수 시각화 2
install.packages("corrgram")
library(corrgram)
corrgram(state.x77, order=TRUE, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of US States Data")

# [그림 6-4] 상관계수 시각화 3
library(corrgram)
cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1", 
                                           "darkkhaki", "darkgreen"))
corrgram(state.x77, order=FALSE, col.regions=cols,
        lower.panel=panel.pie, upper.panel=panel.conf, 
        text.panel=panel.txt, main="Corrgram of US States Data")

 ####################
 ## 6.2 편상관관계 ##
 ####################
 
 colnames(mtcars)
 mtcars2 <- mtcars[, c("mpg", "cyl", "hp", "wt")]
 cor(mtcars2) #단순 상관계수
 
 #편상관계수: pcor함수 사용
 install.packages("ggm")
 library(ggm)
 detach(package:ppcor)
 pcor(c(1, 3, 2, 4), cov(mtcars2)) #편상관계수 계산할 변수 두 개 지정하고, 배제할 변수 지정
 pcor(c("mpg", "hp", "cyl", "wt"), cov(mtcars2)) #변수명을 지정해 주어도 됨
 
 pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)), q=2, n=nrow(mtcars2))
 
 search()
 
 install.packages("ppcor")
 library(ppcor)
 detach(package:ggm)
 pcor(mtcars2)
 
 pcor.test(mtcars2["mpg"], mtcars2["hp"], mtcars2[c("cyl", "wt")]) #마찬가지로 지정하여 특정 변수에 대해서만 수행