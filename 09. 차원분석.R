####################
## 제9장 차원분석 ##
####################

####################
## 9.1 주성분분석 ##
####################

str(state.x77)
colnames(state.x77)

pca <- prcomp(state.x77, scale=TRUE) #주성분분석 수행
summary(pca)

# [그림 9-2] scree plot으로 적정 주성분 개수 결정
plot(pca, type="l", pch=19, lwd=2, col="red", main="Scree Plot")

str(pca)

pca$rotation
round(pca$rotation, 3)
print(pca$rotation, digits=2)

round(scale(state.x77) %*% pca$rotation, 3)

round(pca$x, 3)
pca$x - (scale(state.x77) %*% pca$rotation)

round(pca$x[,1], 3)
round(pca$x[,c(1, 2)], 3)

round(cor(pca$x), 3)
cor(pca$x)

# [그림 9-3] 주성분분석 결과 시각화(biplot)
par(mai=c(0.9,0.1,0.7,0.1))
biplot(pca, cex=c(0.5, 0.75), main="Biplot\n\n")

##################
## 9.2 요인분석 ##
##################

install.packages("ade4")
library(ade4)
data(olympic)
str(olympic)

library(psych)

# [그림 9-5]
fa.parallel(olympic$tab, fm="ml", fa="fa", n.iter=100) #요인 추출: ml(로그우도법)  fa(요인분석))

install.packages("nFactors")
library(nFactors)
nScree(olympic$tab)

eigen(cor(olympic$tab)) #요인 개수별로 고유값행렬 직접 출력

fa <- factanal(olympic$tab, factors=2, scores="regression")
fa

fa$loadings 
print(fa$loadings, cutoff=0.001)

#공통성(설명되는 분산의 비율) 출력
round(fa$uniquenesses, 3)
round(1 - fa$uniquenesses, 3)

0.772*0.814+(-0.089)*0.226

round(fa$loadings %*% t(fa$loadings), 3)

round(cor(olympic$tab), 3)
round(fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses), 3)
round(cor(olympic$tab) - 
        (fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses)), 3)

# [그림 9-6] #요인분석 결과 시각화 1
library(psych)
factor.plot(fa, labels=colnames(olympic$tab), pch=20, pos=4, title="Factor Plot")

# [그림 9-7] #요인분석 결과 시각화 2
library(gplots)
library(RColorBrewer)
heatmap.2(abs(fa$loadings), col=brewer.pal(9, "Blues"),trace="none", key=FALSE,
          dend="none", cexCol=1.2, main="\n\n\n\nFactor Loadings")

# [그림 9-8] #요인분석 결과 시각화 3
install.packages("semPlot")
library(semPlot)
semPaths(fa, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, title=TRUE)

fa.scores <- fa$scores
fa.scores

# [그림 9-9] #시각화3의 요인에 이름 부여
windows(width=7.0, height=7.0)
colnames(fa.scores) <- c("Run", "Throw")
heatmap.2(fa.scores, col=brewer.pal(9, "GnBu"),trace="none", key=FALSE, 
          dend="none", cexCol=1.2, main="\n\n\n\n\n\nFactor Scores by Athletes")

library(ade4)
data(olympic)

library(psych)
fa <- fa(olympic$tab, nfactors=2, rotate="varimax", fm="ml")
fa

print(fa$loadings, cutoff=0.001)
fa$loadings
fa$scores
fa$weights

# [그림 9-10]
windows(width=7.0, height=5.5)
fa.diagram(fa, simple=FALSE, cut=0.3, digits=2, col="blue", adj=2, 
           e.size=0.08, rsize=2)

######################
## 9.3 다차원척도법 ##
######################

#사전에 생성된 거리행렬을 사용하는 경우
str(eurodist)
labels(eurodist)  
as.matrix(eurodist)[1:5, 1:5]

eurocity.mds <- cmdscale(d=eurodist)
head(eurocity.mds)

# [그림 9-11]
plot(eurocity.mds, type="n", main="Multidimensional Scaling Plot")
text(eurocity.mds, rownames(eurocity.mds), col="maroon", cex=0.7)

str(USJudgeRatings)

#거리행렬을 직접 계산해야 하는 경우
USJudgeRatings.dist <- dist(USJudgeRatings) #거리 계산
USJudgeRatings.mds <- cmdscale(USJudgeRatings.dist)

# [그림 9-12]
plot(USJudgeRatings.mds, type="n", main="Multidimensional Scaling Plot")
text(USJudgeRatings.mds, rownames(USJudgeRatings), col="blue", cex=0.6)

str(mtcars)

library(cluster)
mtcars.dist <- daisy(mtcars, metric="gower")

library(MASS)
mtcars.mds <- isoMDS(mtcars.dist)
str(mtcars.mds)

# [그림 9-13]
plot(mtcars.mds$points, type="n", main="Multidimensional Scaling Plot")
text(mtcars.mds$points, rownames(mtcars), col="purple", cex=0.7)