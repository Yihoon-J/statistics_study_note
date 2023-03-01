#####################
## 제10장 군집분석 ##
#####################

######################
## 10.1 유사도 측정 ##
######################

#연속형 변수
install.packages("flexclust")
library(flexclust)
data(nutrient)
head(nutrient, 5)

d <- dist(nutrient)
class(d)
labels(d)
as.matrix(d)[1:5, 1:5]

#범주형 변수
library(MASS)
str(survey)
levels(survey$Sex)
levels(survey$Smoke)
survey.dummy <- survey[c("Sex", "Smoke")]
head(survey.dummy, 5)

install.packages("fastDummies")
library(fastDummies)
survey.dummy <- dummy_cols(survey.dummy, 
                           remove_selected_columns=TRUE,
                           remove_first_dummy=TRUE,
                           ignore_na=TRUE)
head(survey.dummy, 5)
d <- dist(survey.dummy, method="binary")
as.matrix(d)[1:5, 1:5]

library(cluster)
d <- daisy(survey, metric="gower")
as.matrix(d)[1:5, 1:5]

##########################
## 10.2 계층적 군집분석 ##
##########################

library(flexclust)
data(nutrient)
nutrition <- nutrient
row.names(nutrition) <- tolower(row.names(nutrition)) #row name 소문자로 변경
nutrition.scaled <- scale(nutrition) #data scaling 수행

d <- dist(nutrition.scaled)
clustering.average <- hclust(d, method="average")

# [그림 10-1] 시각화(dandrogram)
plot(clustering.average, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", main="Hierarchical Clustering with Average Linkage")

# 최적의 군집 개수 파악: NbClust
install.packages("NbClust")
library(NbClust)
nc <- NbClust(nutrition.scaled, distance="euclidean",
              min.nc=3, max.nc=15, method="average")

str(nc)
nc$Best.nc

table(nc$Best.nc[1,])

# [그림 10-2] #voting 시각화
barplot(table(nc$Best.nc[1,]), col="mistyrose", 
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Clusters Proposed by Indices")

clusters <- cutree(clustering.average, k=5)
clusters

table(clusters)

# [그림 10-3]군집분석 결과 시각화
plot(clustering.average, hang=-1, cex=0.9, col="darkgreen",
     xlab="Food", 
     main="Hierarchical Clustering with Average Linkage\n Five Clusters")
rect.hclust(clustering.average, k=5)

aggregate(nutrition, by=list(cluster=clusters), mean)

a <- aggregate(nutrition.scaled, by=list(cluster=clusters), mean) #군집별 특성
n <- as.vector(table(clusters)) #군집 내 아이템 개수
cbind(a, n)

##########################
## K means 군집분석 ##
##########################

head(state.x77)
state.scaled <- scale #데이터 스케일링

library(NbClust)
set.seed(123)
nc <- NbClust(state.scaled, distance="euclidean", 
              min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.nc[1,]) #최적의 군집 개수 voting

 # [그림 10-4]
barplot(table(nc$Best.nc[1,]), col="lightsteelblue",
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Clusters Proposed by Indices")

set.seed(123)
clustering.km <- kmeans(state.scaled, centers=3, nstart=25)

str(clustering.km)

clustering.km$cluster #요소별 소속 군집
clustering.km$centers #최종 중심점
clustering.km$size #군집 크기

aggregate(state.x77, by=list(cluster=clustering.km$cluster), mean) #출력값은 표준화된 값이므로, 이를 원본으로 돌려줌

# [그림 10-5]
library(cluster)
clusplot(x=state.x77, clus=clustering.km$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="Cluster Plot")


##################
## PAM 군집분석 ##
##################

install.packages("rattle")
library(rattle)
head(wine)

library(cluster)
set.seed(123)
clustering.pam <- pam(wine[-1], k=3, stand=TRUE)
 
str(clustering.pam) 

clustering.pam$clusinfo #각 군집의 특성
clustering.pam$medoids # 각 군집의 중심점
clustering.pam$id.med #각 medoid의 실제 케이스 번호 (몇 번째 요소인지)

clustering.pam$clustering #각 케이스가 어느 군집인지
aggregate(wine[-1], by=list(cluster=clustering.pam$clustering), mean) #군집별 평균 계산

# [그림 10-6]
windows(width=7.0, height=5.5)
clusplot(clustering.pam, color=TRUE, shade=TRUE, labels=4, lines=0, 
         main="Cluster Plot") #시각화

result.pam <- table(wine$Type, clustering.pam$clustering, 
                    dnn=c("Actual", "Clustered")) #실제 분류와의 교차표 생성
result.pam
mean(wine$Type==clustering.pam$clustering)

library(flexclust)
randIndex(result.pam)