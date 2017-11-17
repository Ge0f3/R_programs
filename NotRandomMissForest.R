library(missForest)
library(mice)
library(missForest)
library(hydroGOF)
iris<-read.csv('/Users/geofe/Documents/workspace/DataSet/Iris.csv')
iris<-iris[,-1]
iris
irisNa<-iris
irisNa[50,]$SepalLengthCm<- NA
irisNa[50,]$SepalWidthCm<- NA
irisNa[50,]$PetalLengthCm<- NA
irisNa[50,]$PetalWidthCm<- NA
irisNa[150,]$SepalLengthCm<- NA
irisNa[150,]$SepalWidthCm<- NA
irisNa[150,]$PetalLengthCm<- NA
irisNa[150,]$PetalWidthCm<- NA
irisNa[100,]$SepalLengthCm<- NA
irisNa[100,]$SepalWidthCm<- NA
irisNa[100,]$PetalLengthCm<- NA
irisNa[100,]$PetalWidthCm<- NA
irisNa
irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris.imp <- missForest(irisNa)
iriscomplete1<-iris.imp$ximp

amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")
iriscomplete1<-amelia_fit$imputations[[1]]
iriscomplete1
rmse1<-rmse(iris[,-5],iriscomplete[,-5])
rmse1

###############Iris KNN########
iris %>% ggvis(~SepalLengthCm, ~SepalWidthCm, fill = ~Species) %>% layer_points()
str(iris)
normalize <- function(x) { 
  num <- x - min(x) 
  denom <- max(x) - min(x) 
  return (num/denom) 
}
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
set.seed(1234)
# replace=TRUE is required to prevent current selection from biasing next selection
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
# view distribution of 1s and 2s
table(ind)
prop.table(table(ind))
round(prop.table(table(ind)) * 100, digits = 1)
#iris training and testing data
iris.training <- iris[ind==1,1:4] 
iris.test <- iriscomplete1[ ind==2,1:4]
iris.trainLabels <- iris[ind==1, 5] 
iris.testLabels <- iriscomplete1[ind==2, 5]
#creating a KNN model for 2%
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision
###ENd of KNN
