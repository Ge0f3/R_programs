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

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete1<-mice::complete(iris_imputed,1)

rmse1<-rmse(iris[,-5],iriscomplete[,-5])
rmse1

#generating 5% Na values

irisNa[49,]$SepalLengthCm<- NA
irisNa[49,]$SepalWidthCm<- NA
irisNa[49,]$PetalLengthCm<- NA
irisNa[49,]$PetalWidthCm<- NA
irisNa[48,]$SepalLengthCm<- NA
irisNa[48,]$SepalWidthCm<- NA
irisNa[48,]$PetalLengthCm<- NA
irisNa[48,]$PetalWidthCm<- NA
irisNa[149,]$SepalLengthCm<- NA
irisNa[149,]$SepalWidthCm<- NA
irisNa[149,]$PetalLengthCm<- NA
irisNa[149,]$PetalWidthCm<- NA
irisNa[98,]$SepalLengthCm<- NA
irisNa[98,]$SepalWidthCm<- NA
irisNa[99,]$SepalLengthCm<- NA
irisNa[99,]$SepalWidthCm<- NA
irisNa[99,]$PetalLengthCm<- NA
irisNa[99,]$PetalWidthCm<- NA
irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete2<-mice::complete(iris_imputed,1)

rmse2<-rmse(iris[,-5],iriscomplete[,-5])
rmse2

#generating 10% missing values
irisNa[45,]$SepalLengthCm<- NA
irisNa[45,]$SepalWidthCm<- NA
irisNa[45,]$PetalLengthCm<- NA
irisNa[45,]$PetalWidthCm<- NA
irisNa[46,]$SepalLengthCm<- NA
irisNa[46,]$SepalWidthCm<- NA
irisNa[46,]$PetalLengthCm<- NA
irisNa[46,]$PetalWidthCm<- NA
irisNa[46,]$SepalLengthCm<- NA
irisNa[47,]$SepalWidthCm<- NA
irisNa[47,]$PetalLengthCm<- NA
irisNa[47,]$PetalWidthCm<- NA
irisNa[97,]$SepalLengthCm<- NA
irisNa[97,]$SepalWidthCm<- NA
irisNa[97,]$PetalLengthCm<- NA
irisNa[97,]$PetalWidthCm<- NA
irisNa[96,]$SepalLengthCm<- NA
irisNa[96,]$SepalWidthCm<- NA
irisNa[96,]$PetalLengthCm<- NA
irisNa[96,]$PetalWidthCm<- NA
irisNa[95,]$SepalLengthCm<- NA
irisNa[95,]$SepalWidthCm<- NA
irisNa[98,]$PetalLengthCm<- NA
irisNa[98,]$PetalWidthCm<- NA
irisNa[148,]$SepalLengthCm<- NA
irisNa[148,]$SepalWidthCm<- NA
irisNa[148,]$PetalLengthCm<- NA
irisNa[148,]$PetalWidthCm<- NA
irisNa[147,]$SepalLengthCm<- NA
irisNa[147,]$SepalWidthCm<- NA

irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete3<-mice::complete(iris_imputed,1)

rmse3<-rmse(iris[,-5],iriscomplete[,-5])
rmse3

#generating 15% Na values
irisNa[43,]$SepalLengthCm<- NA
irisNa[43,]$SepalWidthCm<- NA
irisNa[43,]$PetalLengthCm<- NA
irisNa[43,]$PetalWidthCm<- NA
irisNa[44,]$SepalLengthCm<- NA
irisNa[44,]$SepalWidthCm<- NA
irisNa[44,]$PetalLengthCm<- NA
irisNa[44,]$PetalWidthCm<- NA
irisNa[146,]$SepalLengthCm<- NA
irisNa[146,]$SepalWidthCm<- NA
irisNa[146,]$PetalLengthCm<- NA
irisNa[146,]$PetalWidthCm<- NA
irisNa[147,]$PetalLengthCm<- NA
irisNa[147,]$PetalWidthCm<- NA
irisNa[96,]$SepalLengthCm<- NA
irisNa[96,]$SepalWidthCm<- NA
irisNa[96,]$SepalLengthCm<- NA
irisNa[96,]$SepalWidthCm<- NA
irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete4<-mice::complete(iris_imputed,1)

rmse4<-rmse(iris[,-5],iriscomplete[,-5])
rmse4

#generating for 20 values
irisNa[41,]$SepalLengthCm<- NA
irisNa[41,]$SepalWidthCm<- NA
irisNa[41,]$PetalLengthCm<- NA
irisNa[41,]$PetalWidthCm<- NA
irisNa[42,]$SepalLengthCm<- NA
irisNa[42,]$SepalWidthCm<- NA
irisNa[42,]$PetalLengthCm<- NA
irisNa[42,]$PetalWidthCm<- NA
irisNa[145,]$SepalLengthCm<- NA
irisNa[145,]$SepalWidthCm<- NA
irisNa[145,]$PetalLengthCm<- NA
irisNa[145,]$PetalWidthCm<- NA
irisNa[144,]$PetalLengthCm<- NA
irisNa[144,]$PetalWidthCm<- NA
irisNa[95,]$SepalLengthCm<- NA
irisNa[95,]$SepalWidthCm<- NA
irisNa[95,]$SepalLengthCm<- NA
irisNa[95,]$SepalWidthCm<- NA
irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete5<-mice::complete(iris_imputed,1)

rmse5<-rmse(iris[,-5],iriscomplete[,-5])
rmse5

#generating for 25 values
irisNa[40,]$SepalLengthCm<- NA
irisNa[40,]$SepalWidthCm<- NA
irisNa[40,]$PetalLengthCm<- NA
irisNa[40,]$PetalWidthCm<- NA
irisNa[39,]$SepalLengthCm<- NA
irisNa[39,]$SepalWidthCm<- NA
irisNa[39,]$PetalLengthCm<- NA
irisNa[39,]$PetalWidthCm<- NA
irisNa[144,]$SepalLengthCm<- NA
irisNa[144,]$SepalWidthCm<- NA
irisNa[144,]$PetalLengthCm<- NA
irisNa[144,]$PetalWidthCm<- NA
irisNa[143,]$PetalLengthCm<- NA
irisNa[143,]$PetalWidthCm<- NA
irisNa[94,]$SepalLengthCm<- NA
irisNa[94,]$SepalWidthCm<- NA
irisNa[94,]$SepalLengthCm<- NA
irisNa[94,]$SepalWidthCm<- NA
irisNa$SepalLengthCm<-as.numeric(irisNa$SepalLengthCm)
irisNa$SepalWidthCm<-as.numeric(irisNa$SepalWidthCm)
irisNa$PetalLengthCm<-as.numeric(irisNa$PetalLengthCm)
irisNa$PetalWidthCm<-as.numeric(irisNa$PetalWidthCm)

iris_imputed<-mice(irisNa,m=1,maxit = 50,method = 'pmm',seed = 500)
iriscomplete6<-mice::complete(iris_imputed,1)

rmse6<-rmse(iris[,-5],iriscomplete[,-5])
rmse6

rmsev<-c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6)

plot(rmsev,type='l',col='blue')
barchart(rmsev)


###############Iris KNN########
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
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

#creating a KNN model for 5%
iris.test <- iriscomplete2[ ind==2,1:4]
iris.testLabels <- iriscomplete2[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision

#creating a KNN model for 10%
iris.test <- iriscomplete3[ ind==2,1:4]
iris.testLabels <- iriscomplete3[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision

#creating a KNN model for 15%
iris.test <- iriscomplete3[ ind==2,1:4]
iris.testLabels <- iriscomplete3[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision

#creating a KNN model for 20%
iris.test <- iriscomplete4[ ind==2,1:4]
iris.testLabels <- iriscomplete4[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision
#creating a KNN model for 25%
iris.test <- iriscomplete5[ ind==2,1:4]
iris.testLabels <- iriscomplete5[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
comparision
######End KNN

