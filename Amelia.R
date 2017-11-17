install.packages("Amelia")
library(Amelia)

#load iris dataset
data("iris")

#seed 2% missing values
iris.mis <- prodNA(iris, noNA = 0.02)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#access imputed outputs
amelia_fit$imputations[[1]]
amelia_fit$imputations[[2]]
amelia_fit$imputations[[3]]
amelia_fit$imputations[[4]]
amelia_fit$imputations[[5]]

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete1<-amelia_fit$imputations[[1]]
rmse1<-rmse(iris[,-5],iriscomplete[,-5])

#seed 5% missing values
iris.mis <- prodNA(iris, noNA = 0.05)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete2<-amelia_fit$imputations[[1]]
rmse2<-rmse(iris[,-5],iriscomplete[,-5])

#seed 10% missing values
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete3<-amelia_fit$imputations[[1]]
rmse3<-rmse(iris[,-5],iriscomplete[,-5])

#seed 15% missing values
iris.mis <- prodNA(iris, noNA = 0.15)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete4<-amelia_fit$imputations[[1]]
rmse4<-rmse(iris[,-5],iriscomplete[,-5])



#seed 20% missing values
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete5<-amelia_fit$imputations[[1]]
rmse5<-rmse(iris[,-5],iriscomplete[,-5])

#seed 25% missing values
iris.mis <- prodNA(iris, noNA = 0.25)
summary(iris.mis)
#specify columns and run amelia
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")

#To check a particular column in a data set
amelia_fit$imputations[[5]]$Sepal.Length

iriscomplete6<-amelia_fit$imputations[[1]]
rmse6<-rmse(iris[,-5],iriscomplete[,-5])


rmsev<-c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6)
rmsev
plot(rmsev,type='l',col='blue')

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

