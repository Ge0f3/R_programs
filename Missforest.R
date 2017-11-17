install.packages("missForest")
#missForest
library(missForest)

#load data
data("iris")

#seed 2% missing values
iris.mis <- prodNA(iris, noNA = 0.02)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete1<-iris.imp$ximp
rmse1<-rmse(iris[,-5],iriscomplete[,-5])
rmse1

#seed 5% missing values
iris.mis <- prodNA(iris, noNA = 0.05)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete2<-iris.imp$ximp
rmse2<-rmse(iris[,-5],iriscomplete[,-5])
rmse2

#seed 10% missing values
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete3<-iris.imp$ximp
rmse3<-rmse(iris[,-5],iriscomplete[,-5])
rmse3

#seed 15% missing values
iris.mis <- prodNA(iris, noNA = 0.15)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete4<-iris.imp$ximp
rmse4<-rmse(iris[,-5],iriscomplete[,-5])
rmse4

#seed 20% missing values
iris.mis <- prodNA(iris, noNA = 0.2)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete5<-iris.imp$ximp
rmse5<-rmse(iris[,-5],iriscomplete[,-5])
rmse5

#seed 25% missing values
iris.mis <- prodNA(iris, noNA = 0.25)
summary(iris.mis)

#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror

iriscomplete6<-iris.imp$ximp
rmse6<-rmse(iris[,-5],iriscomplete[,-5])
rmse6


rmsev<-c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6)
rmsev
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
