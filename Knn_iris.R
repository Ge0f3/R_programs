library(ggvis)
library(class)
library(class)
library(gmodels)
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
iris.training <- iris[ind==1, 1:4] 
iris.test <- iris[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5] 
iris.testLabels <- iris[ind==2, 5]
#creating a KNN model 
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
iris_pred
iris.testLabels
comparision<-matrix(c(iris_pred, iris.testLabels), nrow=length(iris_pred))
comparision<-cbind(iris_pred, iris.testLabels)
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)

#comparision the trained vs test data and 
for(i in 1:length(comparision[,1])){
   print(comparision[i,1],comparision[i,2])}