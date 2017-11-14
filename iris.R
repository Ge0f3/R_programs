library(mice)
library(missForest)
library(hydroGOF)
iris<-iris
str(iris)
#generatin 10% missing values
irisNA<-prodNA(iris,noNA=0.1)
#looking into some values of iris dataset
head(irisNA)

md.pattern(irisNA)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA,m=1,maxit = 50,method = 'pmm',seed = 500)
summary(iris_imputed)

#check imputed values for sepal length
iris_imputed$imp$Sepal.Length
#check imputed values for sepal width
iris_imputed$imp$Sepal.Width
#check imputed values for petal length
iris_imputed$imp$Petal.Length
#check imputed values for petal width
iris_imputed$imp$Petal.Width
#no of imputed values for petal widh
count(iris_imputed$imp$Petal.Width)

iriscomplete<-complete(iris_imputed,1)
iriscomplete
rmse1<-rmse(iris[,-5],iriscomplete[,-5])
rmse1
