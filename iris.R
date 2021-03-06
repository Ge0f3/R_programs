library(mice)
library(missForest)
library(hydroGOF)
iris<-iris
str(iris)
#genarting 2% missing values
irisNA1<-prodNA(iris,noNA=0.02)
#looking into some values of iris dataset
head(irisNA1)

md.pattern(irisNA1)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA1,m=1,maxit = 50,method = 'pmm',seed = 500)
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

#generating 5% missing values
irisNA2<-prodNA(iris,noNA=0.05)
#looking into some values of iris dataset
head(irisNA2)

md.pattern(irisNA2)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA2,m=1,maxit = 50,method = 'pmm',seed = 500)
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
rmse2<-rmse(iris[,-5],iriscomplete[,-5])
rmse2

#generatin 10% missing values
irisNA3<-prodNA(iris,noNA=0.1)
#looking into some values of iris dataset
head(irisNA3)

md.pattern(irisNA3)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA3,m=1,maxit = 50,method = 'pmm',seed = 500)
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
rmse3<-rmse(iris[,-5],iriscomplete[,-5])
rmse3

#genarating missing values for 15%
irisNA4<-prodNA(iris,noNA=0.15)
#looking into some values of iris dataset
head(irisNA4)

md.pattern(irisNA4)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA1,m=1,maxit = 50,method = 'pmm',seed = 500)
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
rmse4<-rmse(iris[,-5],iriscomplete[,-5])
rmse4

#generating 20% missing values 
irisNA5<-prodNA(iris,noNA=0.2)
#looking into some values of iris dataset
head(irisNA5)

md.pattern(irisNA5)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA5,m=1,maxit = 50,method = 'pmm',seed = 500)
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
rmse5<-rmse(iris[,-5],iriscomplete[,-5])
rmse5


#generatin 25% missing values
irisNA6<-prodNA(iris,noNA=0.25)
#looking into some values of iris dataset
head(irisNA6)

md.pattern(irisNA6)
#imputing the values and predicting the NA values 
iris_imputed<-mice(irisNA6,m=1,maxit = 50,method = 'pmm',seed = 500)
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
rmse6<-rmse(iris[,-5],iriscomplete[,-5])
rmse6

rmsev<-c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6)
rmsev
plot(rmsev,type='l',col='blue')
