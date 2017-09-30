//mapply 
number1<-mapply(sum,numbers,number)
sum1<- function(x,y){
 num2<-c()
 for(i in 1 : length(x)){
 sum<-x[i]+y[i]
 num2[i]<-sum
 }
 print(num2)
 }
 

plotvalues<-microbenchmark::microbenchmark(mapply(sum,numbers,number),sum1(numbers,number),100)
library("ggplot2")
autoplot(plotvalues)
