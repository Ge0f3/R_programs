numbers<-1:100
numbers<-mapply(log10,numbers)
number<-1:100
number1<-mapply(sum,numbers,number)
sum1<- function(x,y){
  num2<-c()
  for(i in 1 : length(x)){
    sum<-x[i]+y[i]
    num2[i]<-sum
  }
  print(num2)
}


plotvalues<-microbenchmark::microbenchmark(mapply(sum,numbers,number),sum1(numbers,number))
library("ggplot2")
autoplot(plotvalues)