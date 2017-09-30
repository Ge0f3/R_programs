num1=list(1,list(2,3),4,list(5,list(6,7)))
logx<-rapply(num1,function(x){log10(x)})
logresult <- function(x){
  result <- c()
  for(i in 1: length(x)){
    x1 <- x[[i]]
    for(j in 1:length(x1)){
      x2 <- x1[[j]]
      if(is.numeric(x2)){
        result <- c(result, log(x2))
      }
      
    }
  }
  print(result)
}
 
  
  plotvalues<-microbenchmark::microbenchmark(rapply(num1,function(x){log10(x)}),logresult(num1))
  library("ggplot2")
  autoplot(plotvalues)