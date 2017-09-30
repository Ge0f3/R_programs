salary<-mapply(rep,1000,15)
name<-LETTERS[1:16]
employee<-data.frame(name=name,salary=salary,dept=c("Dep1","Dep2","Dep3","Dep4"))
tapply(employee$salary,employee$dept,max)
findmaxsal <- function(x){
    depts <- split( x , f = employee$dept)
     for(i in 1:length(depts)){
         max <- depts[[i]]
         print(max(max[[2]]))
     }
 }
plotvalues<-microbenchmark::microbenchmark(tapply(employee$salary,employee$dept,max),findmaxsal(employee),100)
library("ggplot2")
autoplot(plotvalues) 