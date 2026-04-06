
library(lpSolve)

objective<-c(3,5)
constraints<-matrix(c(2,3,1,1),nrow=2,byrow=TRUE)
directions<-c("<=","<=")
rhs<-c(12,5)

result<-lp("max",objective,constraints,directions,rhs)
print(result)
cat("optimal value of z:",result$objval,"\n")
cat("optimal values of x and y:",result$solution,"\n")