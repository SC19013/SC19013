## -----------------------------------------------------------------------------
Rl <- function(n,sigma){
  u <- runif(floor(n/2))
  v <- 1-u
  u <- c(u,v)
  rd <- numeric(length(u))
  rd <- (-(2*sigma^2)*(log(1-u)))^(1/2)
  return(rd)
}

## -----------------------------------------------------------------------------
Cpnor <- function(theta,n,m){
  sk <- function(x) {
     xbar <- mean(x)
     m3 <- mean((x - xbar)^3)
     m2 <- mean((x - xbar)^2)
     return( m3 / m2^1.5 )
   }
  library(boot)
  boot.sk <- function(x,i) sk(x[i])
  ci.norm <- matrix(NA,m,2)
  for(i in 1:m){
  x <-rnorm(n)
  de <- boot(data=x,statistic=boot.sk, R = 999)
  ci <- boot.ci(de,type=c("norm"))
  ci.norm[i,]<-ci$norm[2:3]
}
CP<-mean(ci.norm[,1]<=theta & ci.norm[,2]>=theta)
return(CP)
}

