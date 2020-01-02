#' @title the empirical coverage rates for the standard normal bootstrap confidence interval
#' @description  a Monte Carlo study to estimate the coverage probabilities of the standard normal bootstrap confidence interval
#' @param theta the skewness of normal distribution
#' @param n the size of each sample you want
#' @param m the times of iteration you want
#' @return a coverage probabilities between 0 and 1
#' @import boot
#' @examples
#' n <- 20; m <- 1000
#' Cpnor(0,n,m)
#' @export
Cpnor <- function(theta,n,m){
  sk <- function(x) {
     xbar <- mean(x)
     m3 <- mean((x - xbar)^3)
     m2 <- mean((x - xbar)^2)
     return( m3 / m2^1.5 )
   }
  boot.sk <- function(x,i) sk(x[i])
  ci.norm <- matrix(NA,m,2)
  for(i in 1:m){
  x <-rnorm(n)
  de <- boot(data=x,statistic=boot.sk, R = 200)
  ci <- boot.ci(de,type=c("norm"))
  ci.norm[i,]<-ci$norm[2:3]
}
CP<-mean(ci.norm[,1]<=theta & ci.norm[,2]>=theta)
return(CP)
}
