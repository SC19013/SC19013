## -----------------------------------------------------------------------------
Rl <- function(n,sigma){
   u <- rexp(n,rate=1/(2*sigma^2))
   x <- u^{1/2}
   return(x) 
}

## -----------------------------------------------------------------------------
x <- Rl(1000,1) 
hist(x, prob = TRUE, main = expression(f(x)==xe^{-(x^2)/2})) 
y <- seq(0, 3, .001) 
lines(y, y*exp(1)^{-(y^2)/2})

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

## -----------------------------------------------------------------------------
theta <- 0
n <- 20; m <- 1000
Cpnor(theta, n, m)

## -----------------------------------------------------------------------------
#for Rcpp
library(Rcpp)
sourceCpp(code = '
          #include <Rcpp.h>
          using namespace Rcpp;
          //[[Rcpp::export]]
          NumericMatrix randomwalkC(double sigma,double x0,int N){
          NumericMatrix x(N,2);
          x(0,0) = x0; 
          NumericVector u = runif(N); 
          for (int i = 1; i < N ;i++){ 
            double y = as<double>(rnorm(1, x(i-1,0), sigma));
            double t = exp(-abs(y))/exp(-abs(x(i-1,0)));
            if (u[i] <= t){
              x(i,0) = x(i-1,0); 
              x(i,1) = 1;
            }
            else{ 
              x(i,0) = y;
              x(i,1) = 0;} 
          };
          return x;
          }')

