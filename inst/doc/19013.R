## -----------------------------------------------------------------------------
x<-rnorm(10,mean = 0,sd=1)
y<-rnorm(10,mean = 0,sd=1)
plot(x,y)

## -----------------------------------------------------------------------------
knitr::kable(mtcars)

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
n <- 1000 
u <- rexp(n,rate=1)
x <- u^{1/2} # 
hist(x, prob = TRUE, main = expression(f(x)==2*xe^{-x^2})) 
y <- seq(0, 3, .001) 
lines(y, 2*y*exp(1)^{-y^2})

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
n <- 1000 
u <- rexp(n,rate=2)
x <- u^{1/2} # 
hist(x, prob = TRUE, main = expression(f(x)==4*xe^{-2*x^2})) 
y <- seq(0, 3, .001) 
lines(y, 4*y*exp(1)^{-2*y^2})

## -----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
n <- 1000 
u <- rexp(n,rate=1/8)
x <- sqrt(u) # 
hist(x, prob = TRUE, main = expression(f(x)==(1/4)*xe^{-(1/8)*x^2})) 
y <- seq(0, 10, .001) 
lines(y, (1/4)*y*exp(1)^{-(1/8)*y^2})

## -----------------------------------------------------------------------------
n <- 1e4 
X1 <- rnorm(n,mean = 0,sd=1)
X2 <- rnorm(n,mean = 3,sd=1)
r1 <- sample(c(0,1),n,replace=TRUE,prob = c(0.75,0.25)) 
r2 <- sample(c(0,1),n,replace=TRUE,prob = c(0.5,0.5)) 
r3 <- sample(c(0,1),n,replace=TRUE,prob = c(0.3,0.7)) 
r4 <- sample(c(0,1),n,replace=TRUE,prob = c(0.1,0.9)) 

Z1 <- r1*X1+(1-r1)*X2
Z2 <- r2*X1+(1-r2)*X2
Z3 <- r3*X1+(1-r3)*X2
Z4 <- r4*X1+(1-r4)*X2

hist(Z1);hist(Z2);hist(Z3);hist(Z4)


## -----------------------------------------------------------------------------
set.seed(12345)
m<-1e4;t<-runif(m,min = 0,max = pi/3)
theta.hat<-mean(pi*sin(t)/3)#the Monte Carlo estimate
print(c(theta.hat,cos(0)-cos(pi/3)))

## -----------------------------------------------------------------------------
set.seed(1)
m<-1e4;x<-runif(m)
theta1<-mean(exp(-x)/(1+x^2))#the MC estimator of integration
u<-runif(m/2)
g<-.5*exp(-u)/(1+u^2)+.5*exp(-(1-u))/(1+(1-u)^2)
theta2<-mean(g)#the Antithetic variables estimator of integration
print(c(theta1,theta2))
MC1<-sd(exp(-x)/(1+x^2))/sqrt(m)#sd estimate of MC
MC2<-sd(g)/sqrt(m/2)#sd eatimate of Antithetic
cat("the variance of MC estimate=",MC1^2)
cat("the variance of Antithetic estimate=",MC2^2)
cat("the approximate reduction in variance =",(MC1^2-MC2^2)/MC1^2)

## -----------------------------------------------------------------------------
##importance sampling estimate 
M <- 50000
g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}
u <- runif(M) 
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
theta0 <- mean(fg)
se0 <- sd(fg)#sd of importance sampling estimate 
cat("se of importance sampling estimate=",se0)
cat("variance estimate of ISE=",se0^2/M)

##stratified importance sampling estimate 
theta.hat <- se <- numeric(5)
k<-5;m<-M/k

set.seed(524)
g1 <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1/5)
}
u1 <- runif(m,0,(1-exp(-1/5))/(1-exp(-1))) 
x1 <- - log(1 - u1 * (1 - exp(-1)))#obtain x1 belongs to (0,1/5)
fg1 <- g1(x1) / (k*exp(-x1) / (1 - exp(-1)))
theta.hat[1] <- mean(fg1)
se[1] <- sd(fg1)

g2 <- function(x) {
  exp(-x - log(1+x^2)) * (x > 1/5) * (x < 2/5)
}
u2 <- runif(m,(1-exp(-1/5))/(1-exp(-1)),(1-exp(-2/5))/(1-exp(-1))) 
x2 <- - log(1 - u2 * (1 - exp(-1)))#obtain x1 belongs to (1/5,2/5)
fg2 <- g(x2) / (k*exp(-x2) / (1 - exp(-1)))
theta.hat[2] <- mean(fg2)
se[2] <- sd(fg2)

g3 <- function(x) {
  exp(-x - log(1+x^2)) * (x > 2/5) * (x < 3/5)
}
u3 <- runif(m,(1-exp(-2/5))/(1-exp(-1)),(1-exp(-3/5))/(1-exp(-1))) 
x3 <- - log(1 - u3 * (1 - exp(-1)))#obtain x1 belongs to (2/5,3/5)
fg3 <- g(x3) / (k*exp(-x3) / (1 - exp(-1)))
theta.hat[3] <- mean(fg3)
se[3] <- sd(fg3)

g4 <- function(x) {
  exp(-x - log(1+x^2)) * (x > 3/5) * (x < 4/5)
}
u4 <- runif(m,(1-exp(-3/5))/(1-exp(-1)),(1-exp(-4/5))/(1-exp(-1))) 
x4 <- - log(1 - u4 * (1 - exp(-1)))#obtain x1 belongs to (3/5,4/5)
fg4 <- g(x4) / (k*exp(-x4) / (1 - exp(-1)))
theta.hat[4] <- mean(fg4)
se[4] <- sd(fg4)

g5 <- function(x) {
  exp(-x - log(1+x^2)) * (x > 4/5) * (x < 1)
}
u5 <- runif(m,(1-exp(-4/5))/(1-exp(-1)),1) 
x5 <- - log(1 - u5 * (1 - exp(-1)))#obtain x1 belongs to (4/5,1)
fg5 <- g(x5) / (k*exp(-x5) / (1 - exp(-1)))
theta.hat[5] <- mean(fg5)
se[5] <- sd(fg5)

theta0-sum(theta.hat)#we can get the means is almast same between two methods
cat("variance estimate of SISE=",sum(se^2)/m)
se0^2-sum(se^2)#the variance is much smaller
(se0^2-sum(se^2))/se0^2#Reduced percentage of variance

## -----------------------------------------------------------------------------
#t-interval for mean
set.seed(223)
m<-1e4;n<-20;ECP<-numeric(2)
L<-U<-numeric(m)
for (i in 1:m) {
  x<-rchisq(n,2)
  y<-(x-mean(x))^2
  L[i]<-mean(x)-qt(.975, n-1)*sqrt(mean(y)/(n-1))
  U[i]<-mean(x)+qt(.975, n-1)*sqrt(mean(y)/(n-1))
}
c<-ifelse(L<2&2<U,1,0)#if the C.I. (L,U) covers the real mean,we make c equal to 1.
sum(c)/1e4#compute the CP
#the interval for variance
#we can get the code from textbook
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
x <- rchisq(n, df = 2)
(n-1) * var(x) / qchisq(alpha, df = n-1)
} )
sum(UCL > 4)
mean(UCL > 4)

## -----------------------------------------------------------------------------
ECP[1]<-sum(c)/1e4
ECP[2]<-mean(UCL > 4)
data.frame(ECP, row.names = c('t-interval for mean','interval for variance'))

## -----------------------------------------------------------------------------
sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

set.seed(013)
alpha <- .1
n <- 30
m <- 2500
beta <- seq(2, 14, .5)
N <- length(beta)
pwr <- numeric(N)
#critical value for the skewness test 
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))

for (j in 1:N) { #for each beta 
  a <- beta[j]
  sktests <- numeric(m)
  for (i in 1:m) { #for each replicate
    x <- rbeta(n,a,a)
    sktests[i] <- as.integer(abs(sk(x)) >= cv)
  }
  pwr[j] <- mean(sktests)
}

plot(beta, pwr, type = "b",
     xlab = bquote(beta),ylim = c(0,.5),
     main = 'against  symmetric sbeta distribution')


## -----------------------------------------------------------------------------
set.seed(013)

df <- seq(1, 7, .1)
N <- length(df)
pwr <- numeric(N)
#critical value for the skewness test 
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))

for (j in 1:N) { #for each df 
  a <- df[j]
  sktests <- numeric(m)
  for (i in 1:m) { #for each replicate
    x <- rt(n,a)
    sktests[i] <- as.integer(abs(sk(x)) >= cv)
  }
  pwr[j] <- mean(sktests)
}

plot(df, pwr, type = "b",
     xlab = bquote(df),
     main = 'against  heavy-tailed symmetric t distribution')

## -----------------------------------------------------------------------------
n <- 20;set.seed(013)
alpha <- .05
mu0 <- 0
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) {
x <- rt(n, 3)
ttest <- t.test(x, alternative = "two.sided", mu = mu0)
p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))

## -----------------------------------------------------------------------------
n <- 20;set.seed(013)
alpha <- .05
mu0 <- 1
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) {
x <- rchisq(n, 1)
ttest <- t.test(x, alternative = "two.sided", mu = mu0)
p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
n <- 20;set.seed(013)
alpha <- .05
mu0 <- 1
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) {
x <- runif(n,0, 2)
ttest <- t.test(x, alternative = "two.sided", mu = mu0)
p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
n <- 20;set.seed(013)
alpha <- .05
mu0 <- 1
m <- 10000 #number of replicates
p <- numeric(m) #storage for p-values
for (j in 1:m) {
x <- rexp(n, 1)
ttest <- t.test(x, alternative = "two.sided", mu = mu0)
p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
n<-10000
alpha<-0.05
xbar<-0.651;ybar<-0.676
pbar<-(xbar+ybar)/2 #the approximate probability
u<-(ybar-xbar)/sqrt((2*pbar*(1-pbar))/n)#Z-test value
cv <- qnorm(1-alpha/2, 0, 1)#the critical value
print(as.integer(abs(u) >= cv))
#the answer equals 1 means we reject the H0

## -----------------------------------------------------------------------------
library("bootstrap")
data(scor, package = "bootstrap")
pairs(scor,pch = 21)#the scatter plots
cor(scor)#the sample correlation matrix

## -----------------------------------------------------------------------------
#bootstrap estimates of the standard errors for each of the cor
#I don't know how to use loops to represent, so I wrote four functions.
library(boot)
set.seed(287)
#cor12 
b.cor12 <- function(x,i){cor(scor[i,1],scor[i,2])} 
obj<-boot(scor,b.cor12,R=999)
se12<-sd(obj$t)
#cor34
b.cor34 <- function(x,i){cor(scor[i,3],scor[i,4])} 
obj<-boot(scor,b.cor34,R=999)
se34<-sd(obj$t)
#cor35
b.cor35 <- function(x,i){cor(scor[i,3],scor[i,5])} 
obj<-boot(scor,b.cor35,R=999)
se35<-sd(obj$t)
#cor45
b.cor45 <- function(x,i){cor(scor[i,4],scor[i,5])} 
obj<-boot(scor,b.cor45,R=999)
se45<-sd(obj$t)
round(c(SE12.boot=se12,SE34.boot=se34,SE35.boot=se35,SE45.boot=se45),4)

## -----------------------------------------------------------------------------
set.seed(014)
sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

#for normal population
theta<-0;b<-1;n<-2e1;m<-1e3;library(boot);set.seed(12345)
boot.sk <- function(x,i) sk(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
  x <-rnorm(n)
  de <- boot(data=x,statistic=boot.sk, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
CP1<-mean(ci.norm[,1]<=theta & ci.norm[,2]>=theta)
L1<-mean(ci.norm[,1]>=theta);R1<-mean( ci.norm[,2]<=theta)
CP2<-mean(ci.basic[,1]<=theta & ci.basic[,2]>=theta)
L2<-mean(ci.basic[,1]>=theta);R2<-mean( ci.basic[,2]<=theta)
CP3<-mean(ci.perc[,1]<=theta & ci.perc[,2]>=theta)
L3<-mean(ci.perc[,1]>=theta);R3<-mean( ci.perc[,2]<=theta)
CP<-c(CP1,CP2,CP3);L<-c(L1,L2,L3);R<-c(R1,R2,R3)                                     
data.frame("CP"=CP,"Miss on the left"=L,"Miss on the right" =R,
           row.names =c("norm","basic","perc"))

## -----------------------------------------------------------------------------
#for $\chi^2(5)$ 
set.seed(013)
theta<-4/sqrt(10)#the real sk of \chisq(5)
b<-1;n<-20;m<-1e3;library(boot);set.seed(12345)
boot.sk <- function(x,i) sk(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
  x <-rchisq(n,5)
  de <- boot(data=x,statistic=boot.sk, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
CP1<-mean(ci.norm[,1]<=theta & ci.norm[,2]>=theta)
L1<-mean(ci.norm[,1]>=theta);R1<-mean( ci.norm[,2]<=theta)
CP2<-mean(ci.basic[,1]<=theta & ci.basic[,2]>=theta)
L1<-mean(ci.basic[,1]>=theta);R1<-mean( ci.basic[,2]<=theta)
CP3<-mean(ci.perc[,1]<=theta & ci.perc[,2]>=theta)
L1<-mean(ci.perc[,1]>=theta);R1<-mean( ci.perc[,2]<=theta)
CP<-c(CP1,CP2,CP3);L<-c(L1,L2,L3);R<-c(R1,R2,R3)                                     
data.frame("CP"=CP,"Miss on the left"=L,"Miss on the right" =R,
           row.names =c("norm","basic","perc"))

## -----------------------------------------------------------------------------
set.seed(1)
eps <- .Machine$double.eps^0.25#criterion to judge whether function is near to 0
k <- c(4:25,100,500,1000)#k mentioned in the question

S <- function(k,a){
  return((1-pt(sqrt((a^2*k)/(k+1-a^2)),df = k))-(1-pt(sqrt((a^2*(k-1))/(k-a^2)),df=k-1)))
}#S_k(a)-S_{k-1}(a)


Root <- function(k1){
a <- seq(0.1, sqrt(k1)-0.1,length = 3)
y <- c(S(k1,a[1]), S(k1,a[2]), S(k1,a[3]))
while(abs(y[2]) > eps) {
  if (y[1] * y[2] < 0) {
    a[3] <- a[2]
    y[3] <- y[2]
  } else {
    a[1] <- a[2]
    y[1] <- y[2]
  }
  a[2] <- (a[1] + a[3]) / 2
  y[2] <- S(k1,a[2])
}
result <-list(k1,a[2],y[2])
return(result)
}

for(i in k){#print the output of each k
  cat('k:',Root(i)[[1]],'root:',Root(i)[[2]],'value of function:',Root(i)[[3]],'\n')
  
} 

## -----------------------------------------------------------------------------
k <- 4
a <- seq(0.1,sqrt(k)-0.1,0.01)
y0 <- numeric(length(a))
y <- (1-pt(sqrt((a^2*k)/(k+1-a^2)),df = k))-(1-pt(sqrt((a^2*(k-1))/(k-a^2)),df=k-1))
plot(a,y,'l')
lines(a,y0)
cat('The root of curves when k = ',k,'is',Root(k)[[2]],'\n')

k <- 10
a <- seq(0.1,sqrt(k)-0.1,0.01)
y0 <- numeric(length(a))
y <- (1-pt(sqrt((a^2*k)/(k+1-a^2)),df = k))-(1-pt(sqrt((a^2*(k-1))/(k-a^2)),df=k-1))
plot(a,y,'l')
lines(a,y0)
cat('The root of curves when k = ',k,'is',Root(k)[[2]],'\n')



## -----------------------------------------------------------------------------
data(scor, package = "bootstrap")
n <- nrow(scor)
#jackknife
cov1<-cov(scor)
ev1<-eigen(cov1)
lambda1<-sort(ev1$values)[5]
theta.hat<-lambda1/sum(ev1$values)
theta.jack <- numeric(n)
for (i in 1:n){
  cov2<-cov(scor[-i,])
  ev2<-eigen(cov2)
  lambda2<-sort(ev2$values)[5]
  theta.jack[i]<-lambda2/sum(ev2$values)
}
bias_jack <- (n-1)*(mean(theta.jack) - theta.hat)
se_jack<-sqrt((n-1)*mean((theta.jack - mean(theta.jack))^2))
data.frame("bias_jack"=bias_jack,"se_jack"=se_jack)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)

L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)

L4 <- lm(magnetic ~ chemical + I(chemical^2) + I(chemical^3))#cubic polynimial
plot(chemical, magnetic, main="Cubic polynomial", pch=16)
yhat4 <- L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2 + L4$coef[4] * a^3
lines(a, yhat4, lwd=2)

n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)
# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  
  J4 <- lm(y ~ x + I(x^2) +I(x^3))#cubic polynomial
  yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] +
    J4$coef[3] * chemical[k]^2 + J4$coef[4] * chemical[k]^3
  e4[k] <- magnetic[k] - yhat4
}
data.frame("MSE1"=mean(e1^2),"MSE2"=mean(e2^2),"MSE3"=mean(e3^2),"MSE4"=mean(e4^2),row.names = "MSE from the n-fold cross validation.")
data.frame("R^2 of L2"=summary(J2)$adj.r.squared,"R^2 of L4"=summary(J4)$adj.r.squared,row.names = "maximum adjusted R^2")

## -----------------------------------------------------------------------------
x <- rnorm(20, 0, sd = 1)
y <- rnorm(30, 0, sd = 2)
z <- c(x, y)
R <- 999;K <-1:50;n<-length(x);set.seed(0374)
reps <- numeric(R)
#define the count five test function
count5test <- function(x, y) { 
  X <- x - mean(x) 
  Y <- y - mean(y) 
  outx <- sum(X > max(Y)) + sum(X < min(Y)) 
  outy <- sum(Y > max(X)) + sum(Y < min(X)) # return 1 (reject) or 0 (do not reject H0) 
  #return(as.integer(max(c(outx, outy)) > 5)) 
  return(max(c(outx, outy)))
  }
t0 <- count5test(x,y)
# Implement a permutation test
for (i in 1:R){ 
  k <- sample(K, size = n, replace = FALSE) 
  x1 <- z[k]
  y1 <- z[-k] 
  reps[i] <- count5test(x1,y1) 
} 
p <- mean(abs(c(t0, reps)) >= abs(t0)) 
print(p)


## -----------------------------------------------------------------------------
#Empirical distance covariance
dCov <- function(x, y) {
  x <- as.matrix(x); y <- as.matrix(y)
  n <- nrow(x); m <- nrow(y)
  if (n != m || n < 2) stop("Sample sizes must agree")
  if (! (all(is.finite(c(x, y)))))
    stop("Data contains missing or infinite values")
  Akl <- function(x) {
    d <- as.matrix(dist(x))
    m <- rowMeans(d); M <- mean(d)
    a <- sweep(d, 1, m); b <- sweep(a, 2, m)
    b + M
  }
  A <- Akl(x); B <- Akl(y)
  sqrt(mean(A * B))
}
ndCov2 <- function(z, ix, dims) {
  #dims contains dimensions of x and y
  p <- dims[1]
  q <- dims[2]
  d <- p + q
  x <- z[ , 1:p] #leave x as is
  y <- z[ix, -(1:p)] #permute rows of y
  return(nrow(z) * dCov(x, y)^2)
}

library("boot")
#the function to generate random number of Model 1
rd1 <- function(n){
  x <- mvrnorm(n,c(0,0),matrix(c(1,0,0,1),2))
  e <- mvrnorm(n,c(0,0),matrix(c(1,0,0,1),2))
  y <- 0.25*x+e
  z <- data.frame(x,y)
  return(z)
}
#the function to generate random number of Model 2
rd2 <- function(n){
  x <- mvrnorm(n,c(0,0),matrix(c(1,0,0,1),2))
  e <- mvrnorm(n,c(0,0),matrix(c(1,0,0,1),2))
  y <- 0.25*x*e
  z <- data.frame(x,y)
  return(z)
}
#obtain p-value of disrance correlation
p.dcf <- function(z){
  boot.obj <- boot(data = z, statistic = ndCov2, R = 100,
                 sim = "permutation", dims = c(2, 2))
  tb <- c(boot.obj$t0, boot.obj$t)
  p.cov <- mean(tb>=tb[1])
  return(p.cov)
}
#obtain p-value of ball covariance test
p.bcf <- function(z){
  p.bc <- bcov.test(z[,1:2],z[,3:4],R=100,seed = i*123)$p.value
  return(p.bc)
}

library(MASS)
library(Ball)
m <- 50;p.values <- matrix(NA,m,3)
for (i in 1:m) {
  p.values[i,1] <-p.dcf(rd1(10))
  p.values[i,2] <-p.dcf(rd1(50))
  p.values[i,3] <-p.dcf(rd1(100))
}
pow1_dc <- colMeans(p.values<0.05)#power of DC in model 1
for (i in 1:m) {
  p.values[i,1] <-p.bcf(rd1(10))
  p.values[i,2] <-p.bcf(rd1(50))
  p.values[i,3] <-p.bcf(rd1(100))
}
pow1_bc <- colMeans(p.values<0.05)#power of BC in model 1
for (i in 1:m) {
  p.values[i,1] <-p.dcf(rd2(10))
  p.values[i,2] <-p.dcf(rd2(50))
  p.values[i,3] <-p.dcf(rd2(100))
}
pow2_dc <- colMeans(p.values<0.05)#power of DC in model 2
for (i in 1:m) {
  p.values[i,1] <-p.bcf(rd2(10))
  p.values[i,2] <-p.bcf(rd2(50))
  p.values[i,3] <-p.bcf(rd2(100))
}
pow2_bc <- colMeans(p.values<0.05)#power of BC in model 2
N <- c(10,50,100)
data.frame(N,pow1_dc,pow1_bc,pow2_dc,pow2_bc)
plot(N,pow1_dc,type = "l",col = "blue",ylab = "power",ylim = c(0,1),main = "power comparison in model 1: Y=X/4+e")
lines(N,pow1_bc,col = "red")
legend("bottomright",legend=c("ball covariance","distance correlation"),
       col=c("red","blue"),lty=1,lwd=1)  

plot(N,pow2_dc,type = "l",col = "blue",ylab = "power",ylim = c(0,1),main = "power comparison in model 2: Y=X/4*e")
lines(N,pow2_bc,col = "red")
legend("bottomright",legend=c("ball covariance","distance correlation"),
       col=c("red","blue"),lty=1,lwd=1)  


## -----------------------------------------------------------------------------
dt<- function(x) 0.5*exp(1)^(-abs(x))

rw.Metropolis <- function( sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dt(y) / dt(x[i-1])))
      x[i] <- y else {
        x[i] <- x[i-1]
        k <- k + 1
      }
  }
  return(list(x=x, k=k))
}

N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis( sigma[1], x0, N)
rw2 <- rw.Metropolis( sigma[2], x0, N)
rw3 <- rw.Metropolis( sigma[3], x0, N)
rw4 <- rw.Metropolis( sigma[4], x0, N)


q1 <- log10(1-2*abs(0.025-0.5))#the 0.025 quantile of Laplace distribution
q2 <- -log10(1-2*abs(0.975-0.5))#the 0.975 quantile of Laplace distribution 
qs <- c(q1,q2)
rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
for (j in 1:4) {
  plot(rw[,j], type="l",
       xlab=bquote(sigma == .(round(sigma[j],3))),
       ylab="X", ylim=range(rw[,j]))
  abline(h=qs)
}

ac <- c(rw1$k, rw2$k, rw3$k, rw4$k)
ac.rate <- (N-ac)/N
rbind(sigma, ac.rate)#the acceptance rates of each chain


## -----------------------------------------------------------------------------
log(exp(5))
exp(log(5))
log(exp(5))-exp(log(5))
isTRUE(all.equal(log(exp(5)),exp(log(5))))

## -----------------------------------------------------------------------------
ks  <- c(4:25,100)
n <- length(ks)
A1 <- A2 <- numeric(n)
cf <- function(k,a){
  return(sqrt(a^2*k/(k+1-a^2)))
}

f1 <- function(u){
  (1+u^2/(k-1))^(-k/2)
}
f2 <- function(u){which
  (1+u^2/k)^(-(k+1)/2)
}
g1 <- function(a){
  2*gamma(k/2)/(sqrt(pi*(k-1))*gamma((k-1)/2))*integrate(f1,0,cf(k-1,a))$value-2*gamma((k+1)/2)/(sqrt(pi*k)*gamma(k/2))*integrate(f2,0,cf(k,a))$value
}#soluton of question 11.5 
g2 <- function(a){
  1-pt(cf(k-1,a),k-1)-1+pt(cf(k,a),k)
} #solution of question 11.4

for (i in 1:n) {
  k <- ks[i]
  A1[i] <- uniroot(g1,c(0.05,sqrt(k)/2+1))$root
  A2[i] <- uniroot(g2,c(0.01,sqrt(k)/2+1))$root
}
A1
A2

## -----------------------------------------------------------------------------

library(rootSolve)
set.seed(013)
N <- 2e2
nA <- 28  
nB <- 24
nO <- 41
nAB<-70  
E <- numeric(N)
L <- c(.1,.1)#setting Initial value
tol <- .Machine$double.eps^0.5
L.old <- L+1
for(j in 1:N){
  E[j]<-2*L[1]*nA*log(L[1])/(2-L[1]-2*L[2])
  +2*L[2]*nB*log(L[2])/(2-L[2]-2*L[1])
  +2*nO*log(1-L[1]-L[2])
  +nA*(2-2*L[1]-2*L[2])*log(2*L[1]*(1-L[1]-L[2]))/(2-L[1]-2*L[2])
  +nB*(2-2*L[1]-2*L[2])*log(2*L[2]*(1-L[1]-L[2]))/(2-L[2]-2*L[1])
  +nAB*log(2*L[1]*L[2])#E-step expression
  model<-function(x){
    #use R.function multiroot to  solve the max for M-step
    F1<-2*L[1]*nA/((2-L[1]-2*L[2])*x[1])-2*nO/(1-x[1]-x[2])+nA*(2-2*L[1]-2*L[2])*(1-2*x[1]-x[2])/((2-L[1]-2*L[2])*x[1]*(1-x[1]-x[2]))-nB*(2-2*L[1]-2*L[2])/((2-L[2]-2*L[1])*(1-x[1]-x[2]))+nAB/x[1]
    F2<-2*L[2]*nB/((2-L[2]-2*L[1])*x[2])-2*nO/(1-x[1]-x[2])-nA*(2-2*L[1]-2*L[2])/((2-L[1]-2*L[2])*(1-x[1]-x[2]))+nB*(2-2*L[1]-2*L[2])*(1-2*x[2]-x[1])/((2-L[2]-2*L[1])*x[2]*(1-x[1]-x[2]))+nAB/x[2]
    c(F1=F1,F2=F2)
  }
  tt<-multiroot(f=model,star=c(.1,.1))
  L<-tt$root
  if (sum(abs(L-L.old)/L.old)<tol) break
  L.old<-L
}
data.frame("p-hat" = L.old[1],"q-hat" = L.old[2])

## -----------------------------------------------------------------------------
data("mtcars")
attach(mtcars)
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
#for loop
out_loop_1 <- vector("list", length(formulas))
for (i in seq_along(formulas)) {
  out_loop_1[[i]] <- lm(formulas[[i]])
}
out_loop_1
#lapply
out_lap_1 <- lapply(formulas, lm)
out_lap_1

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})
#for loop
out_loop_2 <- vector("list", length(bootstraps))
for (i in seq_along(bootstraps)) {
  f <- function(x){lm(mpg~disp, data=x)}
  out_loop_2[[i]] <- f(bootstraps[[i]])
}
out_loop_2
#lapply
out_lap_2 <- lapply(bootstraps, lm, formula = mpg ~ disp)
out_lap_2

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
#for exercises 1
unlist(lapply(out_loop_1, rsq))
unlist(lapply(out_lap_1, rsq))
#for exercise 2
unlist(lapply(out_loop_2, rsq))
unlist(lapply(out_lap_2, rsq))



## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(trials, function(x) x$p.value)
#get rid of the anonymous function
sapply(trials, "[[" , 3)

## -----------------------------------------------------------------------------
#definite mcsapply
mcsapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE) 
{
  FUN <- match.fun(FUN)
  dots <- list(...)
  answer <- .Internal(mapply(FUN, dots, MoreArgs))
  if (USE.NAMES && length(dots)) {
    if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]])) 
      names(answer) <- dots[[1L]]
    else if (!is.null(names1)) 
      names(answer) <- names1
  }
  if (!isFALSE(SIMPLIFY) && length(answer)) 
    simplify2array(answer, higher = (SIMPLIFY == "array"))
  else answer
}
#example
xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)
sapply(seq_along(xs), function(i) {
  weighted.mean(xs[[i]], ws[[i]])
})
mcsapply(weighted.mean, xs, ws)

## -----------------------------------------------------------------------------
#setting
N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25

## -----------------------------------------------------------------------------
#for R
dt<- function(x) 0.5*exp(1)^(-abs(x))

rw.Metropolis <- function( sigma, x0, N) {
  x <- matrix(0,N,2)
  x[1,1] <- x0
  u <- runif(N)
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dt(y) / dt(x[i-1])))
      {
        x[i,1] <- y 
        x[i,2] <- 1
      }
    else 
      {
        x[i,1] <- x[i-1]
        x[i,2] <- 0
      }
  }
  return(x)
}


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

## -----------------------------------------------------------------------------
#random numbers from R
rw1.R <- rw.Metropolis( sigma[1], x0, N)
rw2.R <- rw.Metropolis( sigma[2], x0, N)
rw3.R <- rw.Metropolis( sigma[3], x0, N)
rw4.R <- rw.Metropolis( sigma[4], x0, N)
#random numbers from Rcpp
rw1.C <- randomwalkC( sigma[1], x0, N)
rw2.C <- randomwalkC( sigma[2], x0, N)
rw3.C <- randomwalkC( sigma[3], x0, N)
rw4.C <- randomwalkC( sigma[4], x0, N)
#effective rw
r1.R <- rw1.R[rw1.R[,2]==1,1]; r1.C <- rw1.C[rw1.C[,2]==1,1]
r2.R <- rw2.R[rw2.R[,2]==1,1]; r2.C <- rw2.C[rw2.C[,2]==1,1]
r3.R <- rw3.R[rw3.R[,2]==1,1]; r3.C <- rw3.C[rw3.C[,2]==1,1]
r4.R <- rw4.R[rw4.R[,2]==1,1]; r4.C <- rw4.C[rw4.C[,2]==1,1]
#qqplot
qqplot(r1.R, r1.C, xlab = "r.R",ylab = "r.C",main = expression("Q-Q plot of sigma=0.05" ))
qqplot(r2.R, r2.C, xlab = "r.R",ylab = "r.C",main = expression("Q-Q plot of sigma=0.5"  ))
qqplot(r3.R, r3.C, xlab = "r.R",ylab = "r.C",main = expression("Q-Q plot of sigma=2"    ))
qqplot(r4.R, r4.C, xlab = "r.R",ylab = "r.C",main = expression("Q-Q plot of sigma=16"   ))

## -----------------------------------------------------------------------------
library(microbenchmark) 
ts <- microbenchmark(rw1.R <- rw.Metropolis(sigma[1], x0, N) ,rw2.R <- rw.Metropolis(sigma[2], x0, N) ,rw3.R <- rw.Metropolis(sigma[3], x0, N) ,rw4.R <- rw.Metropolis(sigma[4], x0, N) ,rw1.C <- randomwalkC(sigma[1], x0, N) ,rw2.C <- randomwalkC(sigma[2], x0, N) ,rw3.C <- randomwalkC(sigma[3], x0, N)  ,rw4.C <- randomwalkC(sigma[4], x0, N))
summary(ts)[,c(1,3,5,6)]

