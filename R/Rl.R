#' @title Generate samples from Rayleigh distribution
#' @descriptionuse Generate samples from Rayleigh distribution by using antithetic variables
#' @param n the sample size
#' @param sigma the parameter of Rayleigh distribution
#' @return a random sample of size n
#' @examples
#' n <- 100
#' sample <- Rl(n,2)
#' @export
Rl <- function(n,sigma){
  r1 <- runif(n/2)
  r2 <- 1-r1
  r <- c(r1,r2)
  rd <- numeric(length(r))
  rd <- (-(2*sigma^2)*(log(1-r)))^(1/2)
  return(rd)
}
