#' @title Generate samples from Rayleigh distribution
#' @descriptionuse Generate samples from Rayleigh distribution by using antithetic variables
#' @param n the sample size
#' @param sigma the parameter of Rayleigh distribution
#' @return a random sample of size n
#' @examples
#' n <- 100
#' sample <- Rayleigh(n,2)
#' head(sample,10)
#' @export
Rl <- function(n,sigma){
  u <- runif(floor(n/2))
  v <- 1-u
  u <- c(u,v)
  rd <- numeric(length(u))
  rd <- (-(2*sigma^2)*(log(1-u)))^(1/2)
  return(rd)
}