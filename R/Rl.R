#' @title Generate samples from Rayleigh distribution
#' @descriptionuse Generate samples from Rayleigh distribution by using Transformation methods
#' @param n the sample size
#' @param sigma the parameter of Rayleigh distribution
#' @return a random sample of size n
#' @examples
#' n <- 100
#' sample <- Rl(n,2)
#' @export
Rl <- function(n,sigma){
  u <- rexp(n,rate=1/(2*sigma^2))
  x <- u^{1/2}
  return(x) 
}

