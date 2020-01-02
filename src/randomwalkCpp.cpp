#include <Rcpp.h>
using namespace Rcpp;

//' @title Random Walk using Rcpp
//' @description A Random Walk sampler using Rcpp
//' @param sigma the standard deviation of the normal random increment with a zero mean 
//' @param x0 the initial iteration point
//' @param N the designated number of random numbers (including the initial point x0)
//' @return the sample
//' @examples
//' \dontrun{
//' laplace<-function(x) return(1/2*exp(-abs(x)))
//' lC <- randomwalkC(1,25,1000);
//' }
//' @export
// [[Rcpp::export]]
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
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
