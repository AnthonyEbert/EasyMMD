#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
float kernelMatrix_cpp(NumericVector x_obs, NumericVector x_sim, float sigma) {

  vec y = as<vec>(x_obs);
  vec x = as<vec>(x_sim);
  int n = x.size();

  float a;
  float output;

  mat outputMatrix(n, n, fill::zeros);

  for(int i = 0; i < n; ++i){
    for(int j = 0; j < n; ++j){
      a = std::pow(x[i] - y[j], 2);
      outputMatrix(i,j) = exp(- sigma * a);
    }
  }

  output = accu(outputMatrix);

  return(output);
}

