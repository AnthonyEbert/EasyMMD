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
  int n_x = x.size();
  int n_y = y.size();

  float a;
  float output;

  mat outputMatrix(n_x, n_y, fill::zeros);

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      a = std::pow(x[i] - y[j], 2);
      outputMatrix(i,j) = exp(- sigma * a);
    }
  }

  output = accu(outputMatrix);

  return(output);
}

