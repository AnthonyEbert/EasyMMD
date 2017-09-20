#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double kernelMatrix_sum(NumericVector x_obs, NumericVector x_sim, float sigma) {

  vec y = as<vec>(x_obs);
  vec x = as<vec>(x_sim);
  int n_x = x.size();
  int n_y = y.size();

  double a;

  double output_2 = 0;

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      a = std::pow(x[i] - y[j], 2);
      output_2 += exp(- sigma * a);
    }
  }

  return(output_2);
}



// [[Rcpp::export]]
double kernelMatrix_poly_sum(NumericVector x_obs, NumericVector x_sim, float sigma) {

  vec y = as<vec>(x_obs);
  vec x = as<vec>(x_sim);
  int n_x = x.size();
  int n_y = y.size();

  double a;

  double output_2 = 0;

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      //a = std::pow(x[i] - y[j], 2);
      a = (x[i] * y[i]);
      output_2 += a;
    }
  }

  return(output_2);
}

