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

      if(j % 2048 == 0)
      {
        Rcpp::checkUserInterrupt();
      }
    }
    if(i % 2048 == 0)
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return(output_2);
}


// [[Rcpp::export]]
double kernelMatrix_threshold_sum(NumericVector x_obs, NumericVector x_sim, float sigma, float threshold) {

  vec y = as<vec>(x_obs);
  vec x = as<vec>(x_sim);
  int n_x = x.size();
  int n_y = y.size();

  double a;
  double b;

  double output_2 = 0;

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      b = abs(x[i] - y[j]);
      if(b < threshold/sigma){
        a = std::pow(b, 2);
        output_2 += exp(- sigma * a);
      }

      if(j % 2048 == 0)
      {
        Rcpp::checkUserInterrupt();
      }
    }
    if(i % 2048 == 0)
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return(output_2);
}

// [[Rcpp::export]]
arma::mat kernelMatrix_threshold(NumericVector x_obs, NumericVector x_sim, float sigma, float threshold) {

  vec y = as<vec>(x_obs);
  vec x = as<vec>(x_sim);
  int n_x = x.size();
  int n_y = y.size();

  double a;
  double b;

  mat output_2(n_x, n_y, fill::zeros);

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      b = abs(x[i] - y[j]);
      if(b < threshold/sigma){
        a = std::pow(b, 2);
        output_2(i,j) = exp(- sigma * a);
      }

      if(j % 2048 == 0)
      {
        Rcpp::checkUserInterrupt();
      }
    }
    if(i % 2048 == 0)
    {
      Rcpp::checkUserInterrupt();
    }
  }

  return(output_2);
}
