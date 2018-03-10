#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace std;
using namespace Rcpp;
using namespace arma;

inline
  float exp512(float x) {
    x = 1 + x / 512;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    x = x * x;
    return x;
  }

inline
  float ident(float x) {
    return -x;
  }

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
float kernelMatrix_threshold_sums(const arma::fvec& x_u, const arma::fvec& y_u, const float sigma, const float threshold, bool approx_exp) {

  const arma::fvec y = sort(y_u);
  const arma::fvec x = sort(x_u);
  const unsigned int n_x = x.size();
  const unsigned int n_y = y.size();

  float b;
  const float c = threshold;

  float output_2 = 0;
  int start_p = 0;
  int end_p = n_y;
  float (*exp_f)(float);

  if(approx_exp == 0){
    exp_f = &exp;
  } else if(approx_exp == 1) {
    exp_f = &exp512;
  }

  // int n_start = 0;
  // int n_end = 0;
  // int n_elem = 0;

  for(int i = 0; i < n_x; ++i){
    for(int j = start_p; j < n_y; ++j){
      b = (y[j] - x[i])/sigma;
      if(abs(b) >= c){
        if(b <= - c){
          //n_start += 1;
          start_p = j;
        } else {
          //n_end += 1;
          break;
        }
      } else {
        //a = b * b;
        output_2 += exp_f(- b * b);

        // if(approx_exp){
        //   output_2 += exp512(- b * b);
        // } else {
        //   output_2 += exp(- b * b);
        // }
        //n_elem += 1;
      }

      //   if(j % 2048 == 0)
      //   {
      //     Rcpp::checkUserInterrupt();
      //   }
    }
    // if(i % 2048 == 0)
    // {
    //   Rcpp::checkUserInterrupt();
    // }
  }

  // Rcout << "n_start " << n_start << std::endl;
  // Rcout << "n_end " << n_end << std::endl;
  // Rcout << "n_elem " << n_elem << std::endl;

  return(output_2);
}
