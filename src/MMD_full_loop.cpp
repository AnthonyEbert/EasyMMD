#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

inline
  double expApprox(double x) {
    int n = 10;

    x = 1 + x / std::pow(2, n);
    for(int i = 0; i < n; ++i){
      x = x * x;
    }
    return x;
  }

inline
  double ident(double x) {
    return -2 * x;
  }



inline
  double maha(arma::mat x, arma::mat y, arma::mat Sinv) {
    arma::mat out_mat = (x - y) * Sinv * (x.t() - y.t());
    double out = arma::accu(out_mat);
    return(out);
  }

// [[Rcpp::export]]
double kernelMatrix_sum_multi(const arma::mat& x, const arma::mat& y, const arma::mat Sinv, const double threshold) {

  int n_x = x.n_rows;
  int n_y = y.n_rows;

  double b;
  double c = threshold * threshold;

  double output_2 = 0;
  //
  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      b = maha(y.row(j), x.row(i), Sinv);
      if(b < c){
        output_2 += std::exp(- 0.5 * b);
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
double kernelMatrix_sum(const arma::vec& x, const arma::vec& y, const float sigma, int approx_exp) {

  int n_x = x.size();
  int n_y = y.size();

  double b;

  double output_2 = 0;

  double (*exp_f)(double);

  if(approx_exp == 0){
    exp_f = &std::exp;
  } else if(approx_exp == 1) {
    exp_f = &expApprox;
  } else if(approx_exp == 2) {
    exp_f = &ident;
  }

  for(int i = 0; i < n_x; ++i){
    for(int j = 0; j < n_y; ++j){
      b = (y[j] - x[i])/sigma;
      output_2 += exp_f(- 0.5 * b * b);

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
double kernelMatrix_threshold_sum(const arma::vec& x_u, const arma::vec& y_u, const float sigma, const float threshold, int approx_exp) {

  const arma::vec y = sort(y_u);
  const arma::vec x = sort(x_u);
  const unsigned int n_x = x.size();
  const unsigned int n_y = y.size();

  double b;
  const float c = threshold;

  double output_2 = 0;
  int start_p = 0;
  int end_p = n_y;
  double (*exp_f)(double);

  if(approx_exp == 0){
    exp_f = &std::exp;
  } else if(approx_exp == 1) {
    exp_f = &expApprox;
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
        output_2 += exp_f(- 0.5 * b * b);

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
