// [[Rcpp::depends(fntl,RcppArmadillo)]]
#include "RcppArmadillo.h"
#include "fntl.h"

// [[Rcpp::export]]
arma::sp_mat outer_sp_ex(Rcpp::NumericMatrix X)
{
    std::function f = [ ](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) -> double {
        double norm2 = Rcpp::sum(Rcpp::pow(x - y, 2));
        return std::sqrt(norm2);
    };

    std::function g = [&](const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) -> bool {
        return f(x,y) < 2.0;
    };

    auto res = fntl::outer_sp(X, f, g);

    // Convert csc_mat to arma::sp_mat
    arma::uvec i = arma::conv_to<arma::uvec>::from(res.i);
    arma::uvec p = arma::conv_to<arma::uvec>::from(res.p);
    arma::vec x = arma::conv_to<arma::vec>::from(res.x);
    return arma::sp_mat(i, p, x, res.m, res.n);
}

