// [[Rcpp::depends(fntl,RcppArmadillo)]]
#include "RcppArmadillo.h"
#include "fntl.h"

// [[Rcpp::export]]
arma::sp_mat outer_sp_ex(Rcpp::NumericMatrix X)
{
    std::function f = [](
        const Rcpp::NumericVector& x,
        const Rcpp::NumericVector& y)
        -> std::pair<double,bool>
    {
        double norm = std::sqrt(Rcpp::sum(Rcpp::pow(x - y, 2)));
        bool ind = norm < 2.0;
        return std::pair<double,bool>(norm, ind);
    };

    auto res = fntl::outer_sp(X, f);

    // Convert csc_mat to arma::sp_mat
    arma::uvec i = arma::conv_to<arma::uvec>::from(res.i);
    arma::uvec p = arma::conv_to<arma::uvec>::from(res.p);
    arma::vec x = arma::conv_to<arma::vec>::from(res.x);
    return arma::sp_mat(i, p, x, res.m, res.n);
}

