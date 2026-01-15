// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector coord_apply1_ex(Rcpp::NumericVector x)
{
    std::function f = [&](unsigned int i) -> double
    {
        return std::max(std::min(x(i), 1.0), -1.0);
    };

    auto out = fntl::coord_apply(x.size(), f);
    return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix coord_apply2_ex(Rcpp::NumericMatrix X)
{
    std::function f = [&](unsigned int i, unsigned int j) -> double
    {
        return std::max(std::min(X(i,j), 1.0), -1.0);
    };

    auto out = fntl::coord_apply(X.nrow(), X.ncol(), f);
    return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List coord_apply_sp_ex(Rcpp::NumericMatrix X)
{
    std::function f = [&](unsigned int i, unsigned int j)
        -> std::pair<double, bool>
    {
        double val = std::max(std::min(X(i,j), 1.0), -1.0);
        double ind = fabs(val) < 1;
        return std::pair<double, bool>(val, ind);
    };

    auto out = fntl::coord_apply_sp(X.nrow(), X.ncol(), f);
    return Rcpp::wrap(out);
}

