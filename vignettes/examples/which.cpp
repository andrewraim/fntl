// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::IntegerMatrix which_ex(Rcpp::NumericMatrix X)
{
    std::function<bool(double)> f = [](double x) { return x > 0 && x < 0.5; };
    return fntl::which(X, f);
}
