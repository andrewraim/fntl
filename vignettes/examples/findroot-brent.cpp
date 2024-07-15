// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List findroot_brent_ex(double lower, double upper)
{
    fntl::dfd f = [](double x) { return pow(x, 2) - 1; };
    auto out = fntl::findroot_brent(f, lower, upper);
    return Rcpp::wrap(out);
}
