// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List findroot_bisect_ex(double lower, double upper)
{
    fntl::dfd f = [](double x) {
        return pow(x, 2) - 1;
    };
    auto out = fntl::findroot_bisect(f, lower, upper);
    return Rcpp::wrap(out);
}
