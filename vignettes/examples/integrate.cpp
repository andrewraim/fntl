// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List integrate_ex(double lower, double upper)
{
    fntl::dfd f = [](double x) { return exp(-pow(x, 2) / 2); };
    auto out = fntl::integrate(f, lower, upper);
    return Rcpp::wrap(out);
}
