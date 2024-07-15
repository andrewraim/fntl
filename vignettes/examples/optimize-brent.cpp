// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List optimize_brent_ex(double lower, double upper)
{
    fntl::optimize_args args;
    args.fnscale = -1;

    fntl::dfd f = [](double x) { return exp(-x); };
    auto out = fntl::optimize_brent(f, lower, upper, args);
    return Rcpp::wrap(out);
}
