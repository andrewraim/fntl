// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List crash_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
    	return Rcpp::sum(x*x);
    };
    auto out = fntl::gradient(f, x0);
    return Rcpp::wrap(out);
}
