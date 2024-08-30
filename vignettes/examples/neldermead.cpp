// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List neldermead_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) -> double {
		return Rcpp::sum(Rcpp::pow(x, 2)) - 1;
    };

    auto out = fntl::neldermead(x0, f);
    return Rcpp::wrap(out);
}
