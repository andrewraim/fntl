// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List gradient_ex(Rcpp::NumericVector x0)
{
    const fntl::dfv& f = [](Rcpp::NumericVector x) -> double {
        return Rcpp::sum(Rcpp::pow(x, 2));
    };

    auto out = fntl::gradient(f, x0);
    return Rcpp::wrap(out);
}
