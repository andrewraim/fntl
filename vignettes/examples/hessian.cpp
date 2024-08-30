// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List hessian_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) -> double {
        return Rcpp::sum(Rcpp::sin(x));
    };

    auto out = fntl::hessian(f, x0);
    return Rcpp::wrap(out);
}
