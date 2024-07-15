// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List hessian_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double out = Rcpp::sum(Rcpp::sin(x));
        return out;
    };

    auto out = fntl::hessian(f, x0);
    return Rcpp::wrap(out);
}
