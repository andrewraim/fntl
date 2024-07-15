// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List jacobian_ex(Rcpp::NumericVector x0)
{
    fntl::vfv f = [](Rcpp::NumericVector x) {
        Rcpp::NumericVector out = Rcpp::cumsum(Rcpp::sin(x));
        return out;
    };

    auto out = fntl::jacobian(f, x0);
    return Rcpp::wrap(out);
}
