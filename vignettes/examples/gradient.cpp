// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List gradient_ex(Rcpp::NumericVector x0)
{
    const fntl::dfv& f = [](Rcpp::NumericVector x) {
        double out = Rcpp::sum(Rcpp::pow(x, 2));
        return out;
    };

    auto out = fntl::gradient(f, x0);
    return Rcpp::wrap(out);
}
