// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List neldermead_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
		double out = Rcpp::sum(Rcpp::pow(x, 2)) - 1;
        return out;
    };

    auto out = fntl::neldermead(x0, f);
    return Rcpp::wrap(out);
}
