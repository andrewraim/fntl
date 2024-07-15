// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List callr_ex(Rcpp::Function f)
{
    fntl::dfd ff = [&](double x) {
        Rcpp::NumericVector out = f(x);
        return out(0);
    };

    fntl::integrate_result out = fntl::integrate(ff, 0, 1);
    return Rcpp::wrap(out);
}
