// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List deriv_ex(Rcpp::NumericVector x0, unsigned int i, unsigned int type)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double ss = 0;
        for (unsigned int i = 0; i < x.length(); i++) { ss += (i+1) * x(i); }
        return std::sin(ss);
    };

    auto out = fntl::deriv(f, x0, i, fntl::fd_types(type));
    return Rcpp::wrap(out);
}
