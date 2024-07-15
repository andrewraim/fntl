// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List deriv2_ex(Rcpp::NumericVector x0, unsigned int i, unsigned int j,
    unsigned int type)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double ss = 0;
        for (unsigned int i = 0; i < x.length(); i++) { ss += (i+1) * x(i); }
        return std::sin(ss);
    };

    auto out = fntl::deriv2(f, x0, i, j, fntl::fd_types(type));
    return Rcpp::wrap(out);
}
