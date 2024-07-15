// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
double fd_deriv_ex(Rcpp::NumericVector x0, unsigned int i, double h,
    unsigned int type)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double ss = 0;
        for (unsigned int i = 0; i < x.length(); i++) { ss += (i+1) * x(i); }
        return std::sin(ss);
    };

    return fntl::fd_deriv(f, x0, i, h, fntl::fd_types(type));
}
