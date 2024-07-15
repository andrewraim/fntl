// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
double fd_deriv2_ex(Rcpp::NumericVector x0, unsigned int i, unsigned int j,
    double h_i, double h_j, unsigned int type)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double ss = 0;
        for (unsigned int i = 0; i < x.length(); i++) { ss += (i+1) * x(i); }
        return std::sin(ss);
    };

    return fntl::fd_deriv2(f, x0, i, j, h_i, h_j, fntl::fd_types(type));
}
