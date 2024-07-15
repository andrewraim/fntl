// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List solve_cg_ex(Rcpp::NumericVector b)
{
    fntl::vfv l = [](Rcpp::NumericVector x) {
        unsigned int n = x.size();
        Rcpp::NumericVector out(n);

        for (unsigned int i = 1; i < n-1; i++) {
            out(i) = x(i-1) + 2*x(i) + x(i+1);
        }
        out(0) = 2*x(0) + x(1);
        out(n-1) = x(n-2) + 2*x(n-1);

        return out;
    };

    auto out = fntl::solve_cg(l, b);
    return Rcpp::wrap(out);
}
