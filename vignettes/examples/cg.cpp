// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List cg_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](const Rcpp::NumericVector& x) {
        Rcpp::NumericVector xx = Rcpp::pow(x, 2);
        double ss = Rcpp::sum(xx);
        return std::exp(-ss);
    };

    fntl::vfv g = [](const Rcpp::NumericVector& x) {
        Rcpp::NumericVector xx = Rcpp::pow(x, 2);
        double ss = Rcpp::sum(xx);
        return -2 * std::exp(-ss) * x;
    };

    fntl::cg_args args;
    args.fnscale = -1;

    auto out1 = fntl::cg(x0, f, args);     // with default numerical gradient
    auto out2 = fntl::cg(x0, f, g, args);  // with explicitly coded gradient

    return Rcpp::List::create(
        Rcpp::Named("numerical") = Rcpp::wrap(out1),
        Rcpp::Named("analytical") = Rcpp::wrap(out2)
    );
}
