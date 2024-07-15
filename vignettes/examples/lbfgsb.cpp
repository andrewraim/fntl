// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List lbfgsb_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](Rcpp::NumericVector x) {
        double ss = Rcpp::sum(Rcpp::pow(x, 2));
        return std::exp(-ss);
    };

    fntl::vfv g = [](Rcpp::NumericVector x) {
        double ss = Rcpp::sum(Rcpp::pow(x, 2));
    	Rcpp::NumericVector out = -2 * std::exp(-ss) * x;
        return out;
    };

    fntl::lbfgsb_args args;
    args.fnscale = -1;

    auto out1 = fntl::lbfgsb(x0, f, args);     // with default numerical gradient
    auto out2 = fntl::lbfgsb(x0, f, g, args);  // with explicitly coded gradient

    return Rcpp::List::create(
        Rcpp::Named("numerical") = Rcpp::wrap(out1),
        Rcpp::Named("analytical") = Rcpp::wrap(out2)
    );
}
