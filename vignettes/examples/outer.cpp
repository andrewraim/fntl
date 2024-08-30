// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List outer_ex(Rcpp::NumericMatrix X, Rcpp::NumericMatrix Y,
    Rcpp::NumericVector a, Rcpp::NumericVector b)
{
    fntl::dfvv f =
    [](Rcpp::NumericVector x, Rcpp::NumericVector y) {
        double norm2 = Rcpp::sum(Rcpp::pow(x - y, 2));
        return std::sqrt(norm2);
    };

    return Rcpp::List::create(
        Rcpp::Named("out1") = fntl::outer(X, f),
        Rcpp::Named("out2") = fntl::outer(X, Y, f),
        Rcpp::Named("out3") = fntl::outer_matvec(X, f, a),
        Rcpp::Named("out4") = fntl::outer_matvec(X, Y, f, b)
    );
}

