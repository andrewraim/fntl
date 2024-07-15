// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List apply_ex(Rcpp::NumericMatrix X)
{
    fntl::dfd f = [](double x) { return std::pow(x, 2); };
    fntl::dfv g = [](Rcpp::NumericVector x) {
    	return Rcpp::sum(x);
    };

    return Rcpp::List::create(
        Rcpp::Named("pows") = fntl::mat_apply(X, f),
        Rcpp::Named("rowsums") = fntl::row_apply(X, g),
        Rcpp::Named("colsums") = fntl::col_apply(X, g)
    );
}
