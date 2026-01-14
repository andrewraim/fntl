// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::IntegerVector apply_ex()
{
	Rcpp::IntegerMatrix X(4, 3);
	for (unsigned int j = 0; j < 3; j++) {
		for (unsigned int i = 0; i < 4; i++) {
			X(i,j) = i + j*4 + 1;
		}
	}
	Rcpp::print(X);

    std::function f = [](const Rcpp::IntegerVector& x) -> int {
    	return Rcpp::sum(x);
    };
    //Rcpp::IntegerVector out = fntl::row_apply(X, f);
    Rcpp::IntegerVector out = fntl::col_apply(X, f);
    return out;
}
