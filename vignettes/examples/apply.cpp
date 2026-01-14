// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List apply_ex(Rcpp::NumericMatrix X, Rcpp::IntegerMatrix Y,
	Rcpp::StringMatrix Z)
{
    std::function fx = [](double x) -> double { return std::pow(x, 2); };
    std::function gx = [](const Rcpp::NumericVector& x) -> double {
    	return Rcpp::sum(x);
    };

    std::function fy = [](int x) -> int { return std::pow(x, 2); };
	std::function gy = [](const Rcpp::IntegerVector& x) -> double {
		return std::sqrt(Rcpp::sum(Rcpp::pow(x, 2)));
	};

    std::function fz = [](const char* x) -> int { return strlen(x); };
	std::function gz = [](const Rcpp::CharacterVector& x) -> int {
		size_t maxlen = 0;
		for (unsigned int i = 0; i < x.size(); i++) {
			maxlen = std::max(maxlen, strlen(x[i]));
		}
		return maxlen;
	};

    return Rcpp::List::create(
        Rcpp::Named("X_pows") = fntl::mat_apply(X, fx),
        Rcpp::Named("X_row_sums") = fntl::row_apply(X, gx),
        Rcpp::Named("X_col_sums") = fntl::col_apply(X, gx),
        Rcpp::Named("Y_pows") = fntl::mat_apply(Y, fy),
        Rcpp::Named("Y_row_sums") = fntl::row_apply(Y, gy),
        Rcpp::Named("Y_col_sums") = fntl::col_apply(Y, gy),
        Rcpp::Named("Z_len") = fntl::mat_apply(Z, fz),
        Rcpp::Named("Z_len_rowmax") = fntl::row_apply(Z, gz),
        Rcpp::Named("Z_len_colmax") = fntl::col_apply(Z, gz)
    );
}
