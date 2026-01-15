// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List apply1_ex(Rcpp::NumericMatrix X)
{
    std::function f = [](double x) -> double { return std::pow(x, 2); };
    std::function g = [](const Rcpp::NumericVector& x) -> double {
        return Rcpp::sum(x);
    };
    return Rcpp::List::create(
        Rcpp::Named("pow") = fntl::mat_apply(X, f),
        Rcpp::Named("rowsum") = fntl::row_apply(X, g),
        Rcpp::Named("colsum") = fntl::col_apply(X, g)
    );
}

// [[Rcpp::export]]
Rcpp::List apply2_ex(Rcpp::IntegerMatrix X)
{
    std::function f = [](int x) -> int { return std::pow(x, 2); };
    std::function g = [](const Rcpp::IntegerVector& x) -> double {
        return std::sqrt(Rcpp::sum(Rcpp::pow(x, 3)));
    };
    return Rcpp::List::create(
        Rcpp::Named("pow") = fntl::mat_apply(X, f),
        Rcpp::Named("rowsum") = fntl::row_apply(X, g),
        Rcpp::Named("colsum") = fntl::col_apply(X, g)
    );
}

// [[Rcpp::export]]
Rcpp::List apply3_ex(Rcpp::StringMatrix X)
{
    std::function f = [](const char* x) -> int { return strlen(x); };
    std::function g = [](const Rcpp::CharacterVector& x) -> int {
        size_t maxlen = 0;
        for (unsigned int i = 0; i < x.size(); i++) {
            maxlen = std::max(maxlen, strlen(x[i]));
        }
        return maxlen;
    };
    return Rcpp::List::create(
        Rcpp::Named("len") = fntl::mat_apply(X, f),
        Rcpp::Named("rowmax") = fntl::row_apply(X, g),
        Rcpp::Named("colmax") = fntl::col_apply(X, g)
    );
}
