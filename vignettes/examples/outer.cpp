// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List outer_ex(Rcpp::NumericMatrix X, Rcpp::NumericMatrix Y,
    Rcpp::NumericVector a, Rcpp::NumericVector b)
{
    fntl::dfvv f = [](Rcpp::NumericVector x, Rcpp::NumericVector y) {
        return std::sqrt(Rcpp::sum(Rcpp::pow(x - y, 2)));
    };

   	std::function g = [](
   		const Rcpp::NumericVector& x,
   		const Rcpp::NumericVector& y
   	) -> std::pair<double,bool>
   	{
        double norm = std::sqrt(Rcpp::sum(Rcpp::pow(x - y, 2)));
		bool ind = norm < 2.0;
        return std::pair<double,bool>(norm, ind);
    };

    return Rcpp::List::create(
        Rcpp::Named("out1") = fntl::outer(X, f),
        Rcpp::Named("out2") = fntl::outer(X, Y, f),
        Rcpp::Named("out3") = fntl::outer_matvec(X, f, a),
        Rcpp::Named("out4") = fntl::outer_matvec(X, Y, f, b),
        Rcpp::Named("out5") = fntl::outer_sp(X, g),
        Rcpp::Named("out6") = fntl::outer_sp(X, Y, g)
    );
}

