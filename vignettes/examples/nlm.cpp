// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List nlm_ex(Rcpp::NumericVector x0)
{
    fntl::dfv f = [](const Rcpp::NumericVector& x) {
        Rcpp::NumericVector xx = Rcpp::pow(x, 2);
        double ss = Rcpp::sum(xx);
        return std::exp(-ss);
    };

    fntl::vfv g = [&](const Rcpp::NumericVector& x) {
        double fx = f(x);
    	Rcpp::NumericVector out = -2 * fx * x;
        return out;
    };

    fntl::mfv h = [&](const Rcpp::NumericVector& x) {
        unsigned int n = x.size();
        Rcpp::NumericMatrix out(n,n);
        double fx = f(x);

        for (unsigned int j = 0; j < n; j++) {
            for (unsigned int i = 0; i < n; i++) {
                out(i,j) = fx * ( 4*x(i)*x(j) - 2*(i == j) );
            }
        }

        return out;
    };

    fntl::nlm_args args;
    args.fnscale = -1;

    // 1. Use default numerical gradient and hessian.
    // 2. Use explicitly coded gradient and numerical hessian.
    // 3. Use explicitly coded gradient and numerical hessian.
    auto out1 = fntl::nlm(x0, f, args);
    auto out2 = fntl::nlm(x0, f, g, args);
    auto out3 = fntl::nlm(x0, f, g, h, args);

    return Rcpp::List::create(
        Rcpp::Named("res1") = Rcpp::wrap(out1),
        Rcpp::Named("res2") = Rcpp::wrap(out2),
        Rcpp::Named("res3") = Rcpp::wrap(out3)
    );
}
