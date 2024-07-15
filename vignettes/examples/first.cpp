// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List first_ex(double a, double b)
{
    fntl::integrate_args args;
    args.subdivisions = 200L;

    fntl::dfd f = [&](double x) {
        return std::pow(x, a - 1) * std::pow(1 - x, b - 1);
    };
    fntl::integrate_result out = fntl::integrate(f, 0, 1, args);

    Rprintf("value: %g\n", out.value);
    Rprintf("status: %d\n", to_underlying(out.status));

    return Rcpp::wrap(out);
}
