#ifndef LOG_SUM_EXP_H
#define LOG_SUM_EXP_H

#include <Rcpp.h>

namespace fntl {

/*
* Returns scalar `log(sum(exp(x))` for vector `x`. Computed using the method
* described in StackExchange post <https://stats.stackexchange.com/a/381937>.
*/
inline double log_sum_exp(const Rcpp::NumericVector& x)
{
    unsigned int k = x.size();

    Rcpp::NumericVector v = Rcpp::clone(x);
    std::sort(v.begin(), v.end(), std::greater<double>());

    double s = v(0);

    for (unsigned int j = 1; j < k; j++) {
        double ind = s > R_NegInf;
        double dd = v(j) - std::pow(s, ind);
        s = std::max(v(j), s) + std::log1p(std::exp(-std::fabs(dd)));
    }

    return s;
}

/*
* Returns `log(exp(x) + exp(y))` for scalars `x` and `y`.
*/
inline double log_add2_exp(double x, double y)
{
	double s = std::min(x,y);
	double t = std::max(x,y);
	return t + std::log1p(exp(s - t));
}

/*
* Returns `log(exp(x) - exp(y))` for scalars `x` and `y`. Result is `NaN` when
* $x < y$.
*/
inline double log_sub2_exp(double x, double y)
{
	if (std::isinf(x) && std::isinf(y) && x < 0 && y < 0) {
		return R_NegInf;
	}

	return x + std::log1p(-exp(y - x));
}


}

#endif
