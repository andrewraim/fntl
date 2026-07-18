#ifndef FNTL_TRUNC_H
#define FNTL_TRUNC_H

#include "typedefs.h"
#include "log-sum-exp.h"

namespace fntl {

/*
* In these functions, we compute two versions of probabilities: one making
* use of lower CDF values and one making use of upper CDF values. One of the
* two may lose precision and become `-Inf` while the other remains finite.
* Therefore, take the maximum of the two.
*/

inline double d_trunc(double x, double lo, double hi, const density& f,
	const cdf& F, bool log = false)
{
	double log_pa = F(lo, true, true);
	double log_pb = F(hi, true, true);
	double log_p = log_sub2_exp(log_pb, log_pa);

	double log_cpa = F(lo, false, true);
	double log_cpb = F(hi, false, true);
	double log_cp = log_sub2_exp(log_cpa, log_cpb);

	double log_den = std::max(log_p, log_cp);

	double out = f(x, true) + std::log(lo < x && x <= hi) - log_den;
	return log ? out : exp(out);
}

inline double p_trunc(double x, double lo, double hi, const cdf& F,
	bool lower = true, bool log = false)
{
	double out;

	if (x < lo) {
		out = R_NegInf;
	} else if (x > hi) {
		out = 0;
	} else {
		double lpa = F(lo, true, true);
		double lpb = F(hi, true, true);
		double clpa = F(lo, false, true);
		double clpb = F(hi, false, true);
		double lp_den = log_sub2_exp(lpb, lpa);
		double clp_den = log_sub2_exp(clpa, clpb);

		double lpx = F(x, true, true);
		double clpx = F(x, false, true);
		double lp_num;
		double clp_num;

		if (lower) {
			lp_num = log_sub2_exp(lpx, lpa);
			clp_num = log_sub2_exp(clpa, clpx);
		} else {
			lp_num = log_sub2_exp(lpb, lpx);
			clp_num = log_sub2_exp(clpx, clpb);
		}

		out = std::max(lp_num, clp_num) - std::max(lp_den, clp_den);
   	}

	return log ? out : exp(out);
}

inline double q_trunc(double p, double lo, double hi, const cdf& F,
	const quantile& Finv, bool lower = true, bool log = false)
{
	double lpp = log ? p : std::log(p);
	lpp = lower ? lpp : log_sub2_exp(0, lpp);

	double lpa = F(lo, true, true);
	double lpb = F(hi, true, true);
	double lp = log_sub2_exp(lpb, lpa);

	double clpa = F(lo, false, true);
	double clpb = F(hi, false, true);
	double clp = log_sub2_exp(clpa, clpb);

	/*
	* Probability of the denominator (on the log-scale) computed using both
	* complementary and non-complementary probabilities. The values of lo and
	* hi may be such that one evaluates numerically to -Inf but the other
	* evaluates to a finite number.
	*
	* Also protect against log-probabilities greater than zero, which can
	* happen numerically.
	*/

	// Use the complementary probabilities
	double clq = log_sub2_exp(clpa, lpp + clp);
	clq = std::min(clq, 0.0);
	double out1 = Finv(clq, false, true);

	// Use the non-complementary probabilities
	double lq = log_add2_exp(lpa, lpp + lp);
	lq = std::min(lq, 0.0);
	double out2 = Finv(lq, true, true);

	double out;

	if (std::isinf(out1) || std::isnan(out1)) {
		out = out2;
	} else if (std::isinf(out2) || std::isnan(out2)) {
		out = out1;
	} else {
		out = out2;
	}

	/*
	* Protect against quantiles outside of the support, which can happen
	* numerically (assuming there are no mistakes).
	*/
	return std::max(std::min(out, hi), lo);
}

inline double r_trunc(double lo, double hi, const cdf& F,
	const quantile& Finv)
{
	double u = R::runif(0, 1);
	return q_trunc(u, lo, hi, F, Finv);
}

/*
* Vector versions, for convenience.
*/

inline Rcpp::NumericVector d_trunc(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const density& f, const cdf& F, bool log = false)
{
	unsigned int n = x.size();
	if (n != lo.size()) { Rcpp::stop("n != lo.size()"); }
	if (n != hi.size()) { Rcpp::stop("n != hi.size()"); }
	Rcpp::NumericVector out(n);

	for (unsigned int i = 0; i < n; i++) {
		out(i) = d_trunc(x(i), lo(i), hi(i), f, F, log);
	}

	return out;
}

inline Rcpp::NumericVector p_trunc(const Rcpp::NumericVector& x,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const cdf& F, bool lower = true, bool log = false)
{
	unsigned int n = x.size();
	if (n != lo.size()) { Rcpp::stop("n != lo.size()"); }
	if (n != hi.size()) { Rcpp::stop("n != hi.size()"); }
	Rcpp::NumericVector out(n);

	for (unsigned int i = 0; i < n; i++) {
		out(i) = p_trunc(x(i), lo(i), hi(i), F, lower, log);
	}

	return out;
}

inline Rcpp::NumericVector q_trunc(const Rcpp::NumericVector& p,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const cdf& F, const quantile& Finv, bool lower = true, bool log = false)
{
	unsigned int n = p.size();
	if (n != lo.size()) { Rcpp::stop("n != lo.size()"); }
	if (n != hi.size()) { Rcpp::stop("n != hi.size()"); }
	Rcpp::NumericVector out(n);

	for (unsigned int i = 0; i < n; i++) {
		out(i) = q_trunc(p(i), lo(i), hi(i), F, Finv, lower, log);
	}

	return out;
}

inline Rcpp::NumericVector r_trunc(unsigned int n,
	const Rcpp::NumericVector& lo, const Rcpp::NumericVector& hi,
	const cdf& F, const quantile& Finv)
{
	const Rcpp::NumericVector& u = Rcpp::runif(n);
	return q_trunc(u, lo, hi, F, Finv);
}

}

#endif
