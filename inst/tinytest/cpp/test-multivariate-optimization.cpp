// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List neldermead_mle(const Rcpp::NumericVector& x)
{
	fntl::neldermead_args args;
	args.fnscale = -1.0;

	const fntl::dfv& loglik =
	[&](const Rcpp::NumericVector& par) -> double {
		double mu = par(0);
		double sigma2 = std::exp(par(1)); // Transform to non-negative
		return Rcpp::sum(Rcpp::dnorm(x, mu, std::sqrt(sigma2), true));
	};

	const Rcpp::NumericVector& init = Rcpp::NumericVector::create(0, 0);
	const auto& out = fntl::neldermead(init, loglik, args);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List lbfgsb_mle(const Rcpp::NumericVector& x)
{
	fntl::lbfgsb_args args;
	args.fnscale = -1.0;

	const fntl::dfv& loglik =
	[&](const Rcpp::NumericVector& par) -> double {
		double mu = par(0);
		double sigma2 = std::exp(par(1)); // Transform to non-negative
		return Rcpp::sum(Rcpp::dnorm(x, mu, std::sqrt(sigma2), true));
	};

	const Rcpp::NumericVector& init = Rcpp::NumericVector::create(0, 0);
	const auto& out = fntl::lbfgsb(init, loglik, args);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List bfgs_mle(const Rcpp::NumericVector& x)
{
	fntl::bfgs_args args;
	args.fnscale = -1.0;

	const fntl::dfv& loglik =
	[&](const Rcpp::NumericVector& par) -> double {
		double mu = par(0);
		double sigma2 = std::exp(par(1)); // Transform to non-negative
		return Rcpp::sum(Rcpp::dnorm(x, mu, std::sqrt(sigma2), true));
	};

	const Rcpp::NumericVector& init = Rcpp::NumericVector::create(0, 0);
	const auto& out = fntl::bfgs(init, loglik, args);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List cg_mle(const Rcpp::NumericVector& x)
{
	fntl::cg_args args;
	args.fnscale = -1.0;

	const fntl::dfv& loglik =
	[&](const Rcpp::NumericVector& par) -> double {
		double mu = par(0);
		double sigma2 = std::exp(par(1)); // Transform to non-negative
		return Rcpp::sum(Rcpp::dnorm(x, mu, std::sqrt(sigma2), true));
	};

	const Rcpp::NumericVector& init = Rcpp::NumericVector::create(0, 0);
	const auto& out = fntl::cg(init, loglik, args);
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::List nlm_mle(const Rcpp::NumericVector& x)
{
	fntl::nlm_args args;
	args.fnscale = -1.0;

	const fntl::dfv& loglik =
	[&](const Rcpp::NumericVector& par) -> double {
		double mu = par(0);
		double sigma2 = std::exp(par(1)); // Transform to non-negative
		return Rcpp::sum(Rcpp::dnorm(x, mu, std::sqrt(sigma2), true));
	};

	const Rcpp::NumericVector& init = Rcpp::NumericVector::create(0, 0);
	const auto& out = fntl::nlm(init, loglik, args);
	return Rcpp::wrap(out);
}
