#include <Rcpp.h>

// [[Rcpp::export]]
double lambda_ex1()
{
	auto f = [](double x, double y) -> double { return x*y; };
	return f(2, 3);
}

// [[Rcpp::export]]
double lambda_ex2()
{
	auto f = [](double x, double y) { return x*y; };
	auto g = [](double x) { return x*x; };
	return g(f(2, 3));
}

// [[Rcpp::export]]
double lambda_ex3()
{
	Rcpp::NumericVector x = Rcpp::rnorm(200);
	auto loglik = [&](double mu) {
		Rcpp::NumericVector ll = Rcpp::dnorm(x, mu, 1, true);
		return Rcpp::sum(ll);
	};
	return loglik(0);
}

// [[Rcpp::export]]
double lambda_ex4()
{
	Rcpp::NumericVector x = Rcpp::rnorm(200);
	std::function<double(double)> loglik = [&](double mu) {
		Rcpp::NumericVector ll = Rcpp::dnorm(x, mu, 1, true);
		return Rcpp::sum(ll);
	};
	return loglik(0);
}
