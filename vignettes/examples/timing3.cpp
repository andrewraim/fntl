// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::NumericVector timing3_ex(unsigned int R, const Rcpp::IntegerVector& n_levels)
{
	double beta0_true = 0;
	double beta1_true = 1;

	Rcpp::NumericVector out(n_levels.size());

	for (unsigned int k = 0; k < n_levels.size(); k++) {
		unsigned int n = n_levels(k);
		const Rcpp::NumericVector& x = Rcpp::rnorm(n);
		const Rcpp::NumericVector& pr_true = Rcpp::plogis(beta0_true + beta1_true * x);

		Rcpp::NumericMatrix beta_hats(R, 2);
		Rcpp::NumericMatrix beta_trues(R, 2);

		for (unsigned int r = 0; r < R; r++) {
			// Generate a sample
			Rcpp::IntegerVector y(n);
			for (unsigned int i = 0; i < n; i++) {
				y(i) = R::rbinom(1, pr_true(i));
			}

			// Loglikelihood function. Call vectorized dbinom in R
			const fntl::dfv& loglik = [&](const Rcpp::NumericVector& par) -> double {
			    Rcpp::Function dbinom_r("dbinom");
				const Rcpp::NumericVector& pr = Rcpp::plogis(par(0) + par(1) * x);
    			const Rcpp::NumericVector& ll = dbinom_r(y, Rcpp::Named("size") = 1,
    				Rcpp::Named("prob") = pr, Rcpp::Named("log") = true);
    			return Rcpp::sum(ll);
			};

			// Compute the MLE using L-BFGS-B
			Rcpp::NumericVector init = { 0, 0 };
			fntl::lbfgsb_args args;
			args.fnscale = -1;
			const auto& lbfgsb_out = lbfgsb(init, loglik,  args);

			// Save estimates
			beta_hats(r, 0) = lbfgsb_out.par[0];
			beta_hats(r, 1) = lbfgsb_out.par[1];
			beta_trues(r, 0) = beta0_true;
			beta_trues(r, 1) = beta1_true;
		}

		out(k) = Rcpp::mean(Rcpp::pow(beta_hats - beta_trues, 2));
		Rcpp::checkUserInterrupt();
	}

	return out;
}

