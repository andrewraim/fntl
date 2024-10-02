library(fntl)

Rcpp::sourceCpp("cpp/test-multivariate-optimization.cpp")
set.seed(1234)

# ----- Likelihood maximization with each method -----

# Maximize likelihood of a sample from N(mu, sigma2)
n = 200
mu = 3
sigma2 = 10
tol = 0.01

x = rnorm(n, mu, sqrt(sigma2))

loglik = function(par) {
	mu = par[1]
	sigma2 = exp(par[2])
	sum(dnorm(x, mu, sqrt(sigma2), TRUE))
}
numDeriv::grad(func = loglik, c(0,0))

mu_hat = mean(x)
sigma2_hat = (n-1) / n * var(x)
par_hat = c(mu_hat, log(sigma2_hat))
ll_hat = loglik(par_hat)

out = neldermead_mle(x)
expect_equal(out$par[1], mu_hat, tolerance = tol)
expect_equal(exp(out$par[2]), sigma2_hat, tolerance = tol)
expect_equal(out$value, ll_hat, tolerance = tol)

out = lbfgsb_mle(x)
expect_equal(out$par[1], mu_hat, tolerance = tol)
expect_equal(exp(out$par[2]), sigma2_hat, tolerance = tol)
expect_equal(out$value, ll_hat, tolerance = tol)

out = bfgs_mle(x)
expect_equal(out$par[1], mu_hat, tolerance = tol)
expect_equal(exp(out$par[2]), sigma2_hat, tolerance = tol)
expect_equal(out$value, ll_hat, tolerance = tol)

out = cg_mle(x)
expect_equal(out$par[1], mu_hat, tolerance = tol)
expect_equal(exp(out$par[2]), sigma2_hat, tolerance = tol)
expect_equal(out$value, ll_hat, tolerance = tol)

out = nlm_mle(x)
expect_equal(out$par[1], mu_hat, tolerance = tol)
expect_equal(exp(out$par[2]), sigma2_hat, tolerance = tol)
expect_equal(out$estimate, ll_hat, tolerance = tol)

# ----- Norm minimization with each method -----

# Minimize norm2(x - a) for a given a.
a = c(-1, 0, 1)

out = neldermead_norm2(a)
expect_equal(out$par, a, tolerance = tol)
expect_equal(out$value, 0, tolerance = tol)

out = lbfgsb_norm2(a)
expect_equal(out$par, a, tolerance = tol)
expect_equal(out$value, 0, tolerance = tol)

out = bfgs_norm2(a)
expect_equal(out$par, a, tolerance = tol)
expect_equal(out$value, 0, tolerance = tol)

out = cg_norm2(a)
expect_equal(out$par, a, tolerance = tol)
expect_equal(out$value, 0, tolerance = tol)

out = nlm_norm2(a)
expect_equal(out$par, a, tolerance = tol)
expect_equal(out$estimate, 0, tolerance = tol)
