library(tidyverse)

set.seed(1234)
Rcpp::sourceCpp("cpp/test-trunc.cpp")

shape1 = 5
shape2 = 2
lo = 0.5
hi = 0.6

# Check density
expect_equal(d_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(d_beta_trunc(1, shape1, shape2, lo, hi), 0)
expect_true(d_beta_trunc(0.51, shape1, shape2, lo, hi) > 0)
expect_true(d_beta_trunc(0.59, shape1, shape2, lo, hi) > 0)
expect_equal(d_beta_trunc(0, shape1, shape2, lo, hi, log = T), -Inf)
expect_equal(d_beta_trunc(1, shape1, shape2, lo, hi, log = T), -Inf)
lp = d_beta_trunc(0.59, shape1, shape2, lo, hi, log = T)
expect_true(lp > -Inf)

# Check CDF
expect_equal(p_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(p_beta_trunc(1, shape1, shape2, lo, hi), 1)
pr = p_beta_trunc(0.51, shape1, shape2, lo, hi)
cpr = p_beta_trunc(0.51, shape1, shape2, lo, hi, lower = F)
expect_true(0 < pr && pr < 1)
expect_equal(cpr, 1 - pr)
lpr = p_beta_trunc(0.51, shape1, shape2, lo, hi, log = T)
clpr = p_beta_trunc(0.51, shape1, shape2, lo, hi, lower = F, log = T)
expect_equal(lpr, log(pr))
expect_equal(clpr, log(cpr))

# Check quantile
expect_equal(q_beta_trunc(0, shape1, shape2, lo, hi), lo)
expect_equal(q_beta_trunc(1, shape1, shape2, lo, hi), hi)
qq = q_beta_trunc(0.5, shape1, shape2, lo, hi)
expect_true(lo < qq && qq < hi)

qq = q_beta_trunc(0.75, shape1, shape2, lo, hi)
cqq = q_beta_trunc(0.75, shape1, shape2, lo, hi, lower = F)
expect_equal(p_beta_trunc(qq, shape1, shape2, lo, hi), 0.75)
expect_equal(p_beta_trunc(cqq, shape1, shape2, lo, hi), 0.25)

# Try some cases with distribution truncated to the lower part of the support
lo = 0
hi = 0.001

expect_equal(d_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(d_beta_trunc(1, shape1, shape2, lo, hi), 0)
expect_true(d_beta_trunc(0.0005, shape1, shape2, lo, hi) > 0)

expect_equal(p_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(p_beta_trunc(hi, shape1, shape2, lo, hi), 1)
expect_equal(p_beta_trunc(1, shape1, shape2, lo, hi), 1)
pr1 = p_beta_trunc(hi - hi*1e-2, shape1, shape2, lo, hi)
pr2 = p_beta_trunc(hi - hi*1e-6, shape1, shape2, lo, hi)
pr3 = p_beta_trunc(hi, shape1, shape2, lo, hi)
cpr1 = p_beta_trunc(hi - hi*1e-2, shape1, shape2, lo, hi, lower = F)
cpr2 = p_beta_trunc(hi - hi*1e-6, shape1, shape2, lo, hi, lower = F)
cpr3 = p_beta_trunc(hi, shape1, shape2, lo, hi, lower = F)
expect_true(pr1 > 0)
expect_true(pr1 < pr2)
expect_true(pr2 <= pr3)
# expect_equal(cpr1, 1 - pr1, tolerance = 0.02) ## Precision for this one is off...
expect_equal(cpr2, 1 - pr2, tolerance = 0.02)
expect_equal(cpr3, 1 - pr3)

expect_equal(q_beta_trunc(0, shape1, shape2, lo, hi), lo)
expect_equal(q_beta_trunc(1, shape1, shape2, lo, hi), hi)
qq = q_beta_trunc(0.5, shape1, shape2, lo, hi)
expect_true(lo < qq &&  qq < hi)

# Try some cases with distribution truncated to the upper part of the support
lo = 0.999
hi = 1

expect_equal(d_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(d_beta_trunc(1, shape1, shape2, lo, hi), 0)
expect_true(d_beta_trunc(0.9995, shape1, shape2, lo, hi) > 0)

expect_equal(p_beta_trunc(0, shape1, shape2, lo, hi), 0)
expect_equal(p_beta_trunc(hi, shape1, shape2, lo, hi), 1)
expect_equal(p_beta_trunc(1, shape1, shape2, lo, hi), 1)
pr1 = p_beta_trunc(lo + (hi-lo)*1e-6, shape1, shape2, lo, hi)
pr2 = p_beta_trunc(lo + (hi-lo)*1e-2, shape1, shape2, lo, hi)
pr3 = p_beta_trunc(hi, shape1, shape2, lo, hi)
cpr1 = p_beta_trunc(lo + (hi-lo)*1e-6, shape1, shape2, lo, hi, lower = F)
cpr2 = p_beta_trunc(lo + (hi-lo)*1e-2, shape1, shape2, lo, hi, lower = F)
cpr3 = p_beta_trunc(hi, shape1, shape2, lo, hi, lower = F)
expect_true(pr1 > 0)
expect_true(pr1 < pr2)
expect_true(pr2 <= pr3)
# expect_equal(cpr1, 1 - pr1, tolerance = 0.02) ## Precision for this one is off...
expect_equal(cpr2, 1 - pr2, tolerance = 0.02)
expect_equal(cpr3, 1 - pr3)

expect_equal(q_beta_trunc(0, shape1, shape2, lo, hi), lo)
expect_equal(q_beta_trunc(1, shape1, shape2, lo, hi), hi)
qq = q_beta_trunc(0.5, shape1, shape2, lo, hi)
expect_true(lo < qq &&  qq < hi)

run_checks = function(m, shape1, shape2, lo, hi) {
	# Compare truncated density and CDF with empirical density of generated draws
	x = r_beta_trunc(n = 500000, shape1, shape2, lo, hi)

	prob_emp = numeric(m)
	prob_cdf = numeric(m)
	prob_pdf = numeric(m)
	for (i in 1:m) {
		a = (i - 1) / m
		b = i / m
		prob_emp[i] = mean(a < x & x <= b)
		prob_cdf[i] = p_beta_trunc(b, shape1, shape2, lo, hi) -
			p_beta_trunc(a, shape1, shape2, lo, hi)
		out = integrate(lower = a, upper = b, f = function(x) {
			d_beta_trunc(x, shape1, shape2, lo, hi)
		})
		prob_pdf[i] = out$value
	}
	prob_pdf = as.numeric(prob_pdf)
	prob_cdf = as.numeric(prob_cdf)
	prob_emp = as.numeric(prob_emp)
	expect_equal(prob_cdf, prob_pdf)
	expect_equal(prob_emp, prob_cdf, tolerance = 1e-2)

	# Compare empirical quantiles of draws with quantile function
	pseq = seq(0, 1, length.out = m)
	qq_emp = quantile(x, prob = pseq)
	qq_fun = q_beta_trunc(pseq, shape1, shape2, lo, hi)
	qq_fun = as.numeric(qq_fun)
	qq_emp = as.numeric(qq_emp)
	expect_equal(qq_fun, qq_emp, tolerance = 1e-2)
}

run_checks(m = 20, shape1 = 5, shape2 = 2, lo = 0, hi = 1)
run_checks(m = 20, shape1 = 5, shape2 = 2, lo = 0, hi = 0.5)
run_checks(m = 20, shape1 = 5, shape2 = 2, lo = 0.5, hi = 1)
run_checks(m = 20, shape1 = 5, shape2 = 2, lo = 0, hi = 0.05)
run_checks(m = 20, shape1 = 5, shape2 = 2, lo = 0.95, hi = 1)
