Rcpp::sourceCpp("cpp/test-univariate-optimization.cpp")

x_min = -base::pi / 2
x_max = +base::pi / 2

out = sine_goldensection(-pi, pi, maximize = FALSE)
expect_equal(out$par, x_min)
expect_equal(out$value, sin(x_min))
expect_equal(out$tol, 0, 1e-5)

out = sine_brent(-pi, pi, maximize = FALSE)
expect_equal(out$par, x_min)
expect_equal(out$value, sin(x_min))
expect_equal(out$tol, 0, 1e-5)

out = sine_goldensection(-pi, pi, maximize = TRUE)
expect_equal(out$par, x_max)
expect_equal(out$value, sin(x_max))
expect_equal(out$tol, 0, 1e-5)

out = sine_brent(-pi, pi, maximize = TRUE)
expect_equal(out$par, x_max)
expect_equal(out$value, sin(x_max))
expect_equal(out$tol, 0, 1e-5)
