Rcpp::sourceCpp("cpp/test-univariate-optimization.cpp")

out0 = -base::pi / 2

out1 = sine_goldensection(-pi, pi)
expect_equal(out1$par, out0)
expect_equal(out1$value, sin(out0))
expect_equal(out1$tol, 0, 1e-5)

out2 = sine_brent(-pi, pi)
expect_equal(out2$par, out0)
expect_equal(out2$value, sin(out0))
expect_equal(out2$tol, 0, 1e-5)
