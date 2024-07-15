library(fntl)
library(numDeriv)

Rcpp::sourceCpp("cpp/test-deriv.cpp")

x0 = 0.5
tol = 0.001

f = function(x) { sin(x) }
out0 = grad(f, x0)

out1 = sine_deriv(x0, 0L)
expect_equal(out1$value, out0, tol)
expect_equal(out1$status, 0)
expect_equal(out1$err, 0, tol)
