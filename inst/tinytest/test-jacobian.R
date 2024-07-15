library(fntl)
library(numDeriv)

Rcpp::sourceCpp("cpp/test-jacobian.cpp")

x0 = c(0.5, 0.25)
f = function(x) { c(sum(2 * x^2), sum(4 * x^2)) }

out0 = jacobian(f, x0)
out1 = quadratic_jacobian(x0)
expect_equal(out1$value, out0)
