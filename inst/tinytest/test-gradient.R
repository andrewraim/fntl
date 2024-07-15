library(fntl)
library(numDeriv)

Rcpp::sourceCpp("cpp/test-gradient.cpp")

x0 = rep(0.25, 10)

f = function(x) { sum(x^2) }
out0 = grad(f, x0)
expect_equal(out0, 2*x0)

out1 = quadratic_gradient(x0)
expect_equal(out1$value, out0)
