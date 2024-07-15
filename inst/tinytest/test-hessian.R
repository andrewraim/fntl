library(fntl)
library(numDeriv)

Rcpp::sourceCpp("cpp/test-hessian.cpp")

x0 = c(0.5, 0.5)

out0 = diag(x = 2, nrow = 2, ncol = 2)

f = function(x) { sum(x^2) }
out1 = hessian(f, x0)
expect_equal(out1, out0)

out2 = quadratic_hessian(x0)
expect_equal(out2$value, out0)
