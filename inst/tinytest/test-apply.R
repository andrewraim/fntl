library(fntl)

Rcpp::sourceCpp("cpp/test-apply.cpp")

X = matrix(1:12 + 0.5, 4, 3)

out0 = sapply(X, function(x) { x^(1/3) })
out1 = vec_pow(X, 1/3)
expect_equal(out0, out1)

out0 = apply(X, c(1,2), function(x) { x^(1/3) })
out1 = mat_pow(X, 1/3)
expect_equal(out0, out1)

out0 = apply(X, 1, function(x) { sum(x) })
out1 = row_sum(X)
expect_equal(out0, out1)

out0 = apply(X, 2, function(x) { sum(x) })
out1 = col_sum(X)
expect_equal(out0, out1)

out0 = apply(X, c(1,2), function(x) { floor(x)^2 })
out1 = imat_square(X)
expect_equal(out0, out1)
