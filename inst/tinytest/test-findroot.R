library(fntl)

Rcpp::sourceCpp("cpp/test-findroot.cpp")

a = 1
b = 0
c = -1
tol = 0.001

f = function(x) { a*x^2 + b*x + c }
curve(f, xlim = c(-2, 2))

out0 = (-b + c(-1,1) * sqrt(b - 4*a*c)) / (2*a)

out1 = uniroot(f, lower = 0, upper = 10)
expect_equal(out1$root, out0[2], tol)

out2 = poly_root_bisect(a, b, c, lower = 0, upper = 10)
expect_equal(out2$root, out0[2], tol)
expect_equal(out2$f_root, out1$f.root, tol)

out3 = poly_root_brent(a, b, c, lower = 0, upper = 10)
expect_equal(out3$root, out0[2], tol)
expect_equal(out3$f_root, out1$f.root, tol)
