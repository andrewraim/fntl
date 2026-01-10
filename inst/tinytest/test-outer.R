library(tidyverse)
library(Matrix)

set.seed(1234)
Rcpp::sourceCpp("cpp/test-outer.cpp")

m = 10
n = 8
d = 5

f = \(x,y) { sqrt(sum((x - y)^2)) }
g = \(x,y) { f(x, y) <= 3 }

# ----- Test outer1, outer1_sp, and outer1_matvec -----
x = rnorm(n*d)
X = matrix(x, n, d)

# Expected result
E = matrix(0, n, n)
for (i in 1:n) {
	for (j in 1:n) {
		E[i,j] = dist2(X[i,], X[j,])
	}
}
a = rnorm(n)

expect_error( outer1(x, f) )

A = outer1(X, dist2)
expect_equal(dim(A), c(n,n))
expect_equal(A, E)

sp = outer1_sp(X, f, g = \(x,y) { TRUE })
B = sparseMatrix(sp$i, p = sp$p, x = sp$x, dims = c(sp$m, sp$n), symmetric = T, index1 = FALSE)
expect_all_true(sp$i >= 1)
expect_all_true(sp$i <= m)
expect_all_true(sp$j >= 1)
expect_all_true(sp$j <= n)
expect_equal(as.matrix(B), E)

sp = outer1_sp(X, f, g)
B = sparseMatrix(sp$i, p = sp$p, x = sp$x, dims = c(sp$m, sp$n), symmetric = T, index1 = FALSE)
idx0 = which(E > 3, arr.ind = T)
idx1 = which(E <= 3, arr.ind = T)
expect_equal(B[idx1], E[idx])
expect_all_equal(B[idx0], 0)

expect_true( all(outer1_matvec(X, f, a) == E %*% a) )

# ----- Test outer2, outer2_sp, and outer2_matvec -----
x = rnorm(m*d)
y = rnorm(n*d)
X = matrix(x, m, d)
Y = matrix(y, n, d)

# Expected result
E = matrix(0, m, n)
for (i in 1:m) {
	for (j in 1:n) {
		E[i,j] = dist2(X[i,], Y[j,])
	}
}
a = rnorm(n)

expect_error( outer2(x, y, f) )
expect_error( outer2(X, y, f) )
expect_error( outer2(x, Y, f) )

A = outer2(X, Y, dist2)
expect_equal(dim(A), c(m,n))
expect_equal(A, E)

sp = outer2_sp(X, Y, f, g = \(x,y) { TRUE })
B = sparseMatrix(sp$i, sp$j, x = sp$x, dims = c(sp$m, sp$n), symmetric = F, index1 = FALSE)
expect_all_true(sp$i >= 1)
expect_all_true(sp$i <= m)
expect_all_true(sp$j >= 1)
expect_all_true(sp$j <= n)
expect_equal(as.matrix(B), E)

sp = outer2_sp(X, Y, f, g)
B = sparseMatrix(sp$i, sp$j, x = sp$x, dims = c(sp$m, sp$n), symmetric = F, index1 = FALSE)
idx0 = which(E > 3, arr.ind = T)
idx1 = which(E <= 3, arr.ind = T)
expect_equal(B[idx1], E[idx1])
expect_all_equal(B[idx0], 0)

expect_equal(outer2_matvec(X, Y, f, a), as.numeric(E %*% a))
