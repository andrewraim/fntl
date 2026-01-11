library(Matrix)
Rcpp::sourceCpp("temp.cpp", rebuild = TRUE)

set.seed(1234)

x = matrix(rnorm(12), 4, 3)
y = matrix(rnorm(12), 2, 3)

print(x)
print(y)

out = temp_ex(x, y)
ds1 = out$ds
ds2 = out$ds2
sp0 = out$sp0
sp1 = out$sp1
sp2 = out$sp2
sp3 = out$sp3
Matrix::sparseMatrix(i = sp0$i, p = sp0$p, x = sp0$x, dims = c(sp0$m, sp0$n))
Matrix::sparseMatrix(i = sp1$i, p = sp1$p, x = sp1$x, dims = c(sp1$m, sp1$n))
Matrix::sparseMatrix(i = sp2$i, j = sp2$j, x = sp2$x, dims = c(sp2$m, sp2$n))

# I don't think this is taking csr format as input ...
Matrix::sparseMatrix(i = sp3$i, p = sp3$p, x = sp3$x, dims = c(sp3$m, sp3$n), repr = "R")

