Rcpp::sourceCpp("../vignettes/examples/apply.cpp", rebuild = T)

set.seed(1234)

m = 4
n = 3

X = matrix(rnorm(m*n), m, n)
Y = matrix(rpois(m*n, 10), m, n)

Z = matrix(NA, m, n)
for (i in 1:m) {
	for (j in 1:n) {
		Z[i,j] = sample(LETTERS, size = rpois(1, lambda = 10), replace = F) |>
			paste(collapse = "")
	}
}

out1 = apply1_ex(X)
out2 = apply2_ex(Y)
out3 = apply3_ex(Z)
print(out1)
print(out2)
print(out3)
