Rcpp::sourceCpp("../vignettes/examples/apply.cpp", rebuild = T)

X = matrix(rnorm(12), 4, 3)
Y = matrix(rpois(12, 10), 4, 3)
Z = matrix(LETTERS[1:12], 4, 3)

apply_ex(X, Y, Z)
