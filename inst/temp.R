library(Matrix)
Rcpp::sourceCpp("inst/temp.cpp", rebuild = TRUE)

xx = array(1:24, dim = c(2,3,4))
temp_ex(xx)

