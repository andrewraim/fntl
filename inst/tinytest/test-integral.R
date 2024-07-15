Rcpp::sourceCpp("cpp/test-integral.cpp")

mu = 0
sigma2 = 1

a = 0.5
b = 1
expected = integrate(f = function(x) { exp(-(x - mu)^2 / (2 * sigma2)) }, lower = a, upper = b)
computed = normal_integral(mu, sigma2, a, b)
expect_equal(computed$value, expected$value)
expect_equal(computed$subdivisions, expected$subdivisions)
expect_equal(computed$abs_error, expected$abs.error)
expect_equal(computed$message, expected$message)

a = 0
b = Inf
expected = 1/2 * sqrt(sigma2 * 2 * base::pi)
computed = normal_integral(mu, sigma2, a, b)
expect_equal(computed$value, expected)

a = -Inf
b = 0
expected = 1/2 * sqrt(sigma2 * 2 * base::pi)
computed = normal_integral(mu, sigma2, a, b)
expect_equal(computed$value, expected)

a = -Inf
b = Inf
expected = sqrt(sigma2 * 2 * base::pi)
computed = normal_integral(mu, sigma2, a, b)
expect_equal(computed$value, expected)
