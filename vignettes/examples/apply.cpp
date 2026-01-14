// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::List apply_ex(Rcpp::NumericMatrix X, Rcpp::IntegerMatrix Y,
	Rcpp::StringMatrix Z)
{
    std::function fx = [](double x) -> double { return std::pow(x, 2); };
    std::function gx = [](const Rcpp::NumericVector& x) -> double {
    	return Rcpp::sum(x);
    };

    std::function fy = [](int x) -> int { return std::pow(x, 2); };
	std::function gy = [](const Rcpp::IntegerVector& x) -> int {
		return Rcpp::sum(x);
	};

	// TBD: Consider breaking this example into several. One for our dense
	// containers and another for our sparse containers

	Rprintf("Checkpoint 1\n");

	// const fntl::mat<double>& Xmat = Rcpp::as<fntl::mat<double>>(X);
	// fntl::mat<double> Xmat(X);

	Rprintf("Checkpoint 1.1\n");
	// for (unsigned int i = 0; i < X.nrow(); i++) {
	// 	for (unsigned int j = 0; j < X.ncol(); j++) {
	// 		Rprintf("Xmat(%d,%d) = %g\n", i, j, Xmat(i,j));
	// 	}
	// }
	// Rcpp::NumericMatrix XX = Rcpp::wrap(Xmat);
	// Rcpp::print(XX);
	// fntl::mat<std::string> Zmat = Rcpp::as<fntl::mat<std::string>>(Z);

	Rprintf("Checkpoint 2\n");

    std::function fz = [](const std::string& x) -> int {
    	return x.length();
    };
	std::function gz = [](const std::vector<std::string>& x) -> int {
		size_t maxlen = 0;
		for (unsigned int i = 0; i < x.size(); i++) {
			maxlen = std::max(maxlen, x[i].size());
		}
		return maxlen;
	};

    fntl::mat<std::string> Zmat(Z);

    const fntl::mat<int>& z1 = fntl::mat_apply(Zmat, fz);
    const std::vector<int>& z2 = fntl::row_apply(Zmat, gz);
    const std::vector<int>& z3 = fntl::col_apply(Zmat, gz);

    Rcpp::IntegerMatrix Z1 = Rcpp::wrap(z1);
    Rcpp::IntegerVector Z2 = Rcpp::wrap(z2);
    Rcpp::IntegerVector Z3 = Rcpp::wrap(z3);

    Rprintf("Checkpoint 3\n");

    // const fntl::mat<int>& Z_len0 = fntl::mat_apply(Zmat, fz);
    // Rprintf("Checkpoint 3.1\n");
    // Rcpp::IntegerMatrix Z_len = Rcpp::wrap(Z_len0.x);
    // Rprintf("Checkpoint 3.2\n");
    // Z_len.attr("dim") = Rcpp::Dimension(Z.nrow(), Z.ncol());

    Rprintf("Checkpoint 4\n");

	// Rcpp::IntegerMatrix Z_len(Z.nrow(), Z.ncol());
	// Z_len.insert(Z_len.begin(), Z_len0.x.begin(), Z_len0.x.end());

    return Rcpp::List::create(
        Rcpp::Named("Xpows") = fntl::mat_apply(X, fx),
        Rcpp::Named("Xrowsums") = fntl::row_apply(X, gx),
        Rcpp::Named("Xcolsums") = fntl::col_apply(X, gx),
        Rcpp::Named("Ypows") = fntl::mat_apply(Y, fy),
        Rcpp::Named("Yrowsums") = fntl::row_apply(Y, gy),
        Rcpp::Named("Ycolsums") = fntl::col_apply(Y, gy),
        Rcpp::Named("Zlen") = Z1,
        Rcpp::Named("Zrowmaxlen") = Z2,
        Rcpp::Named("Zcolmaxlen") = Z3
    );
}
