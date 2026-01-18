// [[Rcpp::depends(fntl)]]
#include "fntl.h"

// [[Rcpp::export]]
Rcpp::IntegerVector temp_ex(const Rcpp::IntegerVector& xx)
{
	Rprintf("Begin\n");

	fntl::mdarray<int, 3> x(xx);
	// fntl::mdarray<int, 3> x({ 2, 2, 3 });

	fntl::coord_t<3> dim = x.dim();

	for (unsigned int k = 0; k < dim[2]; k++) {
		for (unsigned int j = 0; j < dim[1]; j++) {
			for (unsigned int i = 0; i < dim[0]; i++) {
				x({i, j, k}) *= 10;
			}
		}
	}

	Rcpp::print(x.to_Vector<INTSXP>());


	fntl::mdarray<int, 3> arr(xx);

	std:: function f = [](const fntl::mdarray<int,1>& x) -> int
	{
		int out = 0;

		fntl::coord_t<1> dim = x.dim();
		Rprintf("dim[0] = %d\n", dim[0]);
		for (unsigned int i = 0; i < dim[0]; i++) {
			out += x({i});
		}

		Rprintf("sum = %d\n", out);
		return out;
	};
	fntl::coord_t<2> margins = {2, 3};
	const fntl::mdarray<int,2>& out = apply(arr, margins, f);

	return out.to_Vector<INTSXP>();
}
