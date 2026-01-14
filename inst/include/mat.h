#ifndef FNTL_MAT_H
#define FNTL_MAT_H

#include <complex>

namespace fntl {

template <typename T>
struct mat
{
	std::vector<T> x;
	unsigned int m = 0;
	unsigned int n = 0;

	mat() { };
	mat(unsigned int rows, unsigned int cols) : m(rows), n(cols) { };
	// mat(SEXP obj);

	// template <int RTYPE>
	// void init(const Rcpp::Matrix<RTYPE>& obj);

	template <int RTYPE>
	mat(const Rcpp::Matrix<RTYPE>& obj);

	// template <int RTYPE>
	operator SEXP() const;

	// Access an element. No searching is required here.
	const T& operator()(unsigned int row, unsigned int col) const {
		Rprintf("mat: size of x is %d\n", x.size());
		if (row >= m || col >= n) {
			 Rcpp::stop("Index out of bounds");
		}
		return x[row + col*m];
	}
};

template <typename T>
template <int RTYPE>
inline mat<T>::mat(const Rcpp::Matrix<RTYPE>& y)
{
	m = y.nrow();
	n = y.ncol();
	x.assign(y.begin(), y.end());
}

/*
template <typename T>
inline mat<T>::mat(SEXP obj)
{
	if (!Rf_isMatrix(obj)) {
		Rcpp::stop("Not a matrix");
	}

	init(obj);

	//if (TYPEOF(obj) == REALSXP) {
	//	Rprintf("Matrix is a NumericMatrix\n");
	//	init(Rcpp::as<Rcpp::NumericMatrix>(obj));
	// } else if (TYPEOF(obj) == INTSXP) {
	//	Rprintf("Matrix is a IntegerMatrix\n");
	//	init(Rcpp::as<Rcpp::IntegerMatrix>(obj));
	// } else if (TYPEOF(obj) == LGLSXP) {
	// 	init(Rcpp::as<Rcpp::LogicalMatrix>(obj));
	// } else if (TYPEOF(obj) == CPLXSXP) {
	//	init(Rcpp::as<Rcpp::ComplexMatrix>(obj));
	//	Rprintf("Matrix is a ComplexMatrix\n");
	// } else if (TYPEOF(obj) == STRSXP) {
	// 	init(Rcpp::as<Rcpp::CharacterMatrix>(obj));
	// } else if (TYPEOF(obj) == RAWSXP) {
	// 	init(Rcpp::as<Rcpp::RawMatrix>(obj));
	// } else if (TYPEOF(obj) == VECSXP) {
	// 	init(Rcpp::as<Rcpp::GenericMatrix>(obj));
	// } else {
	// 	Rcpp::stop("Unknown matrix type");
	// }

//	switch (TYPEOF(obj)) {
//		case REALSXP: init(Rcpp::as<Rcpp::NumericMatrix>(obj)); break;
//		default:      Rcpp::stop("Unknown matrix type"); break;
//	}
}
*/


/*
template <typename T>
inline mat<T>::mat(SEXP obj)
{
	if (!Rf_isMatrix(obj)) {
		Rcpp::stop("Not a matrix");
	}

	// Rcpp::stop("TYPEOF(obj) = %d, REALSXP = %d", TYPEOF(obj), REALSXP);

	if (TYPEOF(obj) == REALSXP) {
		Rprintf("REALSXP Checkpoint 1\n");
		const Rcpp::NumericMatrix& xx = Rcpp::as<Rcpp::NumericMatrix>(obj);
		Rprintf("REALSXP Checkpoint 2\n");
		// x = Rcpp::as<std::vector<T>>(xx);
		m = xx.nrow();
		Rprintf("REALSXP Checkpoint 3\n");
		n = xx.ncol();
		Rprintf("REALSXP Checkpoint 4\n");
		x.assign(xx.begin(), xx.end());
		Rprintf("REALSXP Checkpoint 5\n");
	} else {
		Rcpp::stop("Unknown matrix type");
	}

	// switch (TYPEOF(obj)) {
	// 	case REALSXP:
	// 		const Rcpp::NumericMatrix& xx = Rcpp::as<Rcpp::NumericMatrix>(obj);
	// 		// x = Rcpp::as<std::vector<T>>(xx);
	// 		m = xx.nrow();
	// 		n = xx.ncol();
	// 		// x.assign(xx.begin(), xx.end());
	// 		break;
	// 	default:
	// 		Rcpp::stop("Unknown matrix type");
	// 		break;
	// }
	//
	//		case INTSXP:
	//			return "IntegerMatrix (int)";
	//			break;
	//		case LGLSXP:
	//			return "LogicalMatrix (bool)";
	//			break;
	//		case STRSXP:
	//			return "CharacterMatrix (string)";
	//			break;
	//		case CPLXSXP:
	//			return "ComplexMatrix (complex)";
	//			break;
}
*/

/*
* Conversion operators to SEXP objects
*/

template<>
inline mat<double>::operator SEXP() const
{
	Rcpp::NumericVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<int>::operator SEXP() const
{
	Rcpp::IntegerVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<bool>::operator SEXP() const
{
	Rcpp::LogicalVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<std::complex<double>>::operator SEXP() const
{
	Rcpp::ComplexVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}

template<>
inline mat<std::string>::operator SEXP() const
{
	Rcpp::CharacterVector out = Rcpp::wrap(x);
	out.attr("dim") = Rcpp::Dimension(m, n);
	return out;
}


}

#endif
