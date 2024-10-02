#ifndef FNTL_RESULT_H
#define FNTL_RESULT_H

/*
* This code follows a specific structure so that we can use the `as` and `wrap`
* constructs to serialize between structs and Rcpp Lists. The structs are
* defined first without Rcpp included yet, then Rcpp is included and the
* implementations for serialization operations are give.
*
* The pattern we follow here is referred to as "intrusive" (rather than
* "non-intrusive") because `wrap` and `as` are defined via member functions.
*
* See the following articles:
* <https://gallery.rcpp.org/articles/custom-templated-wrap-and-as-for-seamingless-interfaces>
* <https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-extending.pdf>
*
* The issue also came up in Stack\ Overflow threads such as:
* <https://stackoverflow.com/questions/51110244/in-rcpp-how-to-get-a-user-defined-structure-from-c-into-r>
* <https://stackoverflow.com/questions/74887786/specialising-rcppas-for-stdarray>
*/


#include <RcppCommon.h>
#include "typedefs.h"
#include "typedefs-rcpp.h"
#include "util.h"

namespace fntl {

struct fd_deriv_result
{
	double value;
	double err;
	unsigned int iter;

	operator SEXP() const;
};

struct gradient_result
{
	std::vector<double> value;
	std::vector<double> err;
	std::vector<unsigned int> iter;

	operator SEXP() const;
};

struct hessian_result
{
	std::vector<double> value;
	std::vector<double> err;
	std::vector<unsigned int> iter;
	double dim;

	operator SEXP() const;
};

struct jacobian_result
{
	std::vector<double> value;
	std::vector<double> err;
	std::vector<unsigned int> iter;
	double rows;
	double cols;

	operator SEXP() const;
};

struct neldermead_result
{
	std::vector<double> par;
	double value;
	neldermead_status status;
	int fncount;

	operator SEXP() const;
};

struct cg_result
{
	std::vector<double> par;
	double value;
	cg_status status;
	int fncount;
	int grcount;

	operator SEXP() const;
};

struct bfgs_result
{
	std::vector<double> par;
	double value;
	bfgs_status status;
	int fncount;
	int grcount;

	operator SEXP() const;
};

struct lbfgsb_result
{
	std::vector<double> par;
	double value;
	lbfgsb_status status;
	int fncount;
	int grcount;
	std::string message;

	operator SEXP() const;
};

struct nlm_result
{
	std::vector<double> par;
	std::vector<double> grad;
	double estimate;
	int iterations;
	nlm_status status;
	std::vector<double> hessian;

	operator SEXP() const;
};

struct findroot_result
{
	double root;
	double f_root;
	unsigned int iter;
	double tol;
	findroot_status status;
	std::string message;

	operator SEXP() const;
};

struct optimize_result
{
	double par;
	double value;
	unsigned int iter;
	double tol;
	optimize_status status;
	std::string message;

	operator SEXP() const;
};

struct integrate_result
{
	double value;
	double abs_error;
	int subdivisions;
	integrate_status status;
	int n_eval;
	std::string message;

	operator SEXP() const;
};

struct richardson_result
{
	double value;
	double err;
	unsigned int iter;
	richardson_status status;

	operator SEXP() const;
};

}

#include <Rcpp.h>

namespace fntl {

/*
* Conversation operators to SEXP objects
*/

inline fd_deriv_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("value") = value,
		Rcpp::Named("err") = err,
		Rcpp::Named("iter") = iter
	);
}

inline gradient_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("value") = value,
		Rcpp::Named("err") = err,
		Rcpp::Named("iter") = iter
	);
}

inline hessian_result::operator SEXP() const
{
	Rcpp::NumericMatrix value_mat(dim, dim);
	Rcpp::NumericMatrix err_mat(dim, dim);
	Rcpp::IntegerMatrix iter_mat(dim, dim);

	// Unpack as lower triangular of symmetric matrices
	unsigned int idx = 0;
	for (unsigned int j = 0; j < dim; j++) {
		value_mat(j,j) = value[idx];
		err_mat(j,j) = err[idx];
		iter_mat(j,j) = iter[idx];
		idx++;

		for (unsigned int i = j+1; i < dim; i++) {
			value_mat(i,j) = value[idx];
			err_mat(i,j) = err[idx];
			iter_mat(i,j) = iter[idx];

			value_mat(j,i) = value_mat(i,j);
			err_mat(j,i) = err_mat(i,j);
			iter_mat(j,i) = iter_mat(i,j);

			idx++;
		}
	}

	return Rcpp::List::create(
		Rcpp::Named("value") = value_mat,
		Rcpp::Named("err") = err_mat,
		Rcpp::Named("iter") = iter_mat
	);
}

inline jacobian_result::operator SEXP() const
{
	Rcpp::NumericMatrix value_mat(rows, cols);
	Rcpp::NumericMatrix err_mat(rows, cols);
	Rcpp::IntegerMatrix iter_mat(rows, cols);

	for (unsigned int i = 0; i < rows; i++) {
		for (unsigned int j = 0; j < cols; j++) {
			value_mat(i,j) = value[i*rows + j];
			err_mat(i,j) = err[i*rows + j];
			iter_mat(i,j) = iter[i*rows + j];
		}
	}

	return Rcpp::List::create(
		Rcpp::Named("value") = value_mat,
		Rcpp::Named("err") = err_mat,
		Rcpp::Named("iter") = iter_mat
	);
}

inline optimize_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("value") = value,
		Rcpp::Named("iter") = iter,
		Rcpp::Named("tol") = tol,
		Rcpp::Named("status") = to_underlying(status),
		Rcpp::Named("message") = message
	);
}

inline neldermead_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("value") = value,
		Rcpp::Named("fncount") = fncount,
		Rcpp::Named("status") = to_underlying(status)
	);
}

inline cg_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("value") = value,
		Rcpp::Named("fncount") = fncount,
		Rcpp::Named("grcount") = grcount,
		Rcpp::Named("status") = to_underlying(status)
	);
}

inline bfgs_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("value") = value,
		Rcpp::Named("fncount") = fncount,
		Rcpp::Named("grcount") = grcount,
		Rcpp::Named("status") = to_underlying(status)
	);
}

inline lbfgsb_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("value") = value,
		Rcpp::Named("fncount") = fncount,
		Rcpp::Named("grcount") = grcount,
		Rcpp::Named("status") = to_underlying(status),
		Rcpp::Named("message") = message
	);
}

inline nlm_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("par") = par,
		Rcpp::Named("grad") = grad,
		Rcpp::Named("estimate") = estimate,
		Rcpp::Named("iterations") = iterations,
		Rcpp::Named("status") = to_underlying(status),
		Rcpp::Named("hessian") = hessian
	);
};

inline findroot_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("root") = root,
		Rcpp::Named("f_root") = f_root,
		Rcpp::Named("iter") = iter,
		Rcpp::Named("tol") = tol,
		Rcpp::Named("status") = to_underlying(status),
		Rcpp::Named("message") = message
	);
}

inline integrate_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("value") = value,
		Rcpp::Named("subdivisions") = subdivisions,
		Rcpp::Named("n_eval") = n_eval,
		Rcpp::Named("abs_error") = abs_error,
		Rcpp::Named("status") = to_underlying(status),
		Rcpp::Named("message") = message
	);
}

inline richardson_result::operator SEXP() const
{
	return Rcpp::List::create(
		Rcpp::Named("value") = value,
		Rcpp::Named("err") = err,
		Rcpp::Named("iter") = iter,
		Rcpp::Named("status") = to_underlying(status)
	);
}

}

#endif
