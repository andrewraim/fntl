#ifndef FNTL_TYPEDEFS_H
#define FNTL_TYPEDEFS_H

namespace fntl {

static const std::vector<std::string> integrate_messages = {
	"OK",
	"maximum number of subdivisions reached",
	"roundoff error was detected",
	"extremely bad integrand behaviour",
	"roundoff error is detected in the extrapolation table",
	"the integral is probably divergent",
	"the input is invalid"
};

static const std::vector<std::string> optimize_messages = {
	"OK",
	"Numerical overflow: tol may be too small",
	"Not converged within maxiter iterations"
};

static const std::vector<std::string> findroot_messages = {
	"OK",
	"Numerical overflow: tol may be too small",
	"Not converged within maxiter iterations"
};

typedef std::array<unsigned int, 2> coord2_t;

typedef std::function<double(double)> dfd;

typedef std::function<double(double,bool)> density;
typedef std::function<double(double,bool,bool)> cdf;
typedef std::function<double(double,bool,bool)> quantile;

static const double mach_eps = std::numeric_limits<double>::epsilon();
static const double mach_eps_2r = sqrt(mach_eps);
static const double mach_eps_4r = std::pow(mach_eps, 0.25);
static const unsigned int uint_max = std::numeric_limits<unsigned int>::max();

enum class fd_types : unsigned int {
	SYMMETRIC = 0L,
	FORWARD = 1L,
	BACKWARD = 2L
};

enum class neldermead_status : unsigned int {
	OK = 0L,
	NOT_CONVERGED = 1L,
	SIMLEX_DEGENERACY = 10L
};

enum class cg_status : unsigned int {
	OK = 0L,
	NOT_CONVERGED = 1L
};

enum class bfgs_status : unsigned int {
	OK = 0L,
	NOT_CONVERGED = 1L
};

enum class lbfgsb_status : unsigned int {
	OK = 0L,
	NOT_CONVERGED = 1L,
	WARN = 51L,
	ERROR = 52L,
};

enum class nlm_status : unsigned int {
	OK = 0L,
	GRADIENT_WITHIN_TOL = 1L,
	ITERATES_WITH_TOL = 2L,
	NO_LOWER_STEP = 3L,
	ITERATION_MAX = 4L,
	STEP_SIZE_EXCEEDED = 5L
};

enum class error_action : unsigned int {
	STOP = 3L,
	WARNING = 2L,
	MESSAGE = 1L,
	NONE = 0L
};

enum class findroot_status : unsigned int {
	OK = 0L,
	NUMERICAL_OVERFLOW = 1L,
	NOT_CONVERGED = 2L
};

enum class optimize_status : unsigned int {
	OK = 0L,
	NUMERICAL_OVERFLOW = 1L,
	NOT_CONVERGED = 2L
};

enum class integrate_status : int {
	OK = 0L,
	MAX_SUBDIVISIONS = 1L,
	ROUNDOFF_ERROR = 2L,
	BAD_INTEGRAND_BEHAVIOR = 3L,
	ROUNDOFF_ERROR_EXTRAPOLATION_TABLE = 4L,
	PROBABLY_DIVERGENT_INTEGRAL = 5L,
	INVALID_INPUT = 6L
};

enum class richardson_status : unsigned int {
	OK = 0L,
	NOT_CONVERGED = 1L,
	NUMERICAL_PRECISION = 2L
};


}

#endif
