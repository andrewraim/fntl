#ifndef FNTL_METROPOLIS_H
#define FNTL_METROPOLIS_H

#include <Rcpp.h>

namespace fntl {

template<typename T>
std::vector<T> metropolis(
	unsigned int n,
	const T& init,
	const std::function<double(T)>& log_eval,
	const std::function<T(T)>& draw,
	unsigned int burn = 0,
	unsigned int thin = 1,
	unsigned int report = 1e7)
{
	unsigned int n_keep = std::ceil((n - burn) / thin);
	unsigned int t_keep = 0;
	unsigned int rejects = 0;

	T x = init;
	double log_prev = log_eval(x);
	std::vector<T> out(n_keep);

	for (unsigned int t = 0; t < n; t++)
	{
		/* TBD
		*
		* - Check for interrupt
		* - Is it worth making an args struct for this function?
		*/

		const T& x0 = draw(x);
		double u = R::runif(0, 1);
		double log_num = log_prev;
		double log_den = log_eval(x);
		const double log_ratio = std::min(log_num - log_den, 0.0);
		if (std::log(u) < log_ratio) {
			x = x0;
			log_prev = log_eval(x);
		} else {
			rejects++;
		}

		if (t >= burn && (t+1) % thin == 0) {
			out(t_keep) = x;
			t_keep++;
		}

		if ((t+1) % report == 0) {
			printf("[%d] rejected %0.2f%%\n", t+1, rejects / double(t+1) * 100);
			Rcpp::checkUserInterrupt();
		}

	}

	return out;
}

}

#endif
