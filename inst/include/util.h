#ifndef FNTL_UTIL_H
#define FNTL_UTIL_H

namespace fntl {

inline std::string paste(const Rcpp::StringVector& x, const std::string& delim)
{
	std::string out;
	unsigned int n = x.size();
	for (unsigned int i = 0; i < n; i++) {
		if (i > 0) {
			out += delim + x(i);
		} else {
			out += x(i);
		}
	}

	return out;
}

}

#endif
