#ifndef FNTL_UTIL_H
#define FNTL_UTIL_H

#include <type_traits>

namespace fntl {

/*
* Convert a "class enum" to its underlying type. This is especially handy in
* the present package for converting return codes to integers. This
* implementation was suggested by user Class Skeleton in a thread on
* [StackOverflow](https://stackoverflow.com/q/8357240).
*/
template <typename T>
constexpr auto to_underlying(T x) noexcept
{
    return static_cast<std::underlying_type_t<T>>(x);
}

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
