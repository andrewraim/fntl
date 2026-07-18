#ifndef FNTL_UTIL_H
#define FNTL_UTIL_H

namespace fntl {

inline void logger(const char* fmt, ...)
{
	// Get the current time in the local time zone
	std::time_t raw = std::time(nullptr);
	std::tm* local = std::localtime(&raw);

	// Write formatted time to a string
	char buffer[64];
	strftime(buffer, 64, "%Y-%m-%d %H:%M:%S", local);

	// Insert "..." placeholders into format string for message; see
	// <https://stackoverflow.com/q/1056411>
	char msg[256];
	va_list args;
	va_start(args, fmt);
	vsnprintf(msg, 255, fmt, args);
	va_end(args);

	// Print the formatted message with timestamp
	Rprintf("%s - %s", buffer, msg);
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
