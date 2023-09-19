#ifndef DEMANGLE_HPP
#define DEMANGLE_HPP

#include "cpptrace_default.hpp"

#include <string>

namespace cpptrace {
    namespace detail {
        std::string demangle(const std::string&);
    }
}

#endif
