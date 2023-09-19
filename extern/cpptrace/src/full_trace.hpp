#ifndef FULL_TRACE_HPP
#define FULL_TRACE_HPP

#include "cpptrace.hpp"
#include "common.hpp"

#include <cstddef>
#include <vector>

namespace cpptrace {
    namespace detail {
        CPPTRACE_FORCE_NO_INLINE
        std::vector<stacktrace_frame> generate_trace(size_t skip);
    }
}

#endif
