#include "cpptrace_default.hpp"
#ifdef CPPTRACE_GET_SYMBOLS_WITH_NOTHING

#include <cpptrace.hpp>
#include "symbols.hpp"

#include <vector>

namespace cpptrace {
    namespace detail {
        namespace nothing {
            std::vector<stacktrace_frame> resolve_frames(const std::vector<void*>& frames) {
                return std::vector<stacktrace_frame>(frames.size(), {
                    0,
                    0,
                    0,
                    "",
                    ""
                });
            }
        }
    }
}

#endif
