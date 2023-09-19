#ifdef CPPTRACE_GET_SYMBOLS_WITH_LIBDL

#include "cpptrace.hpp"
#include "symbols.hpp"

#include <cstdint>
#include <memory>
#include <vector>

#include <dlfcn.h>

namespace cpptrace {
    namespace detail {
        namespace libdl {
            stacktrace_frame resolve_frame(const void* addr) {
                Dl_info info;
                if(dladdr(addr, &info)) { // thread-safe
                    return {
                        reinterpret_cast<uintptr_t>(addr),
                        0,
                        0,
                        info.dli_fname ? info.dli_fname : "",
                        info.dli_sname ? info.dli_sname : ""
                    };
                } else {
                    return {
                        reinterpret_cast<uintptr_t>(addr),
                        0,
                        0,
                        "",
                        ""
                    };
                }
            }

            std::vector<stacktrace_frame> resolve_frames(const std::vector<void*>& frames) {
                std::vector<stacktrace_frame> trace;
                trace.reserve(frames.size());
                for(const void* frame : frames) {
                    trace.push_back(resolve_frame(frame));
                }
                return trace;
            }
        }
    }
}

#endif
