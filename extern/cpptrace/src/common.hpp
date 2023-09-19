#ifndef COMMON_HPP
#define COMMON_HPP

#ifdef _MSC_VER
#define CPPTRACE_FORCE_NO_INLINE __declspec(noinline)
#define CPPTRACE_PFUNC __FUNCSIG__
#define CPPTRACE_MAYBE_UNUSED
#pragma warning(push)
#pragma warning(disable: 4505) // Unused local function
#else
#define CPPTRACE_FORCE_NO_INLINE __attribute__((noinline))
#define CPPTRACE_PFUNC __extension__ __PRETTY_FUNCTION__
#define CPPTRACE_MAYBE_UNUSED __attribute__((unused))
#endif

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <ios>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <memory>
#include <new>

#define IS_WINDOWS 0
#define IS_LINUX 0
#define IS_APPLE 0

#if defined(_WIN32)
 #undef IS_WINDOWS
 #define IS_WINDOWS 1
#elif defined(__linux)
 #undef IS_LINUX
 #define IS_LINUX 1
#elif defined(__APPLE__)
 #undef IS_APPLE
 #define IS_APPLE 1
#else
 #error "Unexpected platform"
#endif

#define IS_CLANG 0
#define IS_GCC 0
#define IS_MSVC 0

#if defined(__clang__)
 #undef IS_CLANG
 #define IS_CLANG 1
#elif defined(__GNUC__) || defined(__GNUG__)
 #undef IS_GCC
 #define IS_GCC 1
#elif defined(_MSC_VER)
 #undef IS_MSVC
 #define IS_MSVC 1
#else
 #error "Unsupported compiler"
#endif

#if IS_WINDOWS
 #include <windows.h>
#else
 #include <sys/stat.h>
#endif

// Lightweight std::source_location.
struct source_location {
    // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
    const char* const file;
    //const char* const function; // disabled for now due to static constexpr restrictions
    // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
    const int line;
    constexpr source_location(
        //const char* _function /*= __builtin_FUNCTION()*/,
//        const char* _file     = __builtin_FILE(),
//        int _line             = __builtin_LINE()
        const char* _file     = __FILE__,
        int _line             = __LINE__
    ) : file(_file), /*function(_function),*/ line(_line) {}
};

CPPTRACE_MAYBE_UNUSED
static void primitive_assert_impl(
    bool condition,
    bool verify,
    const char* expression,
    const char* signature,
    source_location location,
    const char* message = nullptr
) {
    if(!condition) {
        const char* action = verify ? "verification" : "assertion";
        const char* name   = verify ? "verify"       : "assert";
        if(message == nullptr) {
            (void) fprintf(
                stderr,
                "Cpptrace %s failed at %s:%d: %s\n",
                action, location.file, location.line, signature
            );
        } else {
            (void) fprintf(
                stderr,
                "Cpptrace %s failed at %s:%d: %s: %s\n",
                action, location.file, location.line, signature, message
            );
        }
        (void) fprintf(stderr, "    primitive_%s(%s);\n", name, expression);
        std::abort();
    }
}

template<typename T>
void nothing() {}
#define PHONY_USE(E) (nothing<decltype(E)>())

// Still present in release mode, nonfatal
#define internal_verify(c, ...) primitive_assert_impl(c, true, #c, CPPTRACE_PFUNC, {}, ##__VA_ARGS__)

#ifndef NDEBUG
    #define CPPTRACE_PRIMITIVE_ASSERT(c, ...) \
    primitive_assert_impl(c, false, #c, CPPTRACE_PFUNC, {}, ##__VA_ARGS__)
#else
    #define CPPTRACE_PRIMITIVE_ASSERT(c, ...) PHONY_USE(c)
#endif

CPPTRACE_MAYBE_UNUSED
static std::vector<std::string> split(const std::string& str, const std::string& delims) {
    std::vector<std::string> vec;
    size_t old_pos = 0;
    size_t pos = 0;
    while((pos = str.find_first_of(delims, old_pos)) != std::string::npos) {
        vec.emplace_back(str.substr(old_pos, pos - old_pos));
        old_pos = pos + 1;
    }
    vec.emplace_back(str.substr(old_pos));
    return vec;
}

template<typename C>
CPPTRACE_MAYBE_UNUSED
static std::string join(const C& container, const std::string& delim) {
    auto iter = std::begin(container);
    auto end = std::end(container);
    std::string str;
    if(std::distance(iter, end) > 0) {
        str += *iter;
        while(++iter != end) {
            str += delim;
            str += *iter;
        }
    }
    return str;
}

constexpr const char* const whitespace = " \t\n\r\f\v";

CPPTRACE_MAYBE_UNUSED
static std::string trim(const std::string& str) {
    if(str.empty()) {
        return "";
    }
    const size_t left = str.find_first_not_of(whitespace);
    const size_t right = str.find_last_not_of(whitespace) + 1;
    return str.substr(left, right - left);
}

CPPTRACE_MAYBE_UNUSED
static std::string to_hex(uintptr_t addr) {
    std::stringstream sstream;
    sstream<<std::hex<<addr;
    return std::move(sstream).str();
}

CPPTRACE_MAYBE_UNUSED
static bool is_little_endian() {
    uint16_t num = 0x1;
    auto* ptr = (uint8_t*)&num;
    return ptr[0] == 1;
}

// Modified from
// https://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
template<typename T, size_t N>
struct byte_swapper;

template<typename T>
struct byte_swapper<T, 1> {
    T operator()(T val) {
        return val;
    }
};

template<typename T>
struct byte_swapper<T, 2> {
    T operator()(T val) {
        return ((((val) >> 8) & 0xff) | (((val) & 0xff) << 8));
    }
};

template<typename T>
struct byte_swapper<T, 4> {
    T operator()(T val) {
        return ((((val) & 0xff000000) >> 24) |
                (((val) & 0x00ff0000) >>  8) |
                (((val) & 0x0000ff00) <<  8) |
                (((val) & 0x000000ff) << 24));
    }
};

template<typename T>
struct byte_swapper<T, 8> {
    T operator()(T val) {
        return ((((val) & 0xff00000000000000ull) >> 56) |
                (((val) & 0x00ff000000000000ull) >> 40) |
                (((val) & 0x0000ff0000000000ull) >> 24) |
                (((val) & 0x000000ff00000000ull) >> 8 ) |
                (((val) & 0x00000000ff000000ull) << 8 ) |
                (((val) & 0x0000000000ff0000ull) << 24) |
                (((val) & 0x000000000000ff00ull) << 40) |
                (((val) & 0x00000000000000ffull) << 56));
    }
};

template<typename T, typename std::enable_if<std::is_integral<T>::value, int>::type = 0>
T byteswap(T value) {
    return byte_swapper<T, sizeof(T)>{}(value);
}

CPPTRACE_MAYBE_UNUSED
inline void enable_virtual_terminal_processing_if_needed() {
    // enable colors / ansi processing if necessary
    #if IS_WINDOWS
     // https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#example-of-enabling-virtual-terminal-processing
     #ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
      constexpr DWORD ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x4;
     #endif
     HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
     DWORD dwMode = 0;
     if(hOut == INVALID_HANDLE_VALUE) return;
     if(!GetConsoleMode(hOut, &dwMode)) return;
     if(dwMode != (dwMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING))
     if(!SetConsoleMode(hOut, dwMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) return;
    #endif
}

CPPTRACE_MAYBE_UNUSED
// NOLINTNEXTLINE(misc-no-recursion)
inline constexpr unsigned n_digits(unsigned value) {
    return value < 10 ? 1 : 1 + n_digits(value / 10);
}
static_assert(n_digits(1) == 1, "n_digits utility producing the wrong result");
static_assert(n_digits(9) == 1, "n_digits utility producing the wrong result");
static_assert(n_digits(10) == 2, "n_digits utility producing the wrong result");
static_assert(n_digits(11) == 2, "n_digits utility producing the wrong result");
static_assert(n_digits(1024) == 4, "n_digits utility producing the wrong result");

// TODO: Re-evaluate use of off_t
template<typename T, typename std::enable_if<std::is_pod<T>::value, int>::type = 0>
T load_bytes(FILE* obj_file, off_t offset) {
    T object;
    internal_verify(fseek(obj_file, offset, SEEK_SET) == 0, "fseek error");
    internal_verify(fread(&object, sizeof(T), 1, obj_file) == 1, "fread error");
    return object;
}

class file_error : std::exception {
    const char* what() const noexcept override {
        return "Unable to read file";
    }
};

struct nullopt_t {};

static constexpr nullopt_t nullopt;

template<typename T, typename std::enable_if<!std::is_same<typename std::decay<T>::type, void>::value, int>::type = 0>
class optional {
    bool holds_value = false;

    union {
        T uvalue;
    };

public:
    // clang-tidy false positive
    // NOLINTNEXTLINE(modernize-use-equals-default)
    optional() noexcept {}

    optional(nullopt_t) noexcept {}

    ~optional() {
        reset();
    }

    optional(const optional& other) : holds_value(other.holds_value) {
        if(holds_value) {
            new (static_cast<void*>(std::addressof(uvalue))) T(other.uvalue);
        }
    }

    optional(optional&& other) noexcept(std::is_nothrow_move_constructible<T>::value) : holds_value(other.holds_value) {
        if(holds_value) {
            new (static_cast<void*>(std::addressof(uvalue))) T(std::move(other.uvalue));
        }
    }

    optional& operator=(const optional& other) {
        optional copy(other);
        swap(*this, copy);
        return *this;
    }

    optional& operator=(optional&& other) noexcept(
        std::is_nothrow_move_assignable<T>::value && std::is_nothrow_move_constructible<T>::value
    ) {
        reset();
        if(other.holds_value) {
            new (static_cast<void*>(std::addressof(uvalue))) T(std::move(other.uvalue));
            holds_value = true;
        }
        return *this;
    }

    template<
        typename U = T,
        typename std::enable_if<!std::is_same<typename std::decay<U>::type, optional<T>>::value, int>::type = 0
    >
    // clang-tidy false positive
    // NOLINTNEXTLINE(bugprone-forwarding-reference-overload)
    optional(U&& value) : holds_value(true) {
        new (static_cast<void*>(std::addressof(uvalue))) T(std::forward<U>(value));
    }

    template<
        typename U = T,
        typename std::enable_if<!std::is_same<typename std::decay<U>::type, optional<T>>::value, int>::type = 0
    >
    optional& operator=(U&& value) {
        if(holds_value) {
            uvalue = std::forward<U>(value);
        } else {
            new (static_cast<void*>(std::addressof(uvalue))) T(std::forward<U>(value));
            holds_value = true;
        }
        return *this;
    }

    optional& operator=(nullopt_t) noexcept {
        reset();
        return *this;
    }

    void swap(optional& other) {
        if(holds_value && other.holds_value) {
            std::swap(uvalue, other.uvalue);
        } else if(holds_value && !other.holds_value) {
            new (&other.uvalue) T(std::move(uvalue));
            uvalue.~T();
        } else if(!holds_value && other.holds_value) {
            new (static_cast<void*>(std::addressof(uvalue))) T(std::move(other.uvalue));
            other.uvalue.~T();
        }
        std::swap(holds_value, other.holds_value);
    }

    bool has_value() const {
        return holds_value;
    }

    operator bool() const {
        return holds_value;
    }

    void reset() {
        if(holds_value) {
            uvalue.~T();
        }
        holds_value = false;
    }

    T& unwrap() & {
        if(!holds_value) {
            throw std::runtime_error{"Optional does not contain a value"};
        }
        return uvalue;
    }

    const T& unwrap() const & {
        if(!holds_value) {
            throw std::runtime_error{"Optional does not contain a value"};
        }
        return uvalue;
    }

    T&& unwrap() && {
        if(!holds_value) {
            throw std::runtime_error{"Optional does not contain a value"};
        }
        return std::move(uvalue);
    }

    const T&& unwrap() const && {
        if(!holds_value) {
            throw std::runtime_error{"Optional does not contain a value"};
        }
        return std::move(uvalue);
    }

    template<typename U>
    T value_or(U&& default_value) const & {
        return holds_value ? uvalue : static_cast<T>(std::forward<U>(default_value));
    }

    template<typename U>
    T value_or(U&& default_value) && {
        return holds_value ? std::move(uvalue) : static_cast<T>(std::forward<U>(default_value));
    }
};

// shamelessly stolen from stackoverflow
CPPTRACE_MAYBE_UNUSED
static bool directory_exists(const std::string& path) {
    #if IS_WINDOWS
    DWORD dwAttrib = GetFileAttributesA(path.c_str());
    return dwAttrib != INVALID_FILE_ATTRIBUTES && (dwAttrib & FILE_ATTRIBUTE_DIRECTORY);
    #else
    struct stat sb;
    return stat(path.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode);
    #endif
}

CPPTRACE_MAYBE_UNUSED
static std::string basename(const std::string& path) {
    // Assumes no trailing /'s
    auto pos = path.rfind('/');
    if(pos == std::string::npos) {
        return path;
    } else {
        return path.substr(pos + 1);
    }
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif
