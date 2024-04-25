
#ifndef __BOOST_EXTRACT_HPP__
#define __BOOST_EXTRACT_HPP__

#include <string>
#include <regex>
#include <utility>
#include <algorithm>
#include <cassert>


#if defined( XIOS_DISABLE_CURRENT_FUNCTION )

# define XIOS_CURRENT_FUNCTION "(unknown)"

#elif defined(__GNUC__) || (defined(__MWERKS__) && (__MWERKS__ >= 0x3000)) || (defined(__ICC) && (__ICC >= 600)) || defined(__ghs__)

# define XIOS_CURRENT_FUNCTION __PRETTY_FUNCTION__

#elif defined(__DMC__) && (__DMC__ >= 0x810)

# define XIOS_CURRENT_FUNCTION __PRETTY_FUNCTION__

#elif defined(__FUNCSIG__)

# define XIOS_CURRENT_FUNCTION __FUNCSIG__

#elif (defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 600)) || (defined(__IBMCPP__) && (__IBMCPP__ >= 500))

# define XIOS_CURRENT_FUNCTION __FUNCTION__

#elif defined(__BORLANDC__) && (__BORLANDC__ >= 0x550)

# define XIOS_CURRENT_FUNCTION __FUNC__

#elif defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901)

# define XIOS_CURRENT_FUNCTION __func__

#elif defined(__cplusplus) && (__cplusplus >= 201103)

# define XIOS_CURRENT_FUNCTION __func__

#else

# define XIOS_CURRENT_FUNCTION "(unknown)"

#endif


template <class Target,class Source>
inline Target xios_polymorphic_downcast(Source* x)
{
    //BOOST_ASSERT( dynamic_cast<Target>(x) == x );  // detect logic error
    assert( dynamic_cast<Target>(x) == x );  // detect logic error
    return static_cast<Target>(x);
}

inline std::string xios_trim_copy(const std::string &s) {
    std::regex pattern("^\\s+|\\s+$");
    return std::regex_replace(s, pattern, "");
}

inline std::string xios_to_lower_copy(const std::string& input,
    const std::locale& loc = std::locale())
{
    auto const& facet = std::use_facet<std::ctype<char>>(loc);

    std::string out;
    out.reserve(input.size());

    std::transform(input.begin(), input.end(), std::back_inserter(out),
        [&facet](unsigned char c) { return facet.tolower(c); });

    return out;
}

template <class SequenceSequenceT>
inline void xios_split(SequenceSequenceT& retvalue, std::string value, std::string del)
{
    //std::vector<std::string> v;
    const char* delimiter( del.c_str() );
    std::string accum;
    for (auto f = begin(value), l = end(value); f != l;) {
        while (f!=l && *f==delimiter[0]) {
            ++f;
	    retvalue.push_back(std::move(accum));
            accum="";
            //std::swap(accum, v.emplace_back());
        }
        while (f != l && *f != delimiter[0])
            accum += *f++;
    }
    retvalue.push_back(std::move(accum));
}

inline void xios_replace_all(std::string & data, std::string toSearch, std::string replaceStr)
{
    // Get the first occurrence
    size_t pos = data.find(toSearch);
    // Repeat till end is reached
    while( pos != std::string::npos)
    {
        // Replace this occurrence of Sub String
        data.replace(pos, toSearch.size(), replaceStr);
        // Get the next occurrence from the current position
        pos =data.find(toSearch, pos + replaceStr.size());
    }
}


#endif // __BOOST_EXTRACT_HPP__
