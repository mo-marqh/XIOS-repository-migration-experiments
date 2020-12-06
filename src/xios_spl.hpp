#ifndef __XIOS_SPL__
#define __XIOS_SPL__

/// standard C++ headers ///
#include <utility>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <memory>
// standard C
#include <cstring>
#include <cstdlib>

#include <cmath>
// bug in cmath header => macro collision marco with DOMAIN
#ifdef DOMAIN
#undef DOMAIN
#endif

#include <ctime>

// Conteneurs.
#include <vector>
#include <set>
#include <stack>
#include <list>
#include <map>
#include <deque>
#include <queue>
#include <valarray>
#include <tuple>

// Flux.
#include <iostream>
#include <fstream>
#include <sstream>

/// boost headers ///
#include <boost/cast.hpp>
#include <boost/current_function.hpp>

/// Map ///
#define xios_map std::map

/// Macro ///
#define UNUSED(parameter)

/// Définition de types (issus de la bibliothèque standard)///
typedef std::ostringstream StdOStringStream;
typedef std::istringstream StdIStringStream;
typedef std::stringstream  StdStringStream;
typedef std::ofstream      StdOFStream;
typedef std::ifstream      StdIFStream;
typedef std::ostream       StdOStream;
typedef std::istream       StdIStream;
typedef std::string        StdString;
typedef std::size_t        StdSize;

typedef  unsigned short int      ushort;
typedef  unsigned int            uint;
typedef  unsigned long int       ulong;
typedef  long long int           longlong;
typedef  unsigned long long int  ulonglong;

const size_t stringArrayLen=255 ;

/// XIOS headers ///
#include "configure.hpp"
#include "log.hpp"
using namespace std;
//using namespace boost ;

#endif //__XIOS_SPL__
