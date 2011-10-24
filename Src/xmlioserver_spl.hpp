#ifndef __XMLIO_SPL__
#define __XMLIO_SPL__

/// standard C++ headers ///
#include <utility>
#include <string>
#include <algorithm>

// standard C
#include <cstring>
#include <cstdlib>
#include <cmath>
#include <ctime>

// Conteneurs.
#include <vector>
#include <set>
#include <stack>
#include <list>
#include <map>
#include <deque>

// Flux.
#include <iostream>
#include <fstream>
#include <sstream>

/// boost headers ///
//#include <boost/unordered_map.hpp>

/// Map ///
#define xios_map std::map

/// Macro ///
#define UNUSED(parameter)

/// Définition de types (issus de la bibliothèque standard)///
typedef std::ostringstream StdOStringStream;
typedef std::istringstream StdIStringStream;
typedef std::ofstream      StdOFStream;
typedef std::ifstream      StdIFStream;
typedef std::ostream       StdOStream;
typedef std::istream       StdIStream;
typedef std::string        StdString;
typedef std::size_t        StdSize;

/// xmlioserver headers ///
#include "configure.hpp"

#endif //__XMLIO_SPL__
