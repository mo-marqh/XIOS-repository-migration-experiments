#ifndef __XMLIO_Configure__
#define __XMLIO_Configure__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"

/// /////////// Macros /////////// ///
#define DECLARE_PROPERTY(type, name, value) \
   extern type name; // = value

namespace xmlioserver
{
#include "properties.conf"
} // namespace xmlioserver

#endif // __XMLIO_Configure__
