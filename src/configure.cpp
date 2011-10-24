#define __XMLIO_Configure__ // < Ne pas supprimer

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"

/// /////////// Macros /////////// ///
#undef  DECLARE_PROPERTY
#define DECLARE_PROPERTY(type, name, value) \
   type name = value;

namespace xmlioserver
{
#include "properties.conf"
} // namespace xmlioserver
