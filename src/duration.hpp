#ifndef __XMLIO_CDuration__
#define __XMLIO_CDuration__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      typedef long long int Time;
      class CCalendar;

      ///---------------------------------------------------------------

      struct CDuration
      {
         public: /* static */
            static CDuration FromString(const StdString& str);

         public:
            /// Opérateurs ///
            CDuration& operator=(const CDuration& duration);

            friend StdOStream& operator<<(StdOStream& out, const CDuration& duration);
            friend StdIStream& operator>>(StdIStream& in , CDuration& duration);

            /// Test ///
            bool isNone(void) const;

            /// Traitement ///
            CDuration& resolve(const CCalendar& calendar, bool noNegativeTime = false);
            CDuration& solveTimeStep(const CCalendar& c);
            /// Autres ///
            StdString toString(void) const;

            /// Propriétés publiques ///
            double year, month, day, hour, minute, second, timestep;
      };

      ///---------------------------------------------------------------

      const extern CDuration Year, Month,  Week,   Day,
                             Hour, Minute, Second, TimeStep, NoneDu;
} // namespace xios

#endif // __XMLIO_CDuration__
