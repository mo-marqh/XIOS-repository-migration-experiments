#ifndef __XMLIO_CDate__
#define __XMLIO_CDate__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "duration.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CCalendar;

      class CDate
      {
         public :

            /// Constructeurs ///
            CDate(void);                      // Not implemented yet
            CDate(const CCalendar & cal,
                  int yr = 0, int mth = 1, int d   = 1,
                  int hr = 0, int min = 0, int sec = 0);
            CDate(const CDate & odate);
            CDate(const CDate * const odate); // Not implemented yet

            /// Destructeur ///
            ~CDate(void);

            /// Opérateurs ///
            CDate & operator=(const CDate & date);
            friend StdOStream & operator<<(StdOStream & out, const CDate & date);
            friend StdIStream & operator>>(StdIStream & in, CDate & date); // Non testée.

            operator Time(void);  // Retourne le nombre de secondes écoulées depuis la date initiale définie dans le calendrier.

            /// Traitements ///
            bool checkDate(void); // Vérifie la validité de la date.

            /// Divers accesseurs ///
            int getYear  (void) const;
            int getMonth (void) const;
            int getDay   (void) const;
            int getHour  (void) const;
            int getMinute(void) const;
            int getSecond(void) const;

            const CCalendar & getRelCalendar(void) const;

            /// Mutateurs ///
            void setYear  (int newyear);
            void setMonth (int newmonth);

            void addMonth (int value);

            /// Autres ///
            StdString toString(void) const;

         public : /* static */

            static CDate FromString(const StdString & str, const CCalendar & calendar);

         private :

            /// Propriétés privées ///
            const CCalendar & relCalendar; // Calendrier lié à la Date.
            int year, month, day, hour, minute, second; // Année, mois, ...


      }; // class CDate;

   } // namespace date
} // namespace xmlioserver

#endif // __XMLIO_CDate__
