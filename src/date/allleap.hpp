#ifndef __XMLIO_CAllLeapCalendar__
#define __XMLIO_CAllLeapCalendar__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "calendar.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAllLeapCalendar : public CCalendar
      {
            /// Typedef ///
            typedef CCalendar SuperClass;

         public :

            /// Constructeur ///
//            CAllLeapCalendar(void);                                   // Not implemented yet.
            CAllLeapCalendar(const StdString & dateStr);
            CAllLeapCalendar(const StdString & dateStr,const StdString & timeOriginStr);
            CAllLeapCalendar(int yr = 0, int mth = 1, int d   = 1,
                            int hr = 0, int min = 0, int sec = 0);
            CAllLeapCalendar(const CAllLeapCalendar & calendar);       // Not implemented yet.
            CAllLeapCalendar(const CAllLeapCalendar * calendar);       // Not implemented yet.

            /// Accesseurs ///
            virtual int getYearTotalLength(const CDate & date) const;
            virtual int getMonthLength(const CDate & date) const;
            virtual StdString getType(void) const;

            /// Destructeur ///
            virtual ~CAllLeapCalendar(void);

      }; // class CAllLeapCalendar

   } // namespace date
} // namespace xmlioserver

#endif // __XMLIO_CAllLeapCalendar__
