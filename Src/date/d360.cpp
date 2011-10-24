#include "d360.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CD360Calendar::CD360Calendar(const StdString & dateStr)
         : CCalendar("D360", dateStr)
      { /* Ne rien faire de plus */ }

      CD360Calendar::CD360Calendar(int yr, int mth, int d,
                                   int hr, int min, int sec)
         : CCalendar("D360", yr, mth, d, hr, min, sec)
      { /* Ne rien faire de plus */ }

      CD360Calendar::~CD360Calendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      int CD360Calendar::getYearTotalLength(const CDate & date) const
      { return (360 * 86400); }

      int CD360Calendar::getMonthLength(const CDate & date) const
      { return (30); }

      StdString CD360Calendar::getType(void) const
      { return (StdString("d360")); }

      ///--------------------------------------------------------------
   } // namespace date
} // namespace xmlioserver

