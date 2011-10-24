#include "noleap.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CNoLeapCalendar::CNoLeapCalendar(const StdString & dateStr)
         : CCalendar("NoLeap", dateStr)
      { /* Ne rien faire de plus */ }

      CNoLeapCalendar::CNoLeapCalendar(int yr, int mth, int d,
                                       int hr, int min, int sec)
         : CCalendar("NoLeap", yr, mth, d, hr, min, sec)
      { /* Ne rien faire de plus */ }

      CNoLeapCalendar::~CNoLeapCalendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      StdString CNoLeapCalendar::getType(void) const
      { return (StdString("noleap")); }

      ///--------------------------------------------------------------
   } // namespace date
} // namespace xmlioserver

