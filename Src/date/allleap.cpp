#include "allleap.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CAllLeapCalendar::CAllLeapCalendar(const StdString & dateStr)
         : CCalendar("AllLeap", dateStr)
      { /* Ne rien faire de plus */ }

      CAllLeapCalendar::CAllLeapCalendar(int yr, int mth, int d,
                                         int hr, int min, int sec)
         : CCalendar("AllLeap", yr, mth, d, hr, min, sec)
      { /* Ne rien faire de plus */ }

      CAllLeapCalendar::~CAllLeapCalendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      int CAllLeapCalendar::getYearTotalLength(const CDate & date) const
      { return (366 * 86400); }

      int CAllLeapCalendar::getMonthLength(const CDate & date) const
      {
         if (date.getMonth() == 2) return (29);
         return (CCalendar::getMonthLength(date));
      }

      StdString CAllLeapCalendar::getType(void) const
      { return (StdString("allleap")); }

      ///--------------------------------------------------------------
   } // namespace date
} // namespace xmlioserver

