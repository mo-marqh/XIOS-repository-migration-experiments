#include "calendar_util.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CDuration operator*(const double & scal, const CDuration & ddr)
      { return (ddr * scal); }

      CDuration operator-(const CDuration & ddr , const CDuration & dr)
      {
         CDuration dur(ddr);
         dur.year -= dr.year;  dur.month  -= dr.month ; dur.day    -= dr.day;
         dur.hour -= dr.hour;  dur.minute -= dr.minute; dur.second -= dr.second;
         return (dur);
      }

      CDuration operator+(const CDuration & ddr , const CDuration & dr)
      {
         CDuration dur(ddr);
         dur.year += dr.year;  dur.month  += dr.month ; dur.day    += dr.day;
         dur.hour += dr.hour;  dur.minute += dr.minute; dur.second += dr.second;
         return (dur);
      }

      CDuration operator*(const CDuration & ddr , const double & scal)
      {
         CDuration dur(ddr);
         dur.year *= scal;  dur.month  *= scal; dur.day    *= scal;
         dur.hour *= scal;  dur.minute *= scal; dur.second *= scal;
         return (dur);
      }

      CDuration operator-(const CDuration & ddr)
      {
         CDuration dur(ddr);
         dur.year = -dur.year;  dur.month  = -dur.month ; dur.day    = -dur.day;
         dur.hour = -dur.hour;  dur.minute = -dur.minute; dur.second = -dur.second;
         return (dur);
      }

      //-----------------------------------------------------------------

      CDate operator+(const CDate & dt, const CDuration & dr)
      {
         CDuration drr (dr);
         int year = 0, month = 0, day = 0, hour = 0, minute = 0, second = 0;
         const CCalendar & c = dt.getRelCalendar();

         drr.resolve(dt.getRelCalendar());

         // Ajustement des minutes par rapport aux secondes.
         second += dt.getSecond() + drr.second;
         if (second <  0) { minute --; second += c.getMinuteLength(); }
         if (second >= c.getMinuteLength()) { minute ++; second -= c.getMinuteLength(); }

         // Ajustement des heures en fonction des minutes.
         minute += dt.getMinute() + drr.minute;
         if (minute < 0) { hour --; minute += c.getHourLength(); }
         if (minute >= c.getHourLength()) { hour ++; minute -= c.getHourLength(); }

         // Ajustement des jours en fonction des heures.
         hour += dt.getHour() + drr.hour;
         if (hour <  0) { drr.day --; hour += c.getDayLength(); }
         if (hour >= c.getDayLength()) { drr.day ++; hour -= c.getDayLength(); }

         // Ajustement des mois en fonction des jours.
         int signVal = drr.day/fabs(drr.day);
         CDate dtt(dt); dtt.addMonth (signVal);

         for(; c.getMonthLength(dtt) < fabs(drr.day); dtt.addMonth (signVal))
         { drr.day -= signVal * c.getMonthLength(dtt); drr.month += signVal; }

         day += dt.getDay() + drr.day;
         if (day <  0) { drr.month --; day += c.getMonthLength(dtt); }
         if (day >= c.getMonthLength(dtt)) { drr.month ++; day -= c.getMonthLength(dtt); } // << Problème ici
         if (day == 0) day = c.getMonthLength(dtt);

         drr.resolve(dt.getRelCalendar());

         // Ajustement des années en fonction des mois.
         month += dt.getMonth() + drr.month;
         if (month <  0) { year --; month += c.getYearLength(); }
         if (month >= c.getYearLength()) { year ++; month -= c.getYearLength(); }
         if (month == 0) month = c.getYearLength();

         year += dt.getYear() + drr.year;

         return (CDate(dt.getRelCalendar(), year, month, day, hour, minute, second));
      }

      CDate operator-(const CDate & dt, const CDuration & dr) { return (dt + (-dr)); }

      //-----------------------------------------------------------------

      CDuration operator-(const CDate & dt0, const CDate & dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         CDuration dur =
         { dt0.getYear() - dt1.getYear(), dt0.getMonth()  - dt1.getMonth() , dt0.getDay()    - dt1.getDay(),
           dt0.getHour() - dt1.getHour(), dt0.getMinute() - dt1.getMinute(), dt0.getSecond() - dt1.getSecond() };
         return (dur.resolve(dt0.getRelCalendar()));
      }

      //-----------------------------------------------------------------

      /// Les opérateurs de comparaison. (Non testés pour le moment)
      bool operator==(const CDate& dt0, const CDate& dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         return ((dt0.getYear() == dt1.getYear()) && (dt0.getMonth()  == dt1.getMonth())  && (dt1.getDay()    == dt0.getDay()) &&
                 (dt0.getHour() == dt1.getHour()) && (dt0.getMinute() == dt1.getMinute()) && (dt1.getSecond() == dt0.getSecond()));
      }

      bool operator< (const CDate& dt0, const CDate& dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         if (dt0.getYear()   < dt1.getYear())   return true; if (dt0.getMonth()  < dt1.getMonth())  return true;
         if (dt0.getDay()    < dt1.getDay())    return true; if (dt0.getHour()   < dt1.getHour())   return true;
         if (dt0.getMinute() < dt1.getMinute()) return true; if (dt0.getSecond() < dt1.getSecond()) return true;
         return false;
      }

      //-----------------------------------------------------------------

      bool operator!=(const CDate & dt0, const CDate & dt1){ return !(dt1 == dt0); }
      bool operator> (const CDate & dt0, const CDate & dt1){ return (dt1 < dt0); }
      bool operator>=(const CDate & dt0, const CDate & dt1){ return ((dt0 > dt1) || (dt1 == dt0)); }
      bool operator<=(const CDate & dt0, const CDate & dt1){ return ((dt0 < dt1) || (dt1 == dt0)); }

      ///----------------------------------------------------------------

   } // namespace date
} // namespace xmlioserver






