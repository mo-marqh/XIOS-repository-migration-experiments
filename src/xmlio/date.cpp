#include "date.hpp"
#include "calendar.hpp"

namespace xmlioserver
{
   namespace date
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CDate::CDate(const CCalendar& calendar,
                   int yr, int mth, int d,
                   int hr, int min, int sec)
         : relCalendar(calendar)
         , year(yr), month(mth) , day(d)
         , hour(hr), minute(min), second(sec)
      {
         if(!this->checkDate())
         {
            DEBUG(<< "La date initialisée a été modifiée "
                  << "car elle était incorrecte par rapport au calendrier souhaité.");
         }
      }

      CDate::CDate(const CDate & date)
            : relCalendar(date.getRelCalendar()),
              year(date.year), month(date.month)  , day(date.day),
              hour(date.hour), minute(date.minute), second(date.second)
      {
         if(!this->checkDate())
         {
            DEBUG(<< "La date initialisée a été modifiée "
                  << "car elle était incorrecte par rapport au calendrier souhaité.");
         }
      }

      CDate::~CDate(void)
      { /* Ne rien faire de plus */ }

      ///---------------------------------------------------------------

      CDate & CDate::operator=(const CDate & date)
      {
         // relCalendar = d.getRelCalendar(); << inutile si fonction bien utilisée
         year = date.year; month  = date.month ; day    = date.day;
         hour = date.hour; minute = date.minute; second = date.second;
         return (*this);
      }

      StdOStream & operator<<(StdOStream & out, const CDate & date)
      {
         std::streamsize s = out.width (2);
         char c = out.fill ('0');
         out << date.day << '/';
         s = out.width (2); c = out.fill ('0');
         out << date.month << '/';
         s = out.width (4); c = out.fill ('0');
         out << date.year << '-';
         s = out.width (2); c = out.fill ('0');
         out << date.hour << ':';
         s = out.width (2); c = out.fill ('0');
         out << date.minute << ':';
         s = out.width (2); c = out.fill ('0');
         out << date.second;
         
         return (out);
      }

      StdIStream & operator>>(StdIStream & in, CDate & date) // Non testée.
      {
         char c = '/'; // Le caractère c est utilisé pour "recueillir" les séparateurs "/" et ":".
         in >> date.day  >> c >> date.month  >> c >> date.year   >> c;
         in >> date.hour >> c >> date.minute >> c >> date.second;
         if(!date.checkDate())
         {
            DEBUG("La date initialisée (depuis une chaîne de caractères) "
                  << "a été modifiée car elle était incorrecte "
                  << "par rapport au calendrier souhaité.");
         }
         return (in);
      }

      CDate::operator Time(void) // Non vérifiée, pas optimisée ...
      {
         // Todo : Tester si la date courante est supérieure à la date initiale.
         Time retvalue = - relCalendar.getNbSecond(relCalendar.getInitDate())
                         + relCalendar.getNbSecond(*this);

         if ((relCalendar.getId().compare("D360")    == 0) ||
             (relCalendar.getId().compare("AllLeap") == 0) ||
             (relCalendar.getId().compare("NoLeap")  == 0))
         return (retvalue + (getYear() - relCalendar.getInitDate().getYear())
                                       * relCalendar.getYearTotalLength(*this));

         for(CDate _d(relCalendar.getInitDate());
            _d.getYear() < getYear(); _d.setYear(_d.getYear()+1))
            retvalue += relCalendar.getYearTotalLength(_d);
         return (retvalue);
      }

      //----------------------------------------------------------------

      bool CDate::checkDate(void)
      {
         bool retValue = true;

         // Vérificatio de la valeur du mois.
         if (month  < 1) { retValue = false; month  = 1; }
         if (month  > relCalendar.getYearLength())
         { retValue = false; month = relCalendar.getYearLength(); }

         // Vérification de la valeur du jour.
         if (day    < 1) { retValue = false; month  = 1; }
         if (day    > relCalendar.getMonthLength(*this))
         { retValue = false; day = relCalendar.getMonthLength(*this); }

         // Vérification de la valeur de l'heure.
         if (hour   < 0) { retValue = false; hour  = 0; }
         if (hour   >= relCalendar.getDayLength())
         { retValue = false; hour = relCalendar.getDayLength()-1; }

         // Vérification de la valeur des minutes.
         if (minute < 0) { retValue = false; minute = 0; }
         if (minute >= relCalendar.getHourLength())
         { retValue = false; minute = relCalendar.getHourLength()-1; }

         // Vérification de la valeur des secondes.
         if (second < 0) { retValue = false; month  = 0; }
         if (second >= relCalendar.getMinuteLength())
         { retValue = false; second = relCalendar.getMinuteLength()-1; }

         return retValue;
      }

      //----------------------------------------------------------------

      int CDate::getYear  (void) const { return (this->year  ); }
      int CDate::getMonth (void) const { return (this->month ); }
      int CDate::getDay   (void) const { return (this->day   ); }
      int CDate::getHour  (void) const { return (this->hour  ); }
      int CDate::getMinute(void) const { return (this->minute); }
      int CDate::getSecond(void) const { return (this->second); }

      //----------------------------------------------------------------

      const CCalendar & CDate::getRelCalendar(void) const
      { return (this->relCalendar); }

      //----------------------------------------------------------------

      void CDate::setYear  (int newyear)  { this->year  = newyear; }
      void CDate::setMonth (int newmonth) { this->month = newmonth; }

      //----------------------------------------------------------------

      void CDate::addMonth (int value)
      {// Value doit être égale à 1 ou -1.
         this->month += value;
         if (this->month == 13) { year++; this->month = 1 ; }
         if (this->month == 0 ) { year--; this->month = 12; }
      }

      //----------------------------------------------------------------

      CDate CDate::FromString(const StdString & str, const CCalendar & calendar)
      {
         CDate dt(calendar);
         StdIStringStream iss(str);
         iss >> dt;
         return dt;
      }
      
      //----------------------------------------------------------------
      
       StdString CDate::toString(void) const
      { 
         StdOStringStream oss;
         oss << (*this);
         return (oss.str()); 
      }

      ///---------------------------------------------------------------

   } // namespace date
} // namespace xmlioserver
