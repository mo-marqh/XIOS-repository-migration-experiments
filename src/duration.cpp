#include "duration.hpp"
#include "date.hpp"
#include "calendar.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      const CDuration Year   = {1.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                      Month  = {0.0, 1.0, 0.0, 0.0, 0.0, 0.0},
                      Week   = {0.0, 0.0, 7.0, 0.0, 0.0, 0.0},
                      Day    = {0.0, 0.0, 1.0, 0.0, 0.0, 0.0},
                      Hour   = {0.0, 0.0, 0.0, 1.0, 0.0, 0.0},
                      Minute = {0.0, 0.0, 0.0, 0.0, 1.0, 0.0},
                      Second = {0.0, 0.0, 0.0, 0.0, 0.0, 1.0},
                      NoneDu = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

      ///---------------------------------------------------------------

      CDuration & CDuration::operator=(const CDuration & duration)
      {
         year = duration.year;  month  = duration.month ; day    = duration.day;
         hour = duration.hour;  minute = duration.minute; second = duration.second;
         return (*this);
      }

      StdOStream & operator<<(StdOStream & out, const CDuration & duration)
      {
         StdOStringStream sout;
         bool testValue = true;
         if(duration.year   != 0.0) { testValue = false; sout << duration.year   << "y " ; }
         if(duration.month  != 0.0) { testValue = false; sout << duration.month  << "mo "; }
         if(duration.day    != 0.0) { testValue = false; sout << duration.day    << "d " ; }
         if(duration.hour   != 0.0) { testValue = false; sout << duration.hour   << "h " ; }
         if(duration.minute != 0.0) { testValue = false; sout << duration.minute << "mi "; }
         if(duration.second != 0.0 || testValue)       { sout << duration.second << "s " ; }

         // << suppression de l'espace en fin de chaîne.
         out << (sout.str().substr(0, sout.str().size()-1));
         return (out);
      }

      StdIStream & operator>>(StdIStream & in , CDuration & duration)
      {
         duration.year = duration.month  = duration.day    =
         duration.hour = duration.minute = duration.second = 0.0;
         double v = 1.0;
         char   c = '/';
         while (!in.eof())
         {
               if (!(in >> v >> c)) 
               {
                 //DEBUG("----> Pb StdIStream & operator>>(StdIStream & in , CDuration & duration)") ;
                 //if (in.eof())  DEBUG("----> Fin de fichier StdIStream & operator>>(StdIStream & in , CDuration & duration)") ;
               }
               if (in.eof())  
               {
                 //DEBUG("----> Fin de fichier StdIStream & operator>>(StdIStream & in , CDuration & duration)") ;
                 break ;
               }
               switch (c)
               {
                  case 'y': duration.year   = v; break;
                  case 'd': duration.day    = v; break;
                  case 'h': duration.hour   = v; break;
                  case 's': duration.second = v; break;
                  case 'm':
                  {
                     in >> c;
                     if     (c == 'i') duration.minute = v;
                     else if(c == 'o') duration.month  = v;
                     else
                     {
                        StdString valc("m"); valc.append(1, c);
                        //DEBUG("La chaine \"" << valc << "\" ne permet pas de définir une unité de durée.");
                        break;
                     }
                     break;
                  }
                  default:
                     StdString valc; valc.append(1, c);
                     //DEBUG("La chaine \"" << valc << "\" ne permet pas de définir une unité de durée.");
                     break;
               }
            }
            return (in);
      }

      //-----------------------------------------------------------------

      bool CDuration::isNone(void) const
      {
         if ((year == 0) && (month  == 0) && (day    == 0) &&
             (hour == 0) && (minute == 0) && (second == 0))
            return (true);
         return (false);
      }

      //-----------------------------------------------------------------

      CDuration & CDuration::resolve(const CCalendar & c)
      {
         // Simplification de l'écriture des minutes.
         second += modf(minute, &minute) * (float)c.getMinuteLength();
         minute += int(second)/c.getMinuteLength(); second = int(second)%c.getMinuteLength();

         // Simplification de l'écriture des heures.
         minute += modf(hour , &hour) * (float)c.getHourLength();
         hour   += int(minute)/c.getHourLength(); minute = int(minute)%c.getHourLength();

         // Simplification de l'écriture des jours.
         hour   += modf(day, &day) * (float)c.getDayLength();
         day    += int(hour)  /c.getDayLength(); hour   = int(hour)%c.getDayLength();

         // > Aucune équivalence jour - mois fixée par avance. //

         // Simplification de l'écriture des années.
         month  += modf(year, &year) * (float)c.getYearLength();
         year   += int(month) /c.getYearLength(); month  = int(month)%c.getYearLength();
         return (*this);
      }

      //-----------------------------------------------------------------

      StdString CDuration::toString(void) const
      {
         const  CDuration & own = *this;
         StdOStringStream oss; oss << own;
         return (oss.str());
      }

      CDuration CDuration::FromString(const StdString & str)
      {
         CDuration dr = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
         StdIStringStream iss(str); iss >> dr;
         return (dr);
      }

      ///---------------------------------------------------------------


} // namespace xios

