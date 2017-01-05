#include "duration.hpp"
#include "date.hpp"
#include "calendar.hpp"
#include "calendar_util.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      const CDuration Year     = { 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
                      Month    = { 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
                      Week     = { 0.0, 0.0, 7.0, 0.0, 0.0, 0.0, 0.0 },
                      Day      = { 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 },
                      Hour     = { 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 },
                      Minute   = { 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0 },
                      Second   = { 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0 },
                      TimeStep = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0 },
                      NoneDu   = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };

      ///---------------------------------------------------------------

      CDuration& CDuration::operator=(const CDuration& duration)
      {
        year = duration.year; month  = duration.month;  day    = duration.day;
        hour = duration.hour; minute = duration.minute; second = duration.second; timestep = duration.timestep;
        return *this;
      }

      bool operator<(CDuration a, CDuration b)
      {
        double aSeconds = a.second + 60.*(a.minute + 60.*(a.hour + 24.*(a.day + a.year*365.25) ) );
        double bSeconds = b.second + 60.*(b.minute + 60.*(b.hour + 24.*(b.day + b.year*365.25) ) );
        return (aSeconds < bSeconds);
      }

      StdOStream& operator<<(StdOStream& out, const CDuration& duration)
      {
         out << duration.toString();
         return out;
      }

      StdIStream& operator>>(StdIStream& in , CDuration& duration)
      {
        duration = NoneDu;
        double v = 1.0;
        char   c = '/';
        bool   invalidUnit = false;

        do
        {
          in >> v >> c;
          if (in.fail())
            ERROR("StdIStream& operator>>(StdIStream& in , CDuration& duration)",
                  << "Bad duration format: impossible to read a pair (value, unit).");

          switch (c)
          {
            case 'y': duration.year   = v; break;
            case 'd': duration.day    = v; break;
            case 'h': duration.hour   = v; break;
            case 's': duration.second = v; break;
            case 'm':
            {
              in >> c;
              if      (c == 'i') duration.minute = v;
              else if (c == 'o') duration.month  = v;
              else invalidUnit = true;
              break;
            }
            case 't':
            {
              in >> c;
              if (c == 's') duration.timestep = v;
              else invalidUnit = true;
              break;
            }
            default:
              invalidUnit = true;
              break;
          }

          if (invalidUnit)
            ERROR("StdIStream& operator>>(StdIStream& in , CDuration& duration)",
                  << "Bad duration format: invalid unit, unexpected '" << c << "' character.");
        } while (in.peek() != EOF); // check whether there is a next character to read

        return in;
      }

      //-----------------------------------------------------------------

      bool CDuration::isNone(void) const
      {
        return (*this == NoneDu);
      }

      //-----------------------------------------------------------------

      CDuration& CDuration::solveTimeStep(const CCalendar& c)
      {
        CDuration timeStep = c.getTimeStep();
        second += timestep * timeStep.second;
        minute += timestep * timeStep.minute;
        hour   += timestep * timeStep.hour;
        day    += timestep * timeStep.day;
        month  += timestep * timeStep.month;
        year   += timestep * timeStep.year;
        timestep = 0.0;
        return *this;
      }

      CDuration& CDuration::resolve(const CCalendar& c, bool noNegativeTime /*= false*/)
      {
        return c.resolve(*this, noNegativeTime);
      }

      //-----------------------------------------------------------------

      StdString CDuration::toString(void) const
      {
        StdOStringStream sout;
        bool forceOutput = true;

        if (year   != 0.0) { forceOutput = false; sout << year   << "y "; }
        if (month  != 0.0) { forceOutput = false; sout << month  << "mo "; }
        if (day    != 0.0) { forceOutput = false; sout << day    << "d "; }
        if (hour   != 0.0) { forceOutput = false; sout << hour   << "h "; }
        if (minute != 0.0) { forceOutput = false; sout << minute << "mi "; }
        if (second != 0.0) { forceOutput = false; sout << second << "s "; }
        if (timestep != 0.0 || forceOutput)     { sout << timestep << "ts "; }

         // Remove the trailing space
        StdString strOut = sout.str();
        return strOut.erase(strOut.size() - 1);
      }

      StdString CDuration::toStringUDUnits(void) const
      {
         if (timestep != 0.0)
           ERROR("StdString CDuration::toStringUDUnits(void) const",
                 "Impossible to convert a duration to string using UDUnits when a timestep is set.");

         StdOStringStream sout;
         bool forceOutput = true;

         if (year   != 0.0) { forceOutput = false; sout << year   << " yr "; }
         if (month  != 0.0) { forceOutput = false; sout << month  << " month "; }
         if (day    != 0.0) { forceOutput = false; sout << day    << " d "; }
         if (hour   != 0.0) { forceOutput = false; sout << hour   << " h "; }
         if (minute != 0.0) { forceOutput = false; sout << minute << " min "; }
         if (second != 0.0 || forceOutput)       { sout << second << " s "; }

         // Remove the trailing space
         StdString strOut = sout.str();
         return strOut.erase(strOut.size() - 1);
      }

      CDuration CDuration::FromString(const StdString& str)
      {
        CDuration dr = NoneDu;
        StdIStringStream iss(str); iss >> dr;
        return dr;
      }
} // namespace xios

