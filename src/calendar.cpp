#include "calendar.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "calendar_util.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CCalendar::CCalendar(void)
         : CObject()
         , initDate(*this)
         , timeOrigin(*this)
         , currentDate(*this)
      {}

      CCalendar::CCalendar(const StdString& id)
               : CObject(id)
               , initDate(*this)
               , timeOrigin(*this)
               , currentDate(*this)
      {}

      CCalendar::CCalendar(const StdString& id,
                           int yr, int mth, int d,
                           int hr /*= 0*/, int min /*= 0*/, int sec /*= 0*/)
               : CObject(id)
               , initDate(*this)
               , timeOrigin(*this)
               , currentDate(*this)
      {
        initializeDate(yr, mth, d, hr, min, sec);
      }

      CCalendar::CCalendar(const StdString& id, const CDate& startDate)
               : CObject(id)
               , initDate(startDate)
               , timeOrigin(startDate)
               , currentDate(startDate)
      {
        // Initialize the dates only in the derivated classes
        // since we want to use the overloaded virtual functions
      }

      CCalendar::CCalendar(const StdString& id, const CDate& startDate, const CDate& timeOrigin)
               : CObject(id)
               , initDate(startDate)
               , timeOrigin(timeOrigin)
               , currentDate(startDate)
      {
        // Initialize the dates only in the derivated classes
        // since we want to use the overloaded virtual functions
      }

      void CCalendar::initializeDate()
      {
        if (!initDate.setRelCalendar(*this))
          ERROR("CCalendar::initializeDate()",
                "initDate: Bad format or date not conform to the calendar");
        if (!timeOrigin.setRelCalendar(*this))
          ERROR("CCalendar::initializeDate()",
                "timeOrigin: Bad format or date not conform to the calendar");
        if (!currentDate.setRelCalendar(*this))
          ERROR("CCalendar::initializeDate()",
                "currentDate: Bad format or date not conform to the calendar");
      }

      void CCalendar::initializeDate(int yr, int mth, int d,
                                     int hr /*= 0*/, int min /*= 0*/, int sec /*= 0*/)
      {
        initDate = CDate(*this, yr, mth, d, hr, min, sec);
        timeOrigin = initDate;
        currentDate = initDate;
      }

      void CCalendar::initializeDate(const StdString& dateStr)
      {
        initDate = CDate::FromString(dateStr, *this);
        timeOrigin = initDate;
        currentDate = initDate;
      }

      void CCalendar::initializeDate(const StdString& dateStr, const StdString& timeOriginStr)
      {
        initDate = CDate::FromString(dateStr, *this);
        timeOrigin = CDate::FromString(timeOriginStr, *this);
        currentDate = initDate;
      }

      CCalendar::~CCalendar(void)
      { /* Ne rien faire de plus */ }

      ///---------------------------------------------------------------

      StdString CCalendar::toString(void) const
      {
         StdOStringStream oss;
         oss <<   "[type: "   << this->getId()
             << ", start: "   << this->initDate
             << ", current: " << this->currentDate << "]";
         return (oss.str());
      }

      void CCalendar::fromString(const StdString& str)
      { ERROR("CCalendar::fromString(str)",
               << "[ str = " << str << "] Not implemented yet !"); }

      //-----------------------------------------------------------------

      void CCalendar::setTimeStep(const CDuration& timestep)
      {
        if (timestep.timestep)
          ERROR("CCalendar::setTimeStep(const CDuration& timestep)",
                << "Circular definition of the timestep: the timestep cannot refer to itself.");
        this->timestep = timestep;
      }

      CDate& CCalendar::update(int step)
      {
        info(20) << "update step : " << step << " timestep " << this->timestep << std::endl;
        return (this->getCurrentDate() = this->getInitDate() + step * this->timestep);
      }

      //-----------------------------------------------------------------

      void CCalendar::setInitDate(const CDate& initDate)
      {
        if (&initDate.getRelCalendar() != this)
          ERROR("CCalendar::setInitDate(const CDate& initDate)",
                << "The init date cannot be attached to another calendar.");
        this->initDate = initDate;
      }

      void CCalendar::setTimeOrigin(const CDate& timeOrigin)
      {
        if (&timeOrigin.getRelCalendar() != this)
          ERROR("CCalendar::setInitDate(const CDate& timeOrigin)",
                << "The time origin cannot be attached to another calendar.");
        this->timeOrigin = timeOrigin;
      }

      //-----------------------------------------------------------------

      const CDuration& CCalendar::getTimeStep(void) const { return this->timestep; }
      const CDate& CCalendar::getInitDate(void) const     { return this->initDate; }
      const CDate& CCalendar::getTimeOrigin(void) const   { return this->timeOrigin; }
      CDate& CCalendar::getCurrentDate(void)              { return this->currentDate; }

      //-----------------------------------------------------------------

      int CCalendar::getMonthLength(const CDate& date) const
      { // Retourne la durée du mois en jour.
        static const int NoLeapMonthLength[] =
          { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
        return NoLeapMonthLength[date.getMonth() - 1];
      }

      StdString CCalendar::getType(void) const { return StdString(this->getId()); }

      int CCalendar::getYearTotalLength(const CDate& date) const { return (365 * 86400); }

      int CCalendar::getYearLength  (void) const { return 12; }
      int CCalendar::getDayLength   (void) const { return 24; }
      int CCalendar::getHourLength  (void) const { return 60; }
      int CCalendar::getMinuteLength(void) const { return 60; }
      int CCalendar::getDayLengthInSeconds(void) const { return getDayLength() * getHourLength() * getMinuteLength(); }

      StdString CCalendar::getMonthName(int monthId) const
      {
        static const StdString MonthNames[] =
          { "january", "february", "march",     "april" ,  "may",      "june",
            "july",    "august",   "september", "october", "november", "december" };
        return MonthNames[monthId - 1];
      }

      const StdString CCalendar::getMonthShortName(int monthId) const
      { StdString value = this->getMonthName(monthId); value.resize(3); return value; }

      ///----------------------------------------------------------------

} // namespace xios
