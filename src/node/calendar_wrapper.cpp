#include "calendar_wrapper.hpp"
#include "type.hpp"
#include "calendar_type.hpp"
#include "duration.hpp"
#include "context.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CCalendarWrapper::CCalendarWrapper(void)
    : CObjectTemplate<CCalendarWrapper>(), CCalendarWrapperAttributes()
  { /* Ne rien faire de plus */ }

  CCalendarWrapper::CCalendarWrapper(const StdString & id)
    : CObjectTemplate<CCalendarWrapper>(id), CCalendarWrapperAttributes()
  { /* Ne rien faire de plus */ }

  CCalendarWrapper::~CCalendarWrapper(void)
  {}

  //----------------------------------------------------------------

  StdString CCalendarWrapper::GetName(void)    { return StdString("calendar_wrapper"); }
  StdString CCalendarWrapper::GetDefName(void) { return StdString("calendar"); }
  ENodeType CCalendarWrapper::GetType(void)    { return eCalendarWrapper; }

  //----------------------------------------------------------------

  /*!
  \brief Extract the calendar from its wrapper
  \return the calendar
  */
  boost::shared_ptr<CCalendar> CCalendarWrapper::getCalendar(bool checkValid /*= false*/) const
  {
    if (checkValid && !this->calendar)
      ERROR("CCalendarWrapper::getCalendar(bool checkValid = true)", << "The calendar was accessed before being created!");
    return this->calendar;
  }

  const CDate& CCalendarWrapper::getInitDate() const
  {
    return getCalendar(true)->getInitDate();
  }

  const CDate& CCalendarWrapper::getTimeOrigin() const
  {
    return getCalendar(true)->getTimeOrigin();
  }

  //----------------------------------------------------------------

  void CCalendarWrapper::setInitDate(const CDate& initDate)
  {
    getCalendar(true)->setInitDate(initDate);
    start_date = initDate.toString();
  }

  void CCalendarWrapper::setTimeOrigin(const CDate& timeOrigin)
  {
    getCalendar(true)->setTimeOrigin(timeOrigin);
    time_origin = timeOrigin.toString();
  }

  //----------------------------------------------------------------

  /*!
  \brief Parse the calendar node
  \param [in] node xmld node corresponding to the calendar definition
  */
  void CCalendarWrapper::parse(xml::CXMLNode& node)
  {
    SuperClass::parse(node);

    // Try to create the calendar
    createCalendar();
  }

  /*!
  \brief Try to create the calendar from the parsed attributes
  */
  void CCalendarWrapper::createCalendar(void)
  {
    // Create the calendar if possible
    if (calendar)
    {
      ERROR("CCalendarWrapper::createCalendar(void)",
            << "Error: the calendar can only be defined once!");
    }
    else if (!type.isEmpty())
    {
#define DECLARE_CALENDAR(MType, eType)                                     \
      if (type.getValue() == type_attr::eType)                             \
        calendar = boost::shared_ptr<CCalendar>(new C##MType##Calendar());
#include "calendar_type.conf"
#undef DECLARE_CALENDAR

      if (!calendar)
        ERROR("CCalendarWrapper::parse(xml::CXMLNode& node)",
              << "[ type = " << type.getStringValue() << " ] "
              << "The calendar is not properly handled!");

      // Set the timestep is available
      if (!timestep.isEmpty())
        calendar->setTimeStep(timestep.getValue());

      // Parse and set the start date if available
      if (!start_date.isEmpty())
        calendar->setInitDate(CDate::FromString(start_date.getValue(), *calendar));

      // Parse and set the time origin if available
      if (!time_origin.isEmpty())
        calendar->setTimeOrigin(CDate::FromString(time_origin.getValue(), *calendar));

      // Notify the context about the calendar
      CContext* context = CContext::getCurrent();
      if (!context)
        ERROR("CCalendarWrapper::createCalendar(void)", << "Impossible to set the calendar: no current context available.");
      context->setCalendar(calendar);
    }
    else if (!start_date.isEmpty() || !time_origin.isEmpty())
    {
      ERROR("CCalendarWrapper::parse(xml::CXMLNode& node)",
            << "The calendar type must be set before defining the start date or the time origin!");
    }
  }

  /*!
  \brief Try to update the timestep of the calendar with the corresponding attribute
  */
  void CCalendarWrapper::updateTimestep(void)
  {
    if (timestep.isEmpty())
    {
      ERROR("CCalendarWrapper::updateTimestep(void)",
            << "Error: the timestep needs to be defined!");
    }
    else if (calendar)
      calendar->setTimeStep(timestep.getValue());
  }
}