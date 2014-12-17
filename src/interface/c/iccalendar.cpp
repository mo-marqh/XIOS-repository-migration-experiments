#include "xmlioserver.hpp"

#include "exception.hpp"
#include "timer.hpp"
#include "context.hpp"
#include "context_client.hpp"

extern "C"
{
  void cxios_create_calendar()
  {
    CTimer::get("XIOS").resume();
    xios::CContext* context = CContext::getCurrent();
    if (context->hasClient)
      context->solveCalendar();
    CTimer::get("XIOS").suspend();
  }

  void cxios_update_calendar(int step)
  {
    CTimer::get("XIOS").resume();
    xios::CContext* context = CContext::getCurrent();
    if (!context->hasServer) context->client->checkBuffers();
    context->updateCalendar(step);
    context->sendUpdateCalendar(step);
    CTimer::get("XIOS").suspend();
  }
}