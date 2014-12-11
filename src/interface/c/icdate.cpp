/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "context.hpp"
#include "context_client.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   void cxios_update_calendar(int step)
   {
      CTimer::get("XIOS").resume() ;
      xios::CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;
      context->updateCalendar(step) ;
      context->sendUpdateCalendar(step) ;
      CTimer::get("XIOS").suspend() ;
   }

} // extern "C"
