/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "calendar_type.hpp"

#include "icutil.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   void cxios_set_timestep(double ts_year, double ts_month,  double ts_day,
                          double ts_hour, double ts_minute, double ts_second)
   {
      try
      {
         date::CDuration dur = {ts_year, ts_month, ts_day, ts_hour, ts_minute, ts_second};
         boost::shared_ptr<xmlioserver::tree::CContext> context =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CContext>
            (CObjectFactory::GetCurrentContextId());
         boost::shared_ptr<xmlioserver::data::CDataTreatment> dtreat = context->getDataTreatment();
         if (dtreat.get() != 0)
         {
            dtreat->set_timestep(dur);     
         }
         else
         {
            context->solveCalendar();
            context->timestep.setValue(dur.toString());
            context->getCalendar()->setTimeStep(dur);
         }       
      }
      catch (xmlioserver::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }
   
   void cxios_update_calendar(int step)
   {
      try
      {
         boost::shared_ptr<xmlioserver::tree::CContext> context =
         xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CContext>
            (CObjectFactory::GetCurrentContextId());
         boost::shared_ptr<xmlioserver::data::CDataTreatment> dtreat = context->getDataTreatment();
         dtreat->update_calendar(step);
      }
      catch (xmlioserver::CException & exc)
      {
         std::cerr << exc.getMessage() << std::endl;
         exit (EXIT_FAILURE);
      }
   }

} // extern "C"
