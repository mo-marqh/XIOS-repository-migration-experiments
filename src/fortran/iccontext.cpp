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

   // ----------------------- Redéfinition de types ----------------------------

   typedef enum { D360 = 0 , ALLLEAP, NOLEAP, JULIAN, GREGORIAN } XCalendarType ;

   typedef xmlioserver::tree::CContext * XContextPtr;

   // ------------------------- Attributs des contextes ------------------------
   
   
   void cxios_set_context_calendar_type(XContextPtr context_hdl, const char * calendar_type, int calendar_type_size)
   {
      std::string calendar_type_str; 
      if (!cstr2string(calendar_type, calendar_type_size, calendar_type_str)) return;
      context_hdl->calendar_type.setValue(calendar_type_str);
      context_hdl->sendAttributToServer(context_hdl->calendar_type) ;
   }
   
   void cxios_set_context_start_date(XContextPtr context_hdl, const char * start_date, int start_date_size)
   {
      std::string start_date_str; 
      if (!cstr2string(start_date, start_date_size, start_date_str)) return;

      context_hdl->start_date.setValue(start_date_str);
      context_hdl->sendAttributToServer(context_hdl->start_date) ; 
   }
   
   void cxios_set_context_output_dir(XContextPtr context_hdl, const char * output_dir, int output_dir_size)
   {
      std::string output_dir_str; 
      if (!cstr2string(output_dir, output_dir_size, output_dir_str)) return;

      context_hdl->output_dir.setValue(output_dir_str);
      context_hdl->sendAttributToServer(context_hdl->output_dir) ; 
   }
   
   // ------------------------ Création des handle -----------------------------
   
   void cxios_context_handle_create (XContextPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      std::vector<boost::shared_ptr<xmlioserver::tree::CContext> > def_vector =
            xmlioserver::tree::CContext::GetContextGroup()->getChildList();

      for (std::size_t i = 0; i < def_vector.size(); i++)
	   {
          if (def_vector[i]->getId().compare(id) == 0)
          *_ret = def_vector[i].get();
          return;
      }
      // Lever une execption ici
   }
   
   // ------------------------ Changements de contextes ------------------------
   
   void cxios_context_set_current(XContextPtr context, bool withswap)
   {
      CTreeManager::SetCurrentContextId(context->getId());
   }
   
 
   // -------------------- Vérification des identifiants -----------------------

   void cxios_context_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      std::vector<boost::shared_ptr<xmlioserver::tree::CContext> > def_vector =
            xmlioserver::tree::CContext::GetContextGroup()->getChildList();

      for (std::size_t i = 0; i < def_vector.size(); i++)
	   {
          if (def_vector[i]->getId().compare(id) == 0)
          *_ret = true;
      }
     *_ret = false;
   }
} // extern "C"
