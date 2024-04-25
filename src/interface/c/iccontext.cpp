/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */



#include <memory>

#include "xios.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "context.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef enum { D360 = 0 , ALLLEAP, NOLEAP, JULIAN, GREGORIAN } XCalendarType ;

   typedef xios::CContext * XContextPtr;

   // ------------------------ Création des handle -----------------------------

   void cxios_context_handle_create (XContextPtr * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;

      xios_map<StdString, CContext* > def_map =
            xios::CContext::getRoot()->getChildMap();
      
      if (def_map.count(id))
      {
        *_ret = def_map[id];
        CTimer::get("XIOS").suspend() ;
        return;
      }
       CTimer::get("XIOS").suspend() ;
       ERROR("void cxios_context_handle_create (XContextPtr * _ret, const char * _id, int _id_len)",
             << "Context "<<id<<"  unknown");
      // Lever une exeception ici
   }
   CATCH_DUMP_STACK

   // ------------------------ Changements de contextes ------------------------

   void cxios_context_get_current(XContextPtr* context)
   TRY
   {
      CTimer::get("XIOS").resume();
      *context = CContext::getCurrent();
      CTimer::get("XIOS").suspend();
   }
   CATCH_DUMP_STACK

   void cxios_context_get_id(XContextPtr context, char * _id, int _id_len)
   TRY
   {
      string_copy(context->getId(),_id,_id_len);
   }
   CATCH_DUMP_STACK
   
   void cxios_context_set_current(XContextPtr context, bool withswap)
   TRY
   {
      CTimer::get("XIOS").resume() ;
      CContext::setCurrent(context->getId());
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK

   // -------------------- Vérification des identifiants -----------------------

   void cxios_context_valid_id (bool * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume();

      xios_map<StdString, CContext* > def_map =
            xios::CContext::getRoot()->getChildMap();

      *_ret = false;
      if (def_map.count(id))
      {
        *_ret = true;
        CTimer::get("XIOS").suspend();
        return;
      }
      CTimer::get("XIOS").suspend();
   }
   CATCH_DUMP_STACK
} // extern "C"
