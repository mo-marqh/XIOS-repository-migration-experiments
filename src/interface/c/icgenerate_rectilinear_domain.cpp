/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */



#include <memory>

#include "xios.hpp"

#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "generate_rectilinear_domain.hpp"
#include "context.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xios::CGenerateRectilinearDomain   * XGenDomainPtr;

   // ------------------------ Création des handle -----------------------------
   void cxios_generate_rectilinear_domain_handle_create(XGenDomainPtr * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CGenerateRectilinearDomain::get(CContext::getCurrent(), id); 
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK

   // -------------------- Vérification des identifiants -----------------------
   void cxios_generate_rectilinear_domain_valid_id(bool * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = xios::CGenerateRectilinearDomain::has(CContext::getCurrent(), id);
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK
} // extern "C"
