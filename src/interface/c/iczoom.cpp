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
#include "extract_axis.hpp"
#include "extract_domain.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xios::CExtractAxis   * XExtractAxisPtr;
   typedef xios::CExtractDomain * XExtractDomainPtr;
   // ------------------------ Création des handle -----------------------------
   void cxios_zoom_axis_handle_create (XExtractAxisPtr * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      cout << "WARNING : you are using the zoom_axis interface. zoom_domain is deprecated, replace it with extract_axis." << endl;
      cout << "\t Calls to xios_set/get_zoom_axis_attr must be replaced with xios_set/get_extract_axis_attr in your model." << endl;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractAxis::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK

   // -------------------- Vérification des identifiants -----------------------
   void cxios_zoom_axis_valid_id (bool * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      cout << "WARNING : you are using the zoom_axis interface. zoom_domain is deprecated, replace it with extract_axis." << endl;
      cout << "\t Calls to xios_set/get_zoom_axis_attr must be replaced with xios_set/get_extract_axis_attr in your model." << endl;

      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractAxis::has(id);
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK

   // ------------------------ Création des handle -----------------------------
   void cxios_zoom_domain_handle_create(XExtractDomainPtr * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      cout << "WARNING : you are using the zoom_domain interface. zoom_domain is deprecated, replace it with extract_domain." << endl;
      cout << "\t Calls to xios_set/get_zoom_domain_attr must be replaced with xios_set/get_extract_domain_attr in your model." << endl;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractDomain::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK

   // -------------------- Vérification des identifiants -----------------------
   void cxios_zoom_domain_valid_id(bool * _ret, const char * _id, int _id_len)
   TRY
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      cout << "WARNING : you are using the zoom_domain interface. zoom_domain is deprecated, replace it with extract_domain." << endl;
      cout << "\t Calls to xios_set/get_zoom_domain_attr must be replaced with xios_set/get_extract_domain_attr in your model." << endl;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CExtractDomain::has(id);
      CTimer::get("XIOS").suspend() ;
   }
   CATCH_DUMP_STACK
} // extern "C"
