/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "icutil.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xios::tree::CDomain      * XDomainPtr;
   typedef xios::tree::CDomainGroup * XDomainGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_domain_handle_create (XDomainPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CDomain>(id).get();
   }
   
   void cxios_domaingroup_handle_create (XDomainGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::GetObject<xios::tree::CDomainGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_domain_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CDomain>(id);
   }

   void cxios_domaingroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xios::CObjectFactory::HasObject<xios::tree::CDomainGroup>(id);
   }
} // extern "C"
