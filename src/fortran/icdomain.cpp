/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
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

   typedef xmlioserver::tree::CDomain      * XDomainPtr;
   typedef xmlioserver::tree::CDomainGroup * XDomainGroupPtr;

   // ------------------------- Attributs des axes -----------------------------
   
   void cxios_set_domain_name(XDomainPtr domain_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      domain_hdl->name.setValue(name_str);
      domain_hdl->sendAttributToServer(domain_hdl->name) ; 
   }
   
   void cxios_set_domain_standard_name(XDomainPtr domain_hdl, const char * standard_name, int standard_name_size)
   {
      std::string standard_name_str; 
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      domain_hdl->standard_name.setValue(standard_name_str);
      domain_hdl->sendAttributToServer(domain_hdl->standard_name) ; 
   }
   
   void cxios_set_domain_long_name(XDomainPtr domain_hdl, const char * long_name, int long_name_size)
   {
      std::string long_name_str; 
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      domain_hdl->long_name.setValue(long_name_str);
      domain_hdl->sendAttributToServer(domain_hdl->long_name) ; 
   }
   
   void cxios_set_domain_domain_group_ref(XDomainPtr domain_hdl, const char * domain_group_ref, int domain_group_ref_size)
   {
      std::string domain_group_ref_str; 
      if (!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;

      domain_hdl->domain_group_ref.setValue(domain_group_ref_str);
      domain_hdl->sendAttributToServer(domain_hdl->domain_group_ref) ; 
   }
   
   void cxios_set_domain_ni_glo(XDomainPtr domain_hdl, int ni_glo)
   {
      domain_hdl->ni_glo.setValue(ni_glo);
      domain_hdl->sendAttributToServer(domain_hdl->ni_glo) ; 
   }
   
   void cxios_set_domain_nj_glo(XDomainPtr domain_hdl, int nj_glo)
   {
      domain_hdl->nj_glo.setValue(nj_glo);
      domain_hdl->sendAttributToServer(domain_hdl->nj_glo) ; 
   }
   
   void cxios_set_domain_ibegin(XDomainPtr domain_hdl, int ibegin)
   {
      domain_hdl->ibegin.setValue(ibegin);
      domain_hdl->sendAttributToServer(domain_hdl->ibegin) ; 
   }
   
   void cxios_set_domain_iend(XDomainPtr domain_hdl, int iend)
   {
      domain_hdl->iend.setValue(iend);
      domain_hdl->sendAttributToServer(domain_hdl->iend) ; 
   }
   
   void cxios_set_domain_ni(XDomainPtr domain_hdl, int ni)
   {
      domain_hdl->ni.setValue(ni);
      domain_hdl->sendAttributToServer(domain_hdl->ni) ; 
   }
   
   void cxios_set_domain_jbegin(XDomainPtr domain_hdl, int jbegin)
   {
      domain_hdl->jbegin.setValue(jbegin);
      domain_hdl->sendAttributToServer(domain_hdl->jbegin) ; 
   }
   
   void cxios_set_domain_jend(XDomainPtr domain_hdl, int jend)
   {
      domain_hdl->jend.setValue(jend);
      domain_hdl->sendAttributToServer(domain_hdl->jend) ; 
   }
   
   void cxios_set_domain_nj(XDomainPtr domain_hdl, int nj)
   {
      domain_hdl->nj.setValue(nj);
      domain_hdl->sendAttributToServer(domain_hdl->nj) ; 
   }
   
   void cxios_set_domain_mask(XDomainPtr domain_hdl, bool * mask, int mask_extent1, int mask_extent2)
   {
      ARRAY(bool, 2) mask_val(new CArray<bool, 2>(boost::extents [mask_extent1][mask_extent2]));
      std::copy(mask, &(mask[mask_val->num_elements()]), mask_val->data());

      domain_hdl->mask.setValue(mask_val);
      domain_hdl->sendAttributToServer(domain_hdl->mask) ; 
   }
   
   void cxios_set_domain_data_dim(XDomainPtr domain_hdl, int data_dim)
   {
      domain_hdl->data_dim.setValue(data_dim);
      domain_hdl->sendAttributToServer(domain_hdl->data_dim) ; 
   }
   
   void cxios_set_domain_data_ni(XDomainPtr domain_hdl, int data_ni)
   {
      domain_hdl->data_ni.setValue(data_ni);
      domain_hdl->sendAttributToServer(domain_hdl->data_ni) ; 
   }
   
   void cxios_set_domain_data_nj(XDomainPtr domain_hdl, int data_nj)
   {
      domain_hdl->data_nj.setValue(data_nj);
      domain_hdl->sendAttributToServer(domain_hdl->data_nj) ; 
   }
   
   void cxios_set_domain_data_ibegin(XDomainPtr domain_hdl, int data_ibegin)
   {
      domain_hdl->data_ibegin.setValue(data_ibegin);
      domain_hdl->sendAttributToServer(domain_hdl->data_ibegin) ; 
   }
   
   void cxios_set_domain_data_jbegin(XDomainPtr domain_hdl, int data_jbegin)
   {
      domain_hdl->data_jbegin.setValue(data_jbegin);
      domain_hdl->sendAttributToServer(domain_hdl->data_jbegin) ; 
   }
   
   void cxios_set_domain_zoom_ni(XDomainPtr domain_hdl, int zoom_ni)
   {
      domain_hdl->zoom_ni.setValue(zoom_ni);
      domain_hdl->sendAttributToServer(domain_hdl->zoom_ni) ; 
   }
   
   void cxios_set_domain_zoom_nj(XDomainPtr domain_hdl, int zoom_nj)
   {
      domain_hdl->zoom_nj.setValue(zoom_nj);
      domain_hdl->sendAttributToServer(domain_hdl->zoom_nj) ; 
   }
   
   void cxios_set_domain_zoom_ibegin(XDomainPtr domain_hdl, int zoom_ibegin)
   {
      domain_hdl->zoom_ibegin.setValue(zoom_ibegin);
      domain_hdl->sendAttributToServer(domain_hdl->zoom_ibegin) ; 
   }
    
   void cxios_set_domain_zoom_jbegin(XDomainPtr domain_hdl, int zoom_jbegin)
   {
      domain_hdl->zoom_jbegin.setValue(zoom_jbegin);
      domain_hdl->sendAttributToServer(domain_hdl->zoom_jbegin) ; 
   }
   
   void cxios_set_domain_data_n_index(XDomainPtr domain_hdl, int data_n_index)
   {
      domain_hdl->data_n_index.setValue(data_n_index);
      domain_hdl->sendAttributToServer(domain_hdl->data_n_index) ; 
   }
   
   void cxios_set_domain_data_i_index(XDomainPtr domain_hdl, int * data_i_index, int data_i_index_extent1)
   {
      ARRAY(int, 1) data_i_index_val(new CArray<int, 1>(boost::extents [data_i_index_extent1]));
      std::copy(data_i_index, &(data_i_index[data_i_index_val->num_elements()]), data_i_index_val->data());

      domain_hdl->data_i_index.setValue(data_i_index_val);
      domain_hdl->sendAttributToServer(domain_hdl->data_i_index) ; 
   }
   
   void cxios_set_domain_data_j_index(XDomainPtr domain_hdl, int * data_j_index, int data_j_index_extent1)
   {
      ARRAY(int, 1) data_j_index_val(new CArray<int, 1>(boost::extents [data_j_index_extent1]));
      std::copy(data_j_index, &(data_j_index[data_j_index_val->num_elements()]), data_j_index_val->data());

      domain_hdl->data_j_index.setValue(data_j_index_val);
      domain_hdl->sendAttributToServer(domain_hdl->data_j_index) ; 
   }
    
   void cxios_set_domain_lonvalue(XDomainPtr domain_hdl, double * lonvalue, int lonvalue_extent1)
   {
      ARRAY(double, 1) lonvalue_val(new CArray<double, 1>(boost::extents [lonvalue_extent1]));
      std::copy(lonvalue, &(lonvalue[lonvalue_val->num_elements()]), lonvalue_val->data());

      domain_hdl->lonvalue.setValue(lonvalue_val);
      domain_hdl->sendAttributToServer(domain_hdl->lonvalue) ; 
   }
   
   void cxios_set_domain_latvalue(XDomainPtr domain_hdl, double * latvalue, int latvalue_extent1)
   {
      ARRAY(double, 1) latvalue_val(new CArray<double, 1>(boost::extents [latvalue_extent1]));
      std::copy(latvalue, &(latvalue[latvalue_val->num_elements()]), latvalue_val->data());

      domain_hdl->latvalue.setValue(latvalue_val);
      domain_hdl->sendAttributToServer(domain_hdl->latvalue) ; 
   }   
   
   // -------------------- Attributs des groupes de domaine ------------------------
   
   
   void cxios_set_domaingroup_name(XDomainGroupPtr domaingroup_hdl, const char * name, int name_size)
   {
      std::string name_str; 
      if (!cstr2string(name, name_size, name_str)) return;

      domaingroup_hdl->name.setValue(name_str);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->name) ; 
      
   }
   
   void cxios_set_domaingroup_standard_name(XDomainGroupPtr domaingroup_hdl, const char * standard_name, int standard_name_size)
   {
      std::string standard_name_str; 
      if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;

      domaingroup_hdl->standard_name.setValue(standard_name_str);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->standard_name) ;
   }
   
   void cxios_set_domaingroup_long_name(XDomainGroupPtr domaingroup_hdl, const char * long_name, int long_name_size)
   {
      std::string long_name_str; 
      if (!cstr2string(long_name, long_name_size, long_name_str)) return;

      domaingroup_hdl->long_name.setValue(long_name_str);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->long_name) ;
   }
   
   void cxios_set_domaingroup_domain_group_ref(XDomainGroupPtr domaingroup_hdl, const char * domain_group_ref, int domain_group_ref_size)
   {
      std::string domain_group_ref_str; 
      if (!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;

      domaingroup_hdl->domain_group_ref.setValue(domain_group_ref_str);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->domain_group_ref) ;
   }
   
   void cxios_set_domaingroup_ni_glo(XDomainGroupPtr domaingroup_hdl, int ni_glo)
   {
      domaingroup_hdl->ni_glo.setValue(ni_glo);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni_glo) ;
   }
   
   void cxios_set_domaingroup_nj_glo(XDomainGroupPtr domaingroup_hdl, int nj_glo)
   {
      domaingroup_hdl->nj_glo.setValue(nj_glo);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj_glo) ;
   }
   
   void cxios_set_domaingroup_ibegin(XDomainGroupPtr domaingroup_hdl, int ibegin)
   {
      domaingroup_hdl->ibegin.setValue(ibegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ibegin) ;
   }
   
   void cxios_set_domaingroup_iend(XDomainGroupPtr domaingroup_hdl, int iend)
   {
      domaingroup_hdl->iend.setValue(iend);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->iend) ;
   }
   
   void cxios_set_domaingroup_ni(XDomainGroupPtr domaingroup_hdl, int ni)
   {
      domaingroup_hdl->ni.setValue(ni);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni) ;
   }
   
   void cxios_set_domaingroup_jbegin(XDomainGroupPtr domaingroup_hdl, int jbegin)
   {
      domaingroup_hdl->jbegin.setValue(jbegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jbegin) ;
   }
   
   void cxios_set_domaingroup_jend(XDomainGroupPtr domaingroup_hdl, int jend)
   {
      domaingroup_hdl->jend.setValue(jend);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jend) ;
   }
   
   void cxios_set_domaingroup_nj(XDomainGroupPtr domaingroup_hdl, int nj)
   {
      domaingroup_hdl->nj.setValue(nj);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj) ;
   }
   
   void cxios_set_domaingroup_mask(XDomainGroupPtr domaingroup_hdl, bool * mask, int mask_extent1, int mask_extent2)
   {
      ARRAY(bool, 2) mask_val(new CArray<bool, 2>(boost::extents [mask_extent1][mask_extent2]));
      std::copy(mask, &(mask[mask_val->num_elements()]), mask_val->data());

      domaingroup_hdl->mask.setValue(mask_val);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->mask) ;
   }
   
   void cxios_set_domaingroup_data_dim(XDomainGroupPtr domaingroup_hdl, int data_dim)
   {
      domaingroup_hdl->data_dim.setValue(data_dim);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_dim) ;
   }
   
   void cxios_set_domaingroup_data_ni(XDomainGroupPtr domaingroup_hdl, int data_ni)
   {
      domaingroup_hdl->data_ni.setValue(data_ni);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ni) ;
   }
   
   void cxios_set_domaingroup_data_nj(XDomainGroupPtr domaingroup_hdl, int data_nj)
   {
      domaingroup_hdl->data_nj.setValue(data_nj);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_nj) ;
   }
   
   void cxios_set_domaingroup_data_ibegin(XDomainGroupPtr domaingroup_hdl, int data_ibegin)
   {
      domaingroup_hdl->data_ibegin.setValue(data_ibegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ibegin) ;
   }
   
   void cxios_set_domaingroup_data_jbegin(XDomainGroupPtr domaingroup_hdl, int data_jbegin)
   {
      domaingroup_hdl->data_jbegin.setValue(data_jbegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_jbegin) ;
   }
   
   void cxios_set_domaingroup_zoom_ni(XDomainGroupPtr domaingroup_hdl, int zoom_ni)
   {
      domaingroup_hdl->zoom_ni.setValue(zoom_ni);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ni) ;
   }
   
   void cxios_set_domaingroup_zoom_nj(XDomainGroupPtr domaingroup_hdl, int zoom_nj)
   {
      domaingroup_hdl->zoom_nj.setValue(zoom_nj);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_nj) ;
   }
   
   void cxios_set_domaingroup_zoom_ibegin(XDomainGroupPtr domaingroup_hdl, int zoom_ibegin)
   {
      domaingroup_hdl->zoom_ibegin.setValue(zoom_ibegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ibegin) ;
   }
    
   void cxios_set_domaingroup_zoom_jbegin(XDomainGroupPtr domaingroup_hdl, int zoom_jbegin)
   {
      domaingroup_hdl->zoom_jbegin.setValue(zoom_jbegin);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_jbegin) ;
   }
   
   void cxios_set_domaingroup_data_n_index(XDomainGroupPtr domaingroup_hdl, int data_n_index)
   {
      domaingroup_hdl->data_n_index.setValue(data_n_index);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_n_index) ;
   }
   
   void cxios_set_domaingroup_data_i_index(XDomainGroupPtr domaingroup_hdl, int * data_i_index, int data_i_index_extent1)
   {
      ARRAY(int, 1) data_i_index_val(new CArray<int, 1>(boost::extents [data_i_index_extent1]));
      std::copy(data_i_index, &(data_i_index[data_i_index_val->num_elements()]), data_i_index_val->data());

      domaingroup_hdl->data_i_index.setValue(data_i_index_val);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_i_index) ;
   }
   
   void cxios_set_domaingroup_data_j_index(XDomainGroupPtr domaingroup_hdl, int * data_j_index, int data_j_index_extent1)
   {
      ARRAY(int, 1) data_j_index_val(new CArray<int, 1>(boost::extents [data_j_index_extent1]));
      std::copy(data_j_index, &(data_j_index[data_j_index_val->num_elements()]), data_j_index_val->data());

      domaingroup_hdl->data_j_index.setValue(data_j_index_val);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_j_index) ;
   }
    
   void cxios_set_domaingroup_lonvalue(XDomainGroupPtr domaingroup_hdl, double * lonvalue, int lonvalue_extent1)
   {
      ARRAY(double, 1) lonvalue_val(new CArray<double, 1>(boost::extents [lonvalue_extent1]));
      std::copy(lonvalue, &(lonvalue[lonvalue_val->num_elements()]), lonvalue_val->data());

      domaingroup_hdl->lonvalue.setValue(lonvalue_val);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->lonvalue) ;
   }
   
   void cxios_set_domaingroup_latvalue(XDomainGroupPtr domaingroup_hdl, double * latvalue, int latvalue_extent1)
   {
      ARRAY(double, 1) latvalue_val(new CArray<double, 1>(boost::extents [latvalue_extent1]));
      std::copy(latvalue, &(latvalue[latvalue_val->num_elements()]), latvalue_val->data());

      domaingroup_hdl->latvalue.setValue(latvalue_val);
      domaingroup_hdl->sendAttributToServer(domaingroup_hdl->latvalue) ;
   }  
   
   // ------------------------ Création des handle -----------------------------
   
   void cxios_domain_handle_create (XDomainPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CDomain>(id).get();
   }
   
   void cxios_domaingroup_handle_create (XDomainGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::GetObject<xmlioserver::tree::CDomainGroup>(id).get();
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_domain_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CDomain>(id);
   }

   void cxios_domaingroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      *_ret = xmlioserver::CObjectFactory::HasObject<xmlioserver::tree::CDomainGroup>(id);
   }
} // extern "C"
