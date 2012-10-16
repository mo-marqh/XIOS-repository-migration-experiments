/* ************************************************************************** *
 *               Interface auto generated - do not modify                   *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xmlioserver.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CDomain*  domain_Ptr;
  
  void cxios_set_domain_data_dim(domain_Ptr domain_hdl, int data_dim)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_dim.setValue(data_dim);
    domain_hdl->sendAttributToServer(domain_hdl->data_dim);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_dim(domain_Ptr domain_hdl, int* data_dim)
  {
    *data_dim = domain_hdl->data_dim.getValue();
  }
  
  
  void cxios_set_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index,shape(extent1),neverDeleteData) ;
    domain_hdl->data_i_index.reference(tmp.copy());
    domain_hdl->sendAttributToServer(domain_hdl->data_i_index);
  }
  
  void cxios_get_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    CArray<int,1> tmp(data_i_index,shape(extent1),neverDeleteData) ;
    tmp=domain_hdl->data_i_index ;
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_data_ibegin(domain_Ptr domain_hdl, int data_ibegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_ibegin.setValue(data_ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->data_ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_ibegin(domain_Ptr domain_hdl, int* data_ibegin)
  {
    *data_ibegin = domain_hdl->data_ibegin.getValue();
  }
  
  
  void cxios_set_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index,shape(extent1),neverDeleteData) ;
    domain_hdl->data_j_index.reference(tmp.copy());
    domain_hdl->sendAttributToServer(domain_hdl->data_j_index);
  }
  
  void cxios_get_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    CArray<int,1> tmp(data_j_index,shape(extent1),neverDeleteData) ;
    tmp=domain_hdl->data_j_index ;
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_data_jbegin(domain_Ptr domain_hdl, int data_jbegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_jbegin.setValue(data_jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->data_jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_jbegin(domain_Ptr domain_hdl, int* data_jbegin)
  {
    *data_jbegin = domain_hdl->data_jbegin.getValue();
  }
  
  
  void cxios_set_domain_data_n_index(domain_Ptr domain_hdl, int data_n_index)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_n_index.setValue(data_n_index);
    domain_hdl->sendAttributToServer(domain_hdl->data_n_index);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_n_index(domain_Ptr domain_hdl, int* data_n_index)
  {
    *data_n_index = domain_hdl->data_n_index.getValue();
  }
  
  
  void cxios_set_domain_data_ni(domain_Ptr domain_hdl, int data_ni)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_ni.setValue(data_ni);
    domain_hdl->sendAttributToServer(domain_hdl->data_ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_ni(domain_Ptr domain_hdl, int* data_ni)
  {
    *data_ni = domain_hdl->data_ni.getValue();
  }
  
  
  void cxios_set_domain_data_nj(domain_Ptr domain_hdl, int data_nj)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->data_nj.setValue(data_nj);
    domain_hdl->sendAttributToServer(domain_hdl->data_nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_data_nj(domain_Ptr domain_hdl, int* data_nj)
  {
    *data_nj = domain_hdl->data_nj.getValue();
  }
  
  
  void cxios_set_domain_domain_group_ref(domain_Ptr domain_hdl, const char * domain_group_ref, int domain_group_ref_size)
  {
    std::string domain_group_ref_str;
    if(!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;
     CTimer::get("XIOS").resume();
    domain_hdl->domain_group_ref.setValue(domain_group_ref_str);
    domain_hdl->sendAttributToServer(domain_hdl->domain_group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domain_hdl->domain_group_ref.getValue(),domain_group_ref , domain_group_ref_size))
      ERROR("void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_ibegin(domain_Ptr domain_hdl, int ibegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->ibegin.setValue(ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_ibegin(domain_Ptr domain_hdl, int* ibegin)
  {
    *ibegin = domain_hdl->ibegin.getValue();
  }
  
  
  void cxios_set_domain_iend(domain_Ptr domain_hdl, int iend)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->iend.setValue(iend);
    domain_hdl->sendAttributToServer(domain_hdl->iend);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_iend(domain_Ptr domain_hdl, int* iend)
  {
    *iend = domain_hdl->iend.getValue();
  }
  
  
  void cxios_set_domain_jbegin(domain_Ptr domain_hdl, int jbegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->jbegin.setValue(jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_jbegin(domain_Ptr domain_hdl, int* jbegin)
  {
    *jbegin = domain_hdl->jbegin.getValue();
  }
  
  
  void cxios_set_domain_jend(domain_Ptr domain_hdl, int jend)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->jend.setValue(jend);
    domain_hdl->sendAttributToServer(domain_hdl->jend);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_jend(domain_Ptr domain_hdl, int* jend)
  {
    *jend = domain_hdl->jend.getValue();
  }
  
  
  void cxios_set_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue,shape(extent1),neverDeleteData) ;
    domain_hdl->latvalue.reference(tmp.copy());
    domain_hdl->sendAttributToServer(domain_hdl->latvalue);
  }
  
  void cxios_get_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    CArray<double,1> tmp(latvalue,shape(extent1),neverDeleteData) ;
    tmp=domain_hdl->latvalue ;
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_long_name(domain_Ptr domain_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
     CTimer::get("XIOS").resume();
    domain_hdl->long_name.setValue(long_name_str);
    domain_hdl->sendAttributToServer(domain_hdl->long_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domain_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue,shape(extent1),neverDeleteData) ;
    domain_hdl->lonvalue.reference(tmp.copy());
    domain_hdl->sendAttributToServer(domain_hdl->lonvalue);
  }
  
  void cxios_get_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    CArray<double,1> tmp(lonvalue,shape(extent1),neverDeleteData) ;
    tmp=domain_hdl->lonvalue ;
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask,shape(extent1,extent2),neverDeleteData) ;
    domain_hdl->mask.reference(tmp.copy());
    domain_hdl->sendAttributToServer(domain_hdl->mask);
  }
  
  void cxios_get_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    CArray<bool,2> tmp(mask,shape(extent1,extent2),neverDeleteData) ;
    tmp=domain_hdl->mask ;
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_name(domain_Ptr domain_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    domain_hdl->name.setValue(name_str);
    domain_hdl->sendAttributToServer(domain_hdl->name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domain_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_ni(domain_Ptr domain_hdl, int ni)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->ni.setValue(ni);
    domain_hdl->sendAttributToServer(domain_hdl->ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_ni(domain_Ptr domain_hdl, int* ni)
  {
    *ni = domain_hdl->ni.getValue();
  }
  
  
  void cxios_set_domain_ni_glo(domain_Ptr domain_hdl, int ni_glo)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->ni_glo.setValue(ni_glo);
    domain_hdl->sendAttributToServer(domain_hdl->ni_glo);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_ni_glo(domain_Ptr domain_hdl, int* ni_glo)
  {
    *ni_glo = domain_hdl->ni_glo.getValue();
  }
  
  
  void cxios_set_domain_nj(domain_Ptr domain_hdl, int nj)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->nj.setValue(nj);
    domain_hdl->sendAttributToServer(domain_hdl->nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_nj(domain_Ptr domain_hdl, int* nj)
  {
    *nj = domain_hdl->nj.getValue();
  }
  
  
  void cxios_set_domain_nj_glo(domain_Ptr domain_hdl, int nj_glo)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->nj_glo.setValue(nj_glo);
    domain_hdl->sendAttributToServer(domain_hdl->nj_glo);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_nj_glo(domain_Ptr domain_hdl, int* nj_glo)
  {
    *nj_glo = domain_hdl->nj_glo.getValue();
  }
  
  
  void cxios_set_domain_standard_name(domain_Ptr domain_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
     CTimer::get("XIOS").resume();
    domain_hdl->standard_name.setValue(standard_name_str);
    domain_hdl->sendAttributToServer(domain_hdl->standard_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domain_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  
  void cxios_set_domain_zoom_ibegin(domain_Ptr domain_hdl, int zoom_ibegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_ibegin.setValue(zoom_ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_ibegin(domain_Ptr domain_hdl, int* zoom_ibegin)
  {
    *zoom_ibegin = domain_hdl->zoom_ibegin.getValue();
  }
  
  
  void cxios_set_domain_zoom_ibegin_loc(domain_Ptr domain_hdl, int zoom_ibegin_loc)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_ibegin_loc.setValue(zoom_ibegin_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ibegin_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_ibegin_loc(domain_Ptr domain_hdl, int* zoom_ibegin_loc)
  {
    *zoom_ibegin_loc = domain_hdl->zoom_ibegin_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_jbegin(domain_Ptr domain_hdl, int zoom_jbegin)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_jbegin.setValue(zoom_jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_jbegin(domain_Ptr domain_hdl, int* zoom_jbegin)
  {
    *zoom_jbegin = domain_hdl->zoom_jbegin.getValue();
  }
  
  
  void cxios_set_domain_zoom_jbegin_loc(domain_Ptr domain_hdl, int zoom_jbegin_loc)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_jbegin_loc.setValue(zoom_jbegin_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_jbegin_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_jbegin_loc(domain_Ptr domain_hdl, int* zoom_jbegin_loc)
  {
    *zoom_jbegin_loc = domain_hdl->zoom_jbegin_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_ni(domain_Ptr domain_hdl, int zoom_ni)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_ni.setValue(zoom_ni);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_ni(domain_Ptr domain_hdl, int* zoom_ni)
  {
    *zoom_ni = domain_hdl->zoom_ni.getValue();
  }
  
  
  void cxios_set_domain_zoom_ni_loc(domain_Ptr domain_hdl, int zoom_ni_loc)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_ni_loc.setValue(zoom_ni_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ni_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_ni_loc(domain_Ptr domain_hdl, int* zoom_ni_loc)
  {
    *zoom_ni_loc = domain_hdl->zoom_ni_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_nj(domain_Ptr domain_hdl, int zoom_nj)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_nj.setValue(zoom_nj);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_nj(domain_Ptr domain_hdl, int* zoom_nj)
  {
    *zoom_nj = domain_hdl->zoom_nj.getValue();
  }
  
  
  void cxios_set_domain_zoom_nj_loc(domain_Ptr domain_hdl, int zoom_nj_loc)
  {
     CTimer::get("XIOS").resume();
    domain_hdl->zoom_nj_loc.setValue(zoom_nj_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_nj_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domain_zoom_nj_loc(domain_Ptr domain_hdl, int* zoom_nj_loc)
  {
    *zoom_nj_loc = domain_hdl->zoom_nj_loc.getValue();
  }
  
  
  
}
