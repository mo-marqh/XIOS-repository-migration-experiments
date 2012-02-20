/* ************************************************************************** *
 *               Interface auto generated - do not modify                   *
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
  typedef xmlioserver::tree::CDomain*  domain_Ptr;
  
  void cxios_set_domain_data_dim(domain_Ptr domain_hdl, int data_dim)
  {
    domain_hdl->data_dim.setValue(data_dim);
    domain_hdl->sendAttributToServer(domain_hdl->data_dim);
  }
  
  void cxios_get_domain_data_dim(domain_Ptr domain_hdl, int* data_dim)
  {
    *data_dim = domain_hdl->data_dim.getValue();
  }
  
  
  void cxios_set_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    ARRAY(int,1) array_tmp(new CArray<int,1>(boost::extents[extent1]));
    std::copy(data_i_index, &(data_i_index[array_tmp->num_elements()]), array_tmp->data());
    domain_hdl->data_i_index.setValue(array_tmp);
    domain_hdl->sendAttributToServer(domain_hdl->data_i_index);
  }
  
  void cxios_get_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    if (!array_copy(domain_hdl->data_i_index.getValue(), data_i_index, extent1))
     ERROR("void cxios_set_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domain_data_ibegin(domain_Ptr domain_hdl, int data_ibegin)
  {
    domain_hdl->data_ibegin.setValue(data_ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->data_ibegin);
  }
  
  void cxios_get_domain_data_ibegin(domain_Ptr domain_hdl, int* data_ibegin)
  {
    *data_ibegin = domain_hdl->data_ibegin.getValue();
  }
  
  
  void cxios_set_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    ARRAY(int,1) array_tmp(new CArray<int,1>(boost::extents[extent1]));
    std::copy(data_j_index, &(data_j_index[array_tmp->num_elements()]), array_tmp->data());
    domain_hdl->data_j_index.setValue(array_tmp);
    domain_hdl->sendAttributToServer(domain_hdl->data_j_index);
  }
  
  void cxios_get_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    if (!array_copy(domain_hdl->data_j_index.getValue(), data_j_index, extent1))
     ERROR("void cxios_set_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domain_data_jbegin(domain_Ptr domain_hdl, int data_jbegin)
  {
    domain_hdl->data_jbegin.setValue(data_jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->data_jbegin);
  }
  
  void cxios_get_domain_data_jbegin(domain_Ptr domain_hdl, int* data_jbegin)
  {
    *data_jbegin = domain_hdl->data_jbegin.getValue();
  }
  
  
  void cxios_set_domain_data_n_index(domain_Ptr domain_hdl, int data_n_index)
  {
    domain_hdl->data_n_index.setValue(data_n_index);
    domain_hdl->sendAttributToServer(domain_hdl->data_n_index);
  }
  
  void cxios_get_domain_data_n_index(domain_Ptr domain_hdl, int* data_n_index)
  {
    *data_n_index = domain_hdl->data_n_index.getValue();
  }
  
  
  void cxios_set_domain_data_ni(domain_Ptr domain_hdl, int data_ni)
  {
    domain_hdl->data_ni.setValue(data_ni);
    domain_hdl->sendAttributToServer(domain_hdl->data_ni);
  }
  
  void cxios_get_domain_data_ni(domain_Ptr domain_hdl, int* data_ni)
  {
    *data_ni = domain_hdl->data_ni.getValue();
  }
  
  
  void cxios_set_domain_data_nj(domain_Ptr domain_hdl, int data_nj)
  {
    domain_hdl->data_nj.setValue(data_nj);
    domain_hdl->sendAttributToServer(domain_hdl->data_nj);
  }
  
  void cxios_get_domain_data_nj(domain_Ptr domain_hdl, int* data_nj)
  {
    *data_nj = domain_hdl->data_nj.getValue();
  }
  
  
  void cxios_set_domain_domain_group_ref(domain_Ptr domain_hdl, const char * domain_group_ref, int domain_group_ref_size)
  {
    std::string domain_group_ref_str;
    if(!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;
    domain_hdl->domain_group_ref.setValue(domain_group_ref_str);
    domain_hdl->sendAttributToServer(domain_hdl->domain_group_ref);
  }
  
  void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)
  {
    if(!string_copy(domain_hdl->domain_group_ref.getValue(),domain_group_ref , domain_group_ref_size))
      ERROR("void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domain_ibegin(domain_Ptr domain_hdl, int ibegin)
  {
    domain_hdl->ibegin.setValue(ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->ibegin);
  }
  
  void cxios_get_domain_ibegin(domain_Ptr domain_hdl, int* ibegin)
  {
    *ibegin = domain_hdl->ibegin.getValue();
  }
  
  
  void cxios_set_domain_iend(domain_Ptr domain_hdl, int iend)
  {
    domain_hdl->iend.setValue(iend);
    domain_hdl->sendAttributToServer(domain_hdl->iend);
  }
  
  void cxios_get_domain_iend(domain_Ptr domain_hdl, int* iend)
  {
    *iend = domain_hdl->iend.getValue();
  }
  
  
  void cxios_set_domain_jbegin(domain_Ptr domain_hdl, int jbegin)
  {
    domain_hdl->jbegin.setValue(jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->jbegin);
  }
  
  void cxios_get_domain_jbegin(domain_Ptr domain_hdl, int* jbegin)
  {
    *jbegin = domain_hdl->jbegin.getValue();
  }
  
  
  void cxios_set_domain_jend(domain_Ptr domain_hdl, int jend)
  {
    domain_hdl->jend.setValue(jend);
    domain_hdl->sendAttributToServer(domain_hdl->jend);
  }
  
  void cxios_get_domain_jend(domain_Ptr domain_hdl, int* jend)
  {
    *jend = domain_hdl->jend.getValue();
  }
  
  
  void cxios_set_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(latvalue, &(latvalue[array_tmp->num_elements()]), array_tmp->data());
    domain_hdl->latvalue.setValue(array_tmp);
    domain_hdl->sendAttributToServer(domain_hdl->latvalue);
  }
  
  void cxios_get_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    if (!array_copy(domain_hdl->latvalue.getValue(), latvalue, extent1))
     ERROR("void cxios_set_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domain_long_name(domain_Ptr domain_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
    domain_hdl->long_name.setValue(long_name_str);
    domain_hdl->sendAttributToServer(domain_hdl->long_name);
  }
  
  void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)
  {
    if(!string_copy(domain_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(lonvalue, &(lonvalue[array_tmp->num_elements()]), array_tmp->data());
    domain_hdl->lonvalue.setValue(array_tmp);
    domain_hdl->sendAttributToServer(domain_hdl->lonvalue);
  }
  
  void cxios_get_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    if (!array_copy(domain_hdl->lonvalue.getValue(), lonvalue, extent1))
     ERROR("void cxios_set_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    ARRAY(bool,2) array_tmp(new CArray<bool,2>(boost::extents[extent1][extent2]));
    std::copy(mask, &(mask[array_tmp->num_elements()]), array_tmp->data());
    domain_hdl->mask.setValue(array_tmp);
    domain_hdl->sendAttributToServer(domain_hdl->mask);
  }
  
  void cxios_get_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    if (!array_copy(domain_hdl->mask.getValue(), mask, extent1, extent2))
     ERROR("void cxios_set_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domain_name(domain_Ptr domain_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    domain_hdl->name.setValue(name_str);
    domain_hdl->sendAttributToServer(domain_hdl->name);
  }
  
  void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)
  {
    if(!string_copy(domain_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domain_ni(domain_Ptr domain_hdl, int ni)
  {
    domain_hdl->ni.setValue(ni);
    domain_hdl->sendAttributToServer(domain_hdl->ni);
  }
  
  void cxios_get_domain_ni(domain_Ptr domain_hdl, int* ni)
  {
    *ni = domain_hdl->ni.getValue();
  }
  
  
  void cxios_set_domain_ni_glo(domain_Ptr domain_hdl, int ni_glo)
  {
    domain_hdl->ni_glo.setValue(ni_glo);
    domain_hdl->sendAttributToServer(domain_hdl->ni_glo);
  }
  
  void cxios_get_domain_ni_glo(domain_Ptr domain_hdl, int* ni_glo)
  {
    *ni_glo = domain_hdl->ni_glo.getValue();
  }
  
  
  void cxios_set_domain_nj(domain_Ptr domain_hdl, int nj)
  {
    domain_hdl->nj.setValue(nj);
    domain_hdl->sendAttributToServer(domain_hdl->nj);
  }
  
  void cxios_get_domain_nj(domain_Ptr domain_hdl, int* nj)
  {
    *nj = domain_hdl->nj.getValue();
  }
  
  
  void cxios_set_domain_nj_glo(domain_Ptr domain_hdl, int nj_glo)
  {
    domain_hdl->nj_glo.setValue(nj_glo);
    domain_hdl->sendAttributToServer(domain_hdl->nj_glo);
  }
  
  void cxios_get_domain_nj_glo(domain_Ptr domain_hdl, int* nj_glo)
  {
    *nj_glo = domain_hdl->nj_glo.getValue();
  }
  
  
  void cxios_set_domain_standard_name(domain_Ptr domain_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    domain_hdl->standard_name.setValue(standard_name_str);
    domain_hdl->sendAttributToServer(domain_hdl->standard_name);
  }
  
  void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)
  {
    if(!string_copy(domain_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domain_zoom_ibegin(domain_Ptr domain_hdl, int zoom_ibegin)
  {
    domain_hdl->zoom_ibegin.setValue(zoom_ibegin);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ibegin);
  }
  
  void cxios_get_domain_zoom_ibegin(domain_Ptr domain_hdl, int* zoom_ibegin)
  {
    *zoom_ibegin = domain_hdl->zoom_ibegin.getValue();
  }
  
  
  void cxios_set_domain_zoom_ibegin_loc(domain_Ptr domain_hdl, int zoom_ibegin_loc)
  {
    domain_hdl->zoom_ibegin_loc.setValue(zoom_ibegin_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ibegin_loc);
  }
  
  void cxios_get_domain_zoom_ibegin_loc(domain_Ptr domain_hdl, int* zoom_ibegin_loc)
  {
    *zoom_ibegin_loc = domain_hdl->zoom_ibegin_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_jbegin(domain_Ptr domain_hdl, int zoom_jbegin)
  {
    domain_hdl->zoom_jbegin.setValue(zoom_jbegin);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_jbegin);
  }
  
  void cxios_get_domain_zoom_jbegin(domain_Ptr domain_hdl, int* zoom_jbegin)
  {
    *zoom_jbegin = domain_hdl->zoom_jbegin.getValue();
  }
  
  
  void cxios_set_domain_zoom_jbegin_loc(domain_Ptr domain_hdl, int zoom_jbegin_loc)
  {
    domain_hdl->zoom_jbegin_loc.setValue(zoom_jbegin_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_jbegin_loc);
  }
  
  void cxios_get_domain_zoom_jbegin_loc(domain_Ptr domain_hdl, int* zoom_jbegin_loc)
  {
    *zoom_jbegin_loc = domain_hdl->zoom_jbegin_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_ni(domain_Ptr domain_hdl, int zoom_ni)
  {
    domain_hdl->zoom_ni.setValue(zoom_ni);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ni);
  }
  
  void cxios_get_domain_zoom_ni(domain_Ptr domain_hdl, int* zoom_ni)
  {
    *zoom_ni = domain_hdl->zoom_ni.getValue();
  }
  
  
  void cxios_set_domain_zoom_ni_loc(domain_Ptr domain_hdl, int zoom_ni_loc)
  {
    domain_hdl->zoom_ni_loc.setValue(zoom_ni_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_ni_loc);
  }
  
  void cxios_get_domain_zoom_ni_loc(domain_Ptr domain_hdl, int* zoom_ni_loc)
  {
    *zoom_ni_loc = domain_hdl->zoom_ni_loc.getValue();
  }
  
  
  void cxios_set_domain_zoom_nj(domain_Ptr domain_hdl, int zoom_nj)
  {
    domain_hdl->zoom_nj.setValue(zoom_nj);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_nj);
  }
  
  void cxios_get_domain_zoom_nj(domain_Ptr domain_hdl, int* zoom_nj)
  {
    *zoom_nj = domain_hdl->zoom_nj.getValue();
  }
  
  
  void cxios_set_domain_zoom_nj_loc(domain_Ptr domain_hdl, int zoom_nj_loc)
  {
    domain_hdl->zoom_nj_loc.setValue(zoom_nj_loc);
    domain_hdl->sendAttributToServer(domain_hdl->zoom_nj_loc);
  }
  
  void cxios_get_domain_zoom_nj_loc(domain_Ptr domain_hdl, int* zoom_nj_loc)
  {
    *zoom_nj_loc = domain_hdl->zoom_nj_loc.getValue();
  }
  
  
  
}
