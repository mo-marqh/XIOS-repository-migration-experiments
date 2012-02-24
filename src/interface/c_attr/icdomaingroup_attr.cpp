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
  typedef xmlioserver::tree::CDomainGroup*  domaingroup_Ptr;
  
  void cxios_set_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int data_dim)
  {
    domaingroup_hdl->data_dim.setValue(data_dim);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_dim);
  }
  
  void cxios_get_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int* data_dim)
  {
    *data_dim = domaingroup_hdl->data_dim.getValue();
  }
  
  
  void cxios_set_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int extent1)
  {
    ARRAY(int,1) array_tmp(new CArray<int,1>(boost::extents[extent1]));
    std::copy(data_i_index, &(data_i_index[array_tmp->num_elements()]), array_tmp->data());
    domaingroup_hdl->data_i_index.setValue(array_tmp);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_i_index);
  }
  
  void cxios_get_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int extent1)
  {
    if (!array_copy(domaingroup_hdl->data_i_index.getValue(), data_i_index, extent1))
     ERROR("void cxios_set_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int data_ibegin)
  {
    domaingroup_hdl->data_ibegin.setValue(data_ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ibegin);
  }
  
  void cxios_get_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int* data_ibegin)
  {
    *data_ibegin = domaingroup_hdl->data_ibegin.getValue();
  }
  
  
  void cxios_set_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int extent1)
  {
    ARRAY(int,1) array_tmp(new CArray<int,1>(boost::extents[extent1]));
    std::copy(data_j_index, &(data_j_index[array_tmp->num_elements()]), array_tmp->data());
    domaingroup_hdl->data_j_index.setValue(array_tmp);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_j_index);
  }
  
  void cxios_get_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int extent1)
  {
    if (!array_copy(domaingroup_hdl->data_j_index.getValue(), data_j_index, extent1))
     ERROR("void cxios_set_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int data_jbegin)
  {
    domaingroup_hdl->data_jbegin.setValue(data_jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_jbegin);
  }
  
  void cxios_get_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int* data_jbegin)
  {
    *data_jbegin = domaingroup_hdl->data_jbegin.getValue();
  }
  
  
  void cxios_set_domaingroup_data_n_index(domaingroup_Ptr domaingroup_hdl, int data_n_index)
  {
    domaingroup_hdl->data_n_index.setValue(data_n_index);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_n_index);
  }
  
  void cxios_get_domaingroup_data_n_index(domaingroup_Ptr domaingroup_hdl, int* data_n_index)
  {
    *data_n_index = domaingroup_hdl->data_n_index.getValue();
  }
  
  
  void cxios_set_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int data_ni)
  {
    domaingroup_hdl->data_ni.setValue(data_ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ni);
  }
  
  void cxios_get_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int* data_ni)
  {
    *data_ni = domaingroup_hdl->data_ni.getValue();
  }
  
  
  void cxios_set_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int data_nj)
  {
    domaingroup_hdl->data_nj.setValue(data_nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_nj);
  }
  
  void cxios_get_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int* data_nj)
  {
    *data_nj = domaingroup_hdl->data_nj.getValue();
  }
  
  
  void cxios_set_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, const char * domain_group_ref, int domain_group_ref_size)
  {
    std::string domain_group_ref_str;
    if(!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;
    domaingroup_hdl->domain_group_ref.setValue(domain_group_ref_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->domain_group_ref);
  }
  
  void cxios_get_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, char * domain_group_ref, int domain_group_ref_size)
  {
    if(!string_copy(domaingroup_hdl->domain_group_ref.getValue(),domain_group_ref , domain_group_ref_size))
      ERROR("void cxios_get_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, char * domain_group_ref, int domain_group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    domaingroup_hdl->group_ref.setValue(group_ref_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->group_ref);
  }
  
  void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)
  {
    if(!string_copy(domaingroup_hdl->group_ref.getValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int ibegin)
  {
    domaingroup_hdl->ibegin.setValue(ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ibegin);
  }
  
  void cxios_get_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int* ibegin)
  {
    *ibegin = domaingroup_hdl->ibegin.getValue();
  }
  
  
  void cxios_set_domaingroup_iend(domaingroup_Ptr domaingroup_hdl, int iend)
  {
    domaingroup_hdl->iend.setValue(iend);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->iend);
  }
  
  void cxios_get_domaingroup_iend(domaingroup_Ptr domaingroup_hdl, int* iend)
  {
    *iend = domaingroup_hdl->iend.getValue();
  }
  
  
  void cxios_set_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int jbegin)
  {
    domaingroup_hdl->jbegin.setValue(jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jbegin);
  }
  
  void cxios_get_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int* jbegin)
  {
    *jbegin = domaingroup_hdl->jbegin.getValue();
  }
  
  
  void cxios_set_domaingroup_jend(domaingroup_Ptr domaingroup_hdl, int jend)
  {
    domaingroup_hdl->jend.setValue(jend);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jend);
  }
  
  void cxios_get_domaingroup_jend(domaingroup_Ptr domaingroup_hdl, int* jend)
  {
    *jend = domaingroup_hdl->jend.getValue();
  }
  
  
  void cxios_set_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl, double* latvalue, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(latvalue, &(latvalue[array_tmp->num_elements()]), array_tmp->data());
    domaingroup_hdl->latvalue.setValue(array_tmp);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->latvalue);
  }
  
  void cxios_get_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl, double* latvalue, int extent1)
  {
    if (!array_copy(domaingroup_hdl->latvalue.getValue(), latvalue, extent1))
     ERROR("void cxios_set_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl, double* latvalue, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
    domaingroup_hdl->long_name.setValue(long_name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->long_name);
  }
  
  void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)
  {
    if(!string_copy(domaingroup_hdl->long_name.getValue(),long_name , long_name_size))
      ERROR("void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl, double* lonvalue, int extent1)
  {
    ARRAY(double,1) array_tmp(new CArray<double,1>(boost::extents[extent1]));
    std::copy(lonvalue, &(lonvalue[array_tmp->num_elements()]), array_tmp->data());
    domaingroup_hdl->lonvalue.setValue(array_tmp);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->lonvalue);
  }
  
  void cxios_get_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl, double* lonvalue, int extent1)
  {
    if (!array_copy(domaingroup_hdl->lonvalue.getValue(), lonvalue, extent1))
     ERROR("void cxios_set_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl, double* lonvalue, int extent1)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domaingroup_mask(domaingroup_Ptr domaingroup_hdl, bool* mask, int extent1, int extent2)
  {
    ARRAY(bool,2) array_tmp(new CArray<bool,2>(boost::extents[extent1][extent2]));
    std::copy(mask, &(mask[array_tmp->num_elements()]), array_tmp->data());
    domaingroup_hdl->mask.setValue(array_tmp);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->mask);
  }
  
  void cxios_get_domaingroup_mask(domaingroup_Ptr domaingroup_hdl, bool* mask, int extent1, int extent2)
  {
    if (!array_copy(domaingroup_hdl->mask.getValue(), mask, extent1, extent2))
     ERROR("void cxios_set_domaingroup_mask(domaingroup_Ptr domaingroup_hdl, bool* mask, int extent1, int extent2)",<<"Output array size is not conform to array size attribute") ;
  }
  
  
  void cxios_set_domaingroup_name(domaingroup_Ptr domaingroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    domaingroup_hdl->name.setValue(name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->name);
  }
  
  void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)
  {
    if(!string_copy(domaingroup_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int ni)
  {
    domaingroup_hdl->ni.setValue(ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni);
  }
  
  void cxios_get_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int* ni)
  {
    *ni = domaingroup_hdl->ni.getValue();
  }
  
  
  void cxios_set_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int ni_glo)
  {
    domaingroup_hdl->ni_glo.setValue(ni_glo);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni_glo);
  }
  
  void cxios_get_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int* ni_glo)
  {
    *ni_glo = domaingroup_hdl->ni_glo.getValue();
  }
  
  
  void cxios_set_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int nj)
  {
    domaingroup_hdl->nj.setValue(nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj);
  }
  
  void cxios_get_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int* nj)
  {
    *nj = domaingroup_hdl->nj.getValue();
  }
  
  
  void cxios_set_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int nj_glo)
  {
    domaingroup_hdl->nj_glo.setValue(nj_glo);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj_glo);
  }
  
  void cxios_get_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int* nj_glo)
  {
    *nj_glo = domaingroup_hdl->nj_glo.getValue();
  }
  
  
  void cxios_set_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    domaingroup_hdl->standard_name.setValue(standard_name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->standard_name);
  }
  
  void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)
  {
    if(!string_copy(domaingroup_hdl->standard_name.getValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_domaingroup_zoom_ibegin(domaingroup_Ptr domaingroup_hdl, int zoom_ibegin)
  {
    domaingroup_hdl->zoom_ibegin.setValue(zoom_ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ibegin);
  }
  
  void cxios_get_domaingroup_zoom_ibegin(domaingroup_Ptr domaingroup_hdl, int* zoom_ibegin)
  {
    *zoom_ibegin = domaingroup_hdl->zoom_ibegin.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_ibegin_loc(domaingroup_Ptr domaingroup_hdl, int zoom_ibegin_loc)
  {
    domaingroup_hdl->zoom_ibegin_loc.setValue(zoom_ibegin_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ibegin_loc);
  }
  
  void cxios_get_domaingroup_zoom_ibegin_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_ibegin_loc)
  {
    *zoom_ibegin_loc = domaingroup_hdl->zoom_ibegin_loc.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_jbegin(domaingroup_Ptr domaingroup_hdl, int zoom_jbegin)
  {
    domaingroup_hdl->zoom_jbegin.setValue(zoom_jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_jbegin);
  }
  
  void cxios_get_domaingroup_zoom_jbegin(domaingroup_Ptr domaingroup_hdl, int* zoom_jbegin)
  {
    *zoom_jbegin = domaingroup_hdl->zoom_jbegin.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_jbegin_loc(domaingroup_Ptr domaingroup_hdl, int zoom_jbegin_loc)
  {
    domaingroup_hdl->zoom_jbegin_loc.setValue(zoom_jbegin_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_jbegin_loc);
  }
  
  void cxios_get_domaingroup_zoom_jbegin_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_jbegin_loc)
  {
    *zoom_jbegin_loc = domaingroup_hdl->zoom_jbegin_loc.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_ni(domaingroup_Ptr domaingroup_hdl, int zoom_ni)
  {
    domaingroup_hdl->zoom_ni.setValue(zoom_ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ni);
  }
  
  void cxios_get_domaingroup_zoom_ni(domaingroup_Ptr domaingroup_hdl, int* zoom_ni)
  {
    *zoom_ni = domaingroup_hdl->zoom_ni.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_ni_loc(domaingroup_Ptr domaingroup_hdl, int zoom_ni_loc)
  {
    domaingroup_hdl->zoom_ni_loc.setValue(zoom_ni_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ni_loc);
  }
  
  void cxios_get_domaingroup_zoom_ni_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_ni_loc)
  {
    *zoom_ni_loc = domaingroup_hdl->zoom_ni_loc.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_nj(domaingroup_Ptr domaingroup_hdl, int zoom_nj)
  {
    domaingroup_hdl->zoom_nj.setValue(zoom_nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_nj);
  }
  
  void cxios_get_domaingroup_zoom_nj(domaingroup_Ptr domaingroup_hdl, int* zoom_nj)
  {
    *zoom_nj = domaingroup_hdl->zoom_nj.getValue();
  }
  
  
  void cxios_set_domaingroup_zoom_nj_loc(domaingroup_Ptr domaingroup_hdl, int zoom_nj_loc)
  {
    domaingroup_hdl->zoom_nj_loc.setValue(zoom_nj_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_nj_loc);
  }
  
  void cxios_get_domaingroup_zoom_nj_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_nj_loc)
  {
    *zoom_nj_loc = domaingroup_hdl->zoom_nj_loc.getValue();
  }
  
  
  
}
