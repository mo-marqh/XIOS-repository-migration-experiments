/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xios.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "icdate.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CDomain* domain_Ptr;

  void cxios_set_domain_area(domain_Ptr domain_hdl, double* area, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(area, shape(extent1, extent2), neverDeleteData);
    domain_hdl->area.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_area(domain_Ptr domain_hdl, double* area, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(area, shape(extent1, extent2), neverDeleteData);
    tmp=domain_hdl->area.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_area(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->area.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_bounds_lat(domain_Ptr domain_hdl, double* bounds_lat, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lat, shape(extent1, extent2), neverDeleteData);
    domain_hdl->bounds_lat.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_bounds_lat(domain_Ptr domain_hdl, double* bounds_lat, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lat, shape(extent1, extent2), neverDeleteData);
    tmp=domain_hdl->bounds_lat.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_bounds_lat(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->bounds_lat.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_bounds_lon(domain_Ptr domain_hdl, double* bounds_lon, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lon, shape(extent1, extent2), neverDeleteData);
    domain_hdl->bounds_lon.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_bounds_lon(domain_Ptr domain_hdl, double* bounds_lon, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<double,2> tmp(bounds_lon, shape(extent1, extent2), neverDeleteData);
    tmp=domain_hdl->bounds_lon.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_bounds_lon(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->bounds_lon.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_dim(domain_Ptr domain_hdl, int data_dim)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_dim.setValue(data_dim);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_dim(domain_Ptr domain_hdl, int* data_dim)
  {
    CTimer::get("XIOS").resume();
    *data_dim = domain_hdl->data_dim.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_dim(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_dim.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index, shape(extent1), neverDeleteData);
    domain_hdl->data_i_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_i_index(domain_Ptr domain_hdl, int* data_i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index, shape(extent1), neverDeleteData);
    tmp=domain_hdl->data_i_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_i_index(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_i_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_ibegin(domain_Ptr domain_hdl, int data_ibegin)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_ibegin.setValue(data_ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_ibegin(domain_Ptr domain_hdl, int* data_ibegin)
  {
    CTimer::get("XIOS").resume();
    *data_ibegin = domain_hdl->data_ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_ibegin(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index, shape(extent1), neverDeleteData);
    domain_hdl->data_j_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_j_index(domain_Ptr domain_hdl, int* data_j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index, shape(extent1), neverDeleteData);
    tmp=domain_hdl->data_j_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_j_index(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_j_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_jbegin(domain_Ptr domain_hdl, int data_jbegin)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_jbegin.setValue(data_jbegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_jbegin(domain_Ptr domain_hdl, int* data_jbegin)
  {
    CTimer::get("XIOS").resume();
    *data_jbegin = domain_hdl->data_jbegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_jbegin(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_n_index(domain_Ptr domain_hdl, int data_n_index)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_n_index.setValue(data_n_index);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_n_index(domain_Ptr domain_hdl, int* data_n_index)
  {
    CTimer::get("XIOS").resume();
    *data_n_index = domain_hdl->data_n_index.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_n_index(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_n_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_ni(domain_Ptr domain_hdl, int data_ni)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_ni.setValue(data_ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_ni(domain_Ptr domain_hdl, int* data_ni)
  {
    CTimer::get("XIOS").resume();
    *data_ni = domain_hdl->data_ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_ni(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_data_nj(domain_Ptr domain_hdl, int data_nj)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->data_nj.setValue(data_nj);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_data_nj(domain_Ptr domain_hdl, int* data_nj)
  {
    CTimer::get("XIOS").resume();
    *data_nj = domain_hdl->data_nj.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_data_nj(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->data_nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_domain_group_ref(domain_Ptr domain_hdl, const char * domain_group_ref, int domain_group_ref_size)
  {
    std::string domain_group_ref_str;
    if (!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->domain_group_ref.setValue(domain_group_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->domain_group_ref.getInheritedValue(), domain_group_ref, domain_group_ref_size))
      ERROR("void cxios_get_domain_domain_group_ref(domain_Ptr domain_hdl, char * domain_group_ref, int domain_group_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_domain_group_ref(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->domain_group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_domain_ref(domain_Ptr domain_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if (!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->domain_ref.setValue(domain_ref_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_domain_ref(domain_Ptr domain_hdl, char * domain_ref, int domain_ref_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->domain_ref.getInheritedValue(), domain_ref, domain_ref_size))
      ERROR("void cxios_get_domain_domain_ref(domain_Ptr domain_hdl, char * domain_ref, int domain_ref_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_domain_ref(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->domain_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_i_index(domain_Ptr domain_hdl, int* i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(i_index, shape(extent1), neverDeleteData);
    domain_hdl->i_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_i_index(domain_Ptr domain_hdl, int* i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(i_index, shape(extent1), neverDeleteData);
    tmp=domain_hdl->i_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_i_index(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->i_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_ibegin(domain_Ptr domain_hdl, int ibegin)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->ibegin.setValue(ibegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_ibegin(domain_Ptr domain_hdl, int* ibegin)
  {
    CTimer::get("XIOS").resume();
    *ibegin = domain_hdl->ibegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_ibegin(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_j_index(domain_Ptr domain_hdl, int* j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(j_index, shape(extent1), neverDeleteData);
    domain_hdl->j_index.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_j_index(domain_Ptr domain_hdl, int* j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(j_index, shape(extent1), neverDeleteData);
    tmp=domain_hdl->j_index.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_j_index(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->j_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_jbegin(domain_Ptr domain_hdl, int jbegin)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->jbegin.setValue(jbegin);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_jbegin(domain_Ptr domain_hdl, int* jbegin)
  {
    CTimer::get("XIOS").resume();
    *jbegin = domain_hdl->jbegin.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_jbegin(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue, shape(extent1), neverDeleteData);
    domain_hdl->latvalue.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_latvalue(domain_Ptr domain_hdl, double* latvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue, shape(extent1), neverDeleteData);
    tmp=domain_hdl->latvalue.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_latvalue(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->latvalue.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_long_name(domain_Ptr domain_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if (!cstr2string(long_name, long_name_size, long_name_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->long_name.setValue(long_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->long_name.getInheritedValue(), long_name, long_name_size))
      ERROR("void cxios_get_domain_long_name(domain_Ptr domain_hdl, char * long_name, int long_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_long_name(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue, shape(extent1), neverDeleteData);
    domain_hdl->lonvalue.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_lonvalue(domain_Ptr domain_hdl, double* lonvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue, shape(extent1), neverDeleteData);
    tmp=domain_hdl->lonvalue.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_lonvalue(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->lonvalue.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask, shape(extent1, extent2), neverDeleteData);
    domain_hdl->mask.reference(tmp.copy());
     CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_mask(domain_Ptr domain_hdl, bool* mask, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask, shape(extent1, extent2), neverDeleteData);
    tmp=domain_hdl->mask.getInheritedValue();
     CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_mask(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_name(domain_Ptr domain_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if (!cstr2string(name, name_size, name_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->name.setValue(name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->name.getInheritedValue(), name, name_size))
      ERROR("void cxios_get_domain_name(domain_Ptr domain_hdl, char * name, int name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_name(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_ni(domain_Ptr domain_hdl, int ni)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->ni.setValue(ni);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_ni(domain_Ptr domain_hdl, int* ni)
  {
    CTimer::get("XIOS").resume();
    *ni = domain_hdl->ni.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_ni(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_ni_glo(domain_Ptr domain_hdl, int ni_glo)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->ni_glo.setValue(ni_glo);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_ni_glo(domain_Ptr domain_hdl, int* ni_glo)
  {
    CTimer::get("XIOS").resume();
    *ni_glo = domain_hdl->ni_glo.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_ni_glo(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->ni_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_nj(domain_Ptr domain_hdl, int nj)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->nj.setValue(nj);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_nj(domain_Ptr domain_hdl, int* nj)
  {
    CTimer::get("XIOS").resume();
    *nj = domain_hdl->nj.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_nj(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_nj_glo(domain_Ptr domain_hdl, int nj_glo)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->nj_glo.setValue(nj_glo);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_nj_glo(domain_Ptr domain_hdl, int* nj_glo)
  {
    CTimer::get("XIOS").resume();
    *nj_glo = domain_hdl->nj_glo.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_nj_glo(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->nj_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_nvertex(domain_Ptr domain_hdl, int nvertex)
  {
    CTimer::get("XIOS").resume();
    domain_hdl->nvertex.setValue(nvertex);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_nvertex(domain_Ptr domain_hdl, int* nvertex)
  {
    CTimer::get("XIOS").resume();
    *nvertex = domain_hdl->nvertex.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_nvertex(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->nvertex.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_standard_name(domain_Ptr domain_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if (!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->standard_name.setValue(standard_name_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->standard_name.getInheritedValue(), standard_name, standard_name_size))
      ERROR("void cxios_get_domain_standard_name(domain_Ptr domain_hdl, char * standard_name, int standard_name_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_standard_name(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_domain_type(domain_Ptr domain_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if (!cstr2string(type, type_size, type_str)) return;
    CTimer::get("XIOS").resume();
    domain_hdl->type.fromString(type_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_domain_type(domain_Ptr domain_hdl, char * type, int type_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(domain_hdl->type.getInheritedStringValue(), type, type_size))
      ERROR("void cxios_get_domain_type(domain_Ptr domain_hdl, char * type, int type_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_domain_type(domain_Ptr domain_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = domain_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
