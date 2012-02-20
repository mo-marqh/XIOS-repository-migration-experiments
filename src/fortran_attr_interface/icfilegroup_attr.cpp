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
  typedef xmlioserver::tree::CFileGroup*  filegroup_Ptr;
  
  void cxios_set_filegroup_description(filegroup_Ptr filegroup_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
    filegroup_hdl->description.setValue(description_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->description);
  }
  
  void cxios_get_filegroup_description(filegroup_Ptr filegroup_hdl, char * description, int description_size)
  {
    if(!string_copy(filegroup_hdl->description.getValue(),description , description_size))
      ERROR("void cxios_get_filegroup_description(filegroup_Ptr filegroup_hdl, char * description, int description_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_enabled(filegroup_Ptr filegroup_hdl, bool enabled)
  {
    filegroup_hdl->enabled.setValue(enabled);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->enabled);
  }
  
  void cxios_get_filegroup_enabled(filegroup_Ptr filegroup_hdl, bool* enabled)
  {
    *enabled = filegroup_hdl->enabled.getValue();
  }
  
  
  void cxios_set_filegroup_group_ref(filegroup_Ptr filegroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
    filegroup_hdl->group_ref.setValue(group_ref_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->group_ref);
  }
  
  void cxios_get_filegroup_group_ref(filegroup_Ptr filegroup_hdl, char * group_ref, int group_ref_size)
  {
    if(!string_copy(filegroup_hdl->group_ref.getValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_filegroup_group_ref(filegroup_Ptr filegroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_name(filegroup_Ptr filegroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    filegroup_hdl->name.setValue(name_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->name);
  }
  
  void cxios_get_filegroup_name(filegroup_Ptr filegroup_hdl, char * name, int name_size)
  {
    if(!string_copy(filegroup_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_filegroup_name(filegroup_Ptr filegroup_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, const char * name_suffix, int name_suffix_size)
  {
    std::string name_suffix_str;
    if(!cstr2string(name_suffix, name_suffix_size, name_suffix_str)) return;
    filegroup_hdl->name_suffix.setValue(name_suffix_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->name_suffix);
  }
  
  void cxios_get_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, char * name_suffix, int name_suffix_size)
  {
    if(!string_copy(filegroup_hdl->name_suffix.getValue(),name_suffix , name_suffix_size))
      ERROR("void cxios_get_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, char * name_suffix, int name_suffix_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_output_freq(filegroup_Ptr filegroup_hdl, const char * output_freq, int output_freq_size)
  {
    std::string output_freq_str;
    if(!cstr2string(output_freq, output_freq_size, output_freq_str)) return;
    filegroup_hdl->output_freq.setValue(output_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->output_freq);
  }
  
  void cxios_get_filegroup_output_freq(filegroup_Ptr filegroup_hdl, char * output_freq, int output_freq_size)
  {
    if(!string_copy(filegroup_hdl->output_freq.getValue(),output_freq , output_freq_size))
      ERROR("void cxios_get_filegroup_output_freq(filegroup_Ptr filegroup_hdl, char * output_freq, int output_freq_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_output_level(filegroup_Ptr filegroup_hdl, int output_level)
  {
    filegroup_hdl->output_level.setValue(output_level);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->output_level);
  }
  
  void cxios_get_filegroup_output_level(filegroup_Ptr filegroup_hdl, int* output_level)
  {
    *output_level = filegroup_hdl->output_level.getValue();
  }
  
  
  void cxios_set_filegroup_type(filegroup_Ptr filegroup_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if(!cstr2string(type, type_size, type_str)) return;
    filegroup_hdl->type.setValue(type_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->type);
  }
  
  void cxios_get_filegroup_type(filegroup_Ptr filegroup_hdl, char * type, int type_size)
  {
    if(!string_copy(filegroup_hdl->type.getValue(),type , type_size))
      ERROR("void cxios_get_filegroup_type(filegroup_Ptr filegroup_hdl, char * type, int type_size)", <<"Input string is to short");
  }
  
  
  
}
