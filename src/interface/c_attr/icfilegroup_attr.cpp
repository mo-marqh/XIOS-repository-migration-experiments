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
  typedef xios::tree::CFileGroup*  filegroup_Ptr;
  
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
  
  
  void cxios_set_filegroup_par_access(filegroup_Ptr filegroup_hdl, const char * par_access, int par_access_size)
  {
    std::string par_access_str;
    if(!cstr2string(par_access, par_access_size, par_access_str)) return;
    filegroup_hdl->par_access.setValue(par_access_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->par_access);
  }
  
  void cxios_get_filegroup_par_access(filegroup_Ptr filegroup_hdl, char * par_access, int par_access_size)
  {
    if(!string_copy(filegroup_hdl->par_access.getValue(),par_access , par_access_size))
      ERROR("void cxios_get_filegroup_par_access(filegroup_Ptr filegroup_hdl, char * par_access, int par_access_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_split_freq(filegroup_Ptr filegroup_hdl, const char * split_freq, int split_freq_size)
  {
    std::string split_freq_str;
    if(!cstr2string(split_freq, split_freq_size, split_freq_str)) return;
    filegroup_hdl->split_freq.setValue(split_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->split_freq);
  }
  
  void cxios_get_filegroup_split_freq(filegroup_Ptr filegroup_hdl, char * split_freq, int split_freq_size)
  {
    if(!string_copy(filegroup_hdl->split_freq.getValue(),split_freq , split_freq_size))
      ERROR("void cxios_get_filegroup_split_freq(filegroup_Ptr filegroup_hdl, char * split_freq, int split_freq_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, const char * sync_freq, int sync_freq_size)
  {
    std::string sync_freq_str;
    if(!cstr2string(sync_freq, sync_freq_size, sync_freq_str)) return;
    filegroup_hdl->sync_freq.setValue(sync_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->sync_freq);
  }
  
  void cxios_get_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, char * sync_freq, int sync_freq_size)
  {
    if(!string_copy(filegroup_hdl->sync_freq.getValue(),sync_freq , sync_freq_size))
      ERROR("void cxios_get_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, char * sync_freq, int sync_freq_size)", <<"Input string is to short");
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
