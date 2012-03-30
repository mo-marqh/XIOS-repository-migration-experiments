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
  typedef xios::tree::CFile*  file_Ptr;
  
  void cxios_set_file_description(file_Ptr file_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
    file_hdl->description.setValue(description_str);
    file_hdl->sendAttributToServer(file_hdl->description);
  }
  
  void cxios_get_file_description(file_Ptr file_hdl, char * description, int description_size)
  {
    if(!string_copy(file_hdl->description.getValue(),description , description_size))
      ERROR("void cxios_get_file_description(file_Ptr file_hdl, char * description, int description_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_enabled(file_Ptr file_hdl, bool enabled)
  {
    file_hdl->enabled.setValue(enabled);
    file_hdl->sendAttributToServer(file_hdl->enabled);
  }
  
  void cxios_get_file_enabled(file_Ptr file_hdl, bool* enabled)
  {
    *enabled = file_hdl->enabled.getValue();
  }
  
  
  void cxios_set_file_name(file_Ptr file_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
    file_hdl->name.setValue(name_str);
    file_hdl->sendAttributToServer(file_hdl->name);
  }
  
  void cxios_get_file_name(file_Ptr file_hdl, char * name, int name_size)
  {
    if(!string_copy(file_hdl->name.getValue(),name , name_size))
      ERROR("void cxios_get_file_name(file_Ptr file_hdl, char * name, int name_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_name_suffix(file_Ptr file_hdl, const char * name_suffix, int name_suffix_size)
  {
    std::string name_suffix_str;
    if(!cstr2string(name_suffix, name_suffix_size, name_suffix_str)) return;
    file_hdl->name_suffix.setValue(name_suffix_str);
    file_hdl->sendAttributToServer(file_hdl->name_suffix);
  }
  
  void cxios_get_file_name_suffix(file_Ptr file_hdl, char * name_suffix, int name_suffix_size)
  {
    if(!string_copy(file_hdl->name_suffix.getValue(),name_suffix , name_suffix_size))
      ERROR("void cxios_get_file_name_suffix(file_Ptr file_hdl, char * name_suffix, int name_suffix_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_output_freq(file_Ptr file_hdl, const char * output_freq, int output_freq_size)
  {
    std::string output_freq_str;
    if(!cstr2string(output_freq, output_freq_size, output_freq_str)) return;
    file_hdl->output_freq.setValue(output_freq_str);
    file_hdl->sendAttributToServer(file_hdl->output_freq);
  }
  
  void cxios_get_file_output_freq(file_Ptr file_hdl, char * output_freq, int output_freq_size)
  {
    if(!string_copy(file_hdl->output_freq.getValue(),output_freq , output_freq_size))
      ERROR("void cxios_get_file_output_freq(file_Ptr file_hdl, char * output_freq, int output_freq_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_output_level(file_Ptr file_hdl, int output_level)
  {
    file_hdl->output_level.setValue(output_level);
    file_hdl->sendAttributToServer(file_hdl->output_level);
  }
  
  void cxios_get_file_output_level(file_Ptr file_hdl, int* output_level)
  {
    *output_level = file_hdl->output_level.getValue();
  }
  
  
  void cxios_set_file_split_freq(file_Ptr file_hdl, const char * split_freq, int split_freq_size)
  {
    std::string split_freq_str;
    if(!cstr2string(split_freq, split_freq_size, split_freq_str)) return;
    file_hdl->split_freq.setValue(split_freq_str);
    file_hdl->sendAttributToServer(file_hdl->split_freq);
  }
  
  void cxios_get_file_split_freq(file_Ptr file_hdl, char * split_freq, int split_freq_size)
  {
    if(!string_copy(file_hdl->split_freq.getValue(),split_freq , split_freq_size))
      ERROR("void cxios_get_file_split_freq(file_Ptr file_hdl, char * split_freq, int split_freq_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_sync_freq(file_Ptr file_hdl, const char * sync_freq, int sync_freq_size)
  {
    std::string sync_freq_str;
    if(!cstr2string(sync_freq, sync_freq_size, sync_freq_str)) return;
    file_hdl->sync_freq.setValue(sync_freq_str);
    file_hdl->sendAttributToServer(file_hdl->sync_freq);
  }
  
  void cxios_get_file_sync_freq(file_Ptr file_hdl, char * sync_freq, int sync_freq_size)
  {
    if(!string_copy(file_hdl->sync_freq.getValue(),sync_freq , sync_freq_size))
      ERROR("void cxios_get_file_sync_freq(file_Ptr file_hdl, char * sync_freq, int sync_freq_size)", <<"Input string is to short");
  }
  
  
  void cxios_set_file_type(file_Ptr file_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if(!cstr2string(type, type_size, type_str)) return;
    file_hdl->type.setValue(type_str);
    file_hdl->sendAttributToServer(file_hdl->type);
  }
  
  void cxios_get_file_type(file_Ptr file_hdl, char * type, int type_size)
  {
    if(!string_copy(file_hdl->type.getValue(),type , type_size))
      ERROR("void cxios_get_file_type(file_Ptr file_hdl, char * type, int type_size)", <<"Input string is to short");
  }
  
  
  
}
