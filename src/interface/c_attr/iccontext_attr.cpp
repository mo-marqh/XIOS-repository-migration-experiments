/* ************************************************************************** *
 *               Interface auto generated - do not modify                     *
 * ************************************************************************** */


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
  typedef xios::CContext* context_Ptr;

  void cxios_set_context_attached_mode(context_Ptr context_hdl, bool attached_mode)
  {
    CTimer::get("XIOS").resume();
    context_hdl->attached_mode.setValue(attached_mode);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_attached_mode(context_Ptr context_hdl, bool* attached_mode)
  {
    CTimer::get("XIOS").resume();
    *attached_mode = context_hdl->attached_mode.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_attached_mode(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->attached_mode.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_gatherer(context_Ptr context_hdl, const char * default_gatherer, int default_gatherer_size)
  {
    std::string default_gatherer_str;
    if (!cstr2string(default_gatherer, default_gatherer_size, default_gatherer_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_gatherer.setValue(default_gatherer_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_gatherer(context_Ptr context_hdl, char * default_gatherer, int default_gatherer_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_gatherer.getInheritedValue(), default_gatherer, default_gatherer_size))
      ERROR("void cxios_get_context_default_gatherer(context_Ptr context_hdl, char * default_gatherer, int default_gatherer_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_gatherer(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_gatherer.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_pool(context_Ptr context_hdl, const char * default_pool, int default_pool_size)
  {
    std::string default_pool_str;
    if (!cstr2string(default_pool, default_pool_size, default_pool_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_pool.setValue(default_pool_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_pool(context_Ptr context_hdl, char * default_pool, int default_pool_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_pool.getInheritedValue(), default_pool, default_pool_size))
      ERROR("void cxios_get_context_default_pool(context_Ptr context_hdl, char * default_pool, int default_pool_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_pool(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_pool.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_pool_gatherer(context_Ptr context_hdl, const char * default_pool_gatherer, int default_pool_gatherer_size)
  {
    std::string default_pool_gatherer_str;
    if (!cstr2string(default_pool_gatherer, default_pool_gatherer_size, default_pool_gatherer_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_pool_gatherer.setValue(default_pool_gatherer_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_pool_gatherer(context_Ptr context_hdl, char * default_pool_gatherer, int default_pool_gatherer_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_pool_gatherer.getInheritedValue(), default_pool_gatherer, default_pool_gatherer_size))
      ERROR("void cxios_get_context_default_pool_gatherer(context_Ptr context_hdl, char * default_pool_gatherer, int default_pool_gatherer_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_pool_gatherer(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_pool_gatherer.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_pool_reader(context_Ptr context_hdl, const char * default_pool_reader, int default_pool_reader_size)
  {
    std::string default_pool_reader_str;
    if (!cstr2string(default_pool_reader, default_pool_reader_size, default_pool_reader_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_pool_reader.setValue(default_pool_reader_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_pool_reader(context_Ptr context_hdl, char * default_pool_reader, int default_pool_reader_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_pool_reader.getInheritedValue(), default_pool_reader, default_pool_reader_size))
      ERROR("void cxios_get_context_default_pool_reader(context_Ptr context_hdl, char * default_pool_reader, int default_pool_reader_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_pool_reader(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_pool_reader.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_pool_writer(context_Ptr context_hdl, const char * default_pool_writer, int default_pool_writer_size)
  {
    std::string default_pool_writer_str;
    if (!cstr2string(default_pool_writer, default_pool_writer_size, default_pool_writer_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_pool_writer.setValue(default_pool_writer_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_pool_writer(context_Ptr context_hdl, char * default_pool_writer, int default_pool_writer_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_pool_writer.getInheritedValue(), default_pool_writer, default_pool_writer_size))
      ERROR("void cxios_get_context_default_pool_writer(context_Ptr context_hdl, char * default_pool_writer, int default_pool_writer_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_pool_writer(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_pool_writer.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_reader(context_Ptr context_hdl, const char * default_reader, int default_reader_size)
  {
    std::string default_reader_str;
    if (!cstr2string(default_reader, default_reader_size, default_reader_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_reader.setValue(default_reader_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_reader(context_Ptr context_hdl, char * default_reader, int default_reader_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_reader.getInheritedValue(), default_reader, default_reader_size))
      ERROR("void cxios_get_context_default_reader(context_Ptr context_hdl, char * default_reader, int default_reader_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_reader(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_reader.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_using_server2(context_Ptr context_hdl, bool default_using_server2)
  {
    CTimer::get("XIOS").resume();
    context_hdl->default_using_server2.setValue(default_using_server2);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_using_server2(context_Ptr context_hdl, bool* default_using_server2)
  {
    CTimer::get("XIOS").resume();
    *default_using_server2 = context_hdl->default_using_server2.getInheritedValue();
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_using_server2(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_using_server2.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_default_writer(context_Ptr context_hdl, const char * default_writer, int default_writer_size)
  {
    std::string default_writer_str;
    if (!cstr2string(default_writer, default_writer_size, default_writer_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->default_writer.setValue(default_writer_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_default_writer(context_Ptr context_hdl, char * default_writer, int default_writer_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->default_writer.getInheritedValue(), default_writer, default_writer_size))
      ERROR("void cxios_get_context_default_writer(context_Ptr context_hdl, char * default_writer, int default_writer_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_default_writer(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->default_writer.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }


  void cxios_set_context_output_dir(context_Ptr context_hdl, const char * output_dir, int output_dir_size)
  {
    std::string output_dir_str;
    if (!cstr2string(output_dir, output_dir_size, output_dir_str)) return;
    CTimer::get("XIOS").resume();
    context_hdl->output_dir.setValue(output_dir_str);
    CTimer::get("XIOS").suspend();
  }

  void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)
  {
    CTimer::get("XIOS").resume();
    if (!string_copy(context_hdl->output_dir.getInheritedValue(), output_dir, output_dir_size))
      ERROR("void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)", << "Input string is too short");
    CTimer::get("XIOS").suspend();
  }

  bool cxios_is_defined_context_output_dir(context_Ptr context_hdl)
  {
     CTimer::get("XIOS").resume();
     bool isDefined = context_hdl->output_dir.hasInheritedValue();
     CTimer::get("XIOS").suspend();
     return isDefined;
  }
}
