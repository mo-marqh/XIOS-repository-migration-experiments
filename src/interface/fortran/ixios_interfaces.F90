#include "xios_fortran_prefix.hpp"

MODULE XIOS_INTERFACES

USE icontext, ONLY : xios(get_context_handle)

USE icontext_attr, ONLY : xios(set_context_attr_hdl), xios(get_context_attr_hdl), xios(is_defined_context_attr_hdl)

USE idata, ONLY : xios(send_field_r8_1d), xios(send_field_r8_2d), xios(send_field_r8_3d),                    &
                  xios(send_field_r4_1d), xios(send_field_r4_2d), xios(send_field_r4_3d),                    &
                  xios(getVar_k8), xios(getVar_k4), xios(getVar_int), xios(getVar_logic), xios(getVar_char), &
                  xios(setVar_k8), xios(setVar_k4), xios(setVar_int), xios(setVar_logic), xios(setVar_char)

USE idomain, ONLY : xios(get_domain_handle), xios(get_domaingroup_handle)

USE idomain_attr, ONLY : xios(set_domain_attr_hdl), xios(get_domain_attr_hdl), xios(is_defined_domain_attr_hdl)

USE idomaingroup_attr, ONLY : xios(set_domaingroup_attr_hdl), xios(get_domaingroup_attr_hdl), xios(is_defined_domaingroup_attr_hdl)

USE ifield, ONLY : xios(get_field_handle), xios(get_fieldgroup_handle), xios(field_is_active_id), xios(field_is_active_hdl)

USE ifield_attr, ONLY : xios(set_field_attr_hdl), xios(get_field_attr_hdl), xios(is_defined_field_attr_hdl)

USE ifieldgroup_attr, ONLY : xios(set_fieldgroup_attr_hdl), xios(get_fieldgroup_attr_hdl), xios(is_defined_fieldgroup_attr_hdl)

USE ivariable, ONLY : xios(get_variable_handle), xios(get_variablegroup_handle)

USE ivariable_attr, ONLY : xios(set_variable_attr_hdl), xios(get_variable_attr_hdl), xios(is_defined_variable_attr_hdl)

USE ivariablegroup_attr, ONLY : xios(set_variablegroup_attr_hdl), xios(get_variablegroup_attr_hdl), xios(is_defined_variablegroup_attr_hdl)

USE ifile, ONLY : xios(get_file_handle), xios(get_filegroup_handle)

USE ifile_attr, ONLY : xios(set_file_attr_hdl), xios(get_file_attr_hdl), xios(is_defined_file_attr_hdl)

USE ifilegroup_attr, ONLY : xios(set_filegroup_attr_hdl), xios(get_filegroup_attr_hdl), xios(is_defined_filegroup_attr_hdl)

USE igrid, ONLY : xios(get_grid_handle), xios(get_gridgroup_handle)

USE igrid_attr, ONLY : xios(set_grid_attr_hdl), xios(get_grid_attr_hdl), xios(is_defined_grid_attr_hdl)

USE igridgroup_attr, ONLY : xios(set_gridgroup_attr_hdl), xios(get_gridgroup_attr_hdl), xios(is_defined_gridgroup_attr_hdl)

USE iaxis, ONLY : xios(get_axis_handle), xios(get_axisgroup_handle)

USE iaxis_attr, ONLY : xios(set_axis_attr_hdl), xios(get_axis_attr_hdl), xios(is_defined_axis_attr_hdl)

USE iaxisgroup_attr, ONLY : xios(set_axisgroup_attr_hdl), xios(get_axisgroup_attr_hdl), xios(is_defined_axisgroup_attr_hdl)

USE ixml_tree, ONLY : xios(add_axis), xios(add_file), xios(add_grid), xios(add_field), xios(add_domain),   &
                      xios(add_fieldtofile), xios(add_variabletofile), xios(add_variabletofield),          &
                      xios(add_axisgroup), xios(add_filegroup), xios(add_gridgroup), xios(add_fieldgroup), &
                      xios(add_domaingroup), xios(add_fieldgrouptofile), xios(add_variablegrouptofile),    &
                      xios(add_variablegrouptofield)

PRIVATE

INTERFACE xios(set_attr)
  MODULE PROCEDURE xios(set_domaingroup_attr_hdl), xios(set_domain_attr_hdl), xios(set_fieldgroup_attr_hdl), &
                   xios(set_field_attr_hdl),xios(set_variable_attr_hdl), xios(set_variablegroup_attr_hdl),   &
                   xios(set_file_attr_hdl), xios(set_filegroup_attr_hdl),                                    &
                   xios(set_grid_attr_hdl), xios(set_gridgroup_attr_hdl), xios(set_axis_attr_hdl) ,          &
                   xios(set_axisgroup_attr_hdl), xios(set_context_attr_hdl)
END INTERFACE xios(set_attr)

INTERFACE xios(get_attr)
  MODULE PROCEDURE xios(get_domaingroup_attr_hdl), xios(get_domain_attr_hdl), xios(get_fieldgroup_attr_hdl), &
                   xios(get_field_attr_hdl), xios(get_variable_attr_hdl), xios(get_variablegroup_attr_hdl),  &
                   xios(get_file_attr_hdl), xios(get_filegroup_attr_hdl),                                    &
                   xios(get_grid_attr_hdl), xios(get_gridgroup_attr_hdl), xios(get_axis_attr_hdl) ,          &
                   xios(get_axisgroup_attr_hdl), xios(get_context_attr_hdl)
END INTERFACE xios(get_attr)

INTERFACE xios(is_defined_attr)
  MODULE PROCEDURE xios(is_defined_domaingroup_attr_hdl), xios(is_defined_domain_attr_hdl), xios(is_defined_fieldgroup_attr_hdl), &
                   xios(is_defined_field_attr_hdl), xios(is_defined_variable_attr_hdl), xios(is_defined_variablegroup_attr_hdl),  &
                   xios(is_defined_file_attr_hdl), xios(is_defined_filegroup_attr_hdl),                                           &
                   xios(is_defined_grid_attr_hdl), xios(is_defined_gridgroup_attr_hdl), xios(is_defined_axis_attr_hdl) ,          &
                   xios(is_defined_axisgroup_attr_hdl), xios(is_defined_context_attr_hdl)
END INTERFACE xios(is_defined_attr)

INTERFACE xios(get_handle)
  MODULE PROCEDURE xios(get_context_handle), xios(get_domain_handle), xios(get_domaingroup_handle), &
                   xios(get_file_handle), xios(get_filegroup_handle), xios(get_grid_handle),        &
                   xios(get_gridgroup_handle), xios(get_axis_handle), xios(get_axisgroup_handle),   &
                   xios(get_field_handle), xios(get_fieldgroup_handle),xios(get_variable_handle),   &
                   xios(get_variablegroup_handle)
END INTERFACE xios(get_handle)

INTERFACE xios(add_child)
  MODULE PROCEDURE xios(add_axis), xios(add_file), xios(add_grid), xios(add_field), xios(add_domain),                &
                   xios(add_fieldtofile), xios(add_variabletofile), xios(add_variabletofield), xios(add_axisgroup),  &
                   xios(add_filegroup), xios(add_gridgroup), xios(add_fieldgroup), xios(add_domaingroup),            &
                   xios(add_fieldgrouptofile), xios(add_variablegrouptofile),xios(add_variablegrouptofield)
END INTERFACE xios(add_child)


INTERFACE xios(send_field)
  MODULE PROCEDURE xios(send_field_r8_1d), xios(send_field_r8_2d), xios(send_field_r8_3d), &
                   xios(send_field_r4_1d), xios(send_field_r4_2d), xios(send_field_r4_3d)
END INTERFACE xios(send_field)

INTERFACE xios(field_is_active)
  MODULE PROCEDURE xios(field_is_active_id),xios(field_is_active_hdl)
END INTERFACE xios(field_is_active)

INTERFACE xios(getVar)
  MODULE PROCEDURE xios(getVar_k8), xios(getVar_k4), xios(getVar_int), xios(getVar_logic), xios(getVar_char)
END INTERFACE xios(getVar)

INTERFACE xios(setVar)
  MODULE PROCEDURE xios(setVar_k8), xios(setVar_k4), xios(setVar_int), xios(setVar_logic), xios(setVar_char)
END INTERFACE xios(setVar)

PUBLIC :: xios(set_attr), xios(get_attr), xios(is_defined_attr), xios(get_handle), &
          xios(add_child), xios(send_field), xios(field_is_active), xios(getVar), xios(setVar)

END MODULE XIOS_INTERFACES
