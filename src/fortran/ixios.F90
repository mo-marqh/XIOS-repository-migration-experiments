#include "xios_fortran_prefix.hpp"

MODULE XIOS


USE icontext, ONLY : txios(context), xios(set_context_attr), xios(set_context_attr_hdl),   &
                     xios(get_context_handle), xios(set_current_context),                  &
                     xios(is_valid_context)

USE idata, ONLY : xios(initialize),xios(init_server), xios(finalize), xios(context_initialize),  &
                  xios(close_context_definition),       &
                  xios(context_finalize), xios(send_field_r8_1d), xios(send_field_r8_2d), &
                  xios(send_field_r8_3d), xios(send_field_r4_1d), xios(send_field_r4_2d), &
                  xios(send_field_r4_3d)

USE idate, ONLY : txios(date),txios(time), xios(set_timestep),xios(update_calendar)

USE idomain, ONLY : txios(domain), txios(domaingroup), xios(set_domaingroup_attr),       &
                    xios(set_domaingroup_attr_hdl), xios(set_domain_attr), xios(set_domain_attr_hdl),&
                    xios(get_domain_handle), xios(get_domaingroup_handle),xios(is_valid_domain),     &
                    xios(is_valid_domaingroup)
                     
USE ifield, ONLY : txios(field), txios(fieldgroup), xios(set_fieldgroup_attr),           &
                   xios(set_fieldgroup_attr_hdl),xios(set_field_attr),xios(set_field_attr_hdl),      &
                   xios(get_field_handle), xios(get_fieldgroup_handle), xios(is_valid_field),        &
                   xios(is_valid_fieldgroup),xios(field_is_active_id),xios(field_is_active_hdl)  
                   
USE ifile, ONLY : txios(file), txios(filegroup), xios(set_file_attr),xios(set_file_attr_hdl), &
                  xios(set_filegroup_attr), xios(set_filegroup_attr_hdl), xios(get_file_handle),    & 
                  xios(get_filegroup_handle), xios(is_valid_file), xios(is_valid_filegroup)
                  
USE igrid, ONLY : txios(grid), txios(gridgroup), xios(set_grid_attr), xios(set_grid_attr_hdl), &
                  xios(set_gridgroup_attr), xios(set_gridgroup_attr_hdl), xios(get_grid_handle),     &
                  xios(get_gridgroup_handle), xios(is_valid_grid), xios(is_valid_gridgroup) 

USE iaxis, ONLY : txios(axis), txios(axisgroup), xios(set_axis_attr), xios(set_axis_attr_hdl), &
                  xios(set_axisgroup_attr), xios(set_axisgroup_attr_hdl), xios(get_axis_handle),     &
                  xios(get_axisgroup_handle), xios(is_valid_axis), xios(is_valid_axisgroup) 

USE ixml_tree, ONLY : xios(add_axis), xios(add_file), xios(add_grid), xios(add_field), xios(add_domain),          &
                     xios(add_fieldtofile), xios(add_axisgroup), xios(add_filegroup), xios(add_gridgroup), &
                     xios(add_fieldgroup), xios(add_domaingroup), xios(add_fieldgrouptofile)
                  

PRIVATE


INTERFACE xios(set_attr)
  MODULE PROCEDURE xios(set_domaingroup_attr_hdl), xios(set_domain_attr_hdl), xios(set_fieldgroup_attr_hdl), &
                   xios(set_field_attr_hdl), xios(set_file_attr_hdl), xios(set_filegroup_attr_hdl),          &
                   xios(set_grid_attr_hdl), xios(set_gridgroup_attr_hdl), xios(set_axis_attr_hdl) ,          &
                   xios(set_axisgroup_attr_hdl), xios(set_context_attr_hdl)
END INTERFACE xios(set_attr)

INTERFACE xios(get_handle)
  MODULE PROCEDURE  xios(get_context_handle), xios(get_domain_handle), xios(get_domaingroup_handle),        &
                    xios(get_file_handle), xios(get_filegroup_handle), xios(get_grid_handle),               &
                    xios(get_gridgroup_handle), xios(get_axis_handle), xios(get_axisgroup_handle),          &
                    xios(get_field_handle), xios(get_fieldgroup_handle)
END INTERFACE xios(get_handle) 

INTERFACE xios(add_child)
  MODULE PROCEDURE xios(add_axis), xios(add_file), xios(add_grid), xios(add_field), xios(add_domain),    &
                   xios(add_fieldtofile), xios(add_axisgroup), xios(add_filegroup), xios(add_gridgroup), &
                   xios(add_fieldgroup), xios(add_domaingroup), xios(add_fieldgrouptofile)
END INTERFACE xios(add_child)


INTERFACE xios(send_field)
  MODULE PROCEDURE  xios(send_field_r8_1d), xios(send_field_r8_2d), xios(send_field_r8_3d),              &
                    xios(send_field_r4_1d), xios(send_field_r4_2d), xios(send_field_r4_3d)
END INTERFACE xios(send_field)

INTERFACE xios(field_is_active)
  MODULE PROCEDURE xios(field_is_active_id),xios(field_is_active_hdl)
END INTERFACE
  
 PUBLIC :: txios(domain), txios(domaingroup),txios(field), txios(fieldgroup),txios(file), txios(filegroup), &
          txios(grid), txios(gridgroup), txios(axis), txios(axisgroup),txios(context), txios(date),txios(time)  

 PUBLIC :: xios(set_attr), xios(set_domain_attr), xios(set_domaingroup_attr), xios(set_fieldgroup_attr), &
          xios(set_field_attr), xios(set_file_attr), xios(set_filegroup_attr),          &
          xios(set_grid_attr), xios(set_gridgroup_attr), xios(set_axis_attr) ,          &
          xios(set_axisgroup_attr), xios(set_context_attr)

 PUBLIC :: xios(get_handle) 
 PUBLIC :: xios(add_child) 

 PUBLIC :: xios(is_valid_context),xios(is_valid_domain), xios(is_valid_domaingroup),xios(is_valid_field),        &
          xios(is_valid_fieldgroup), xios(is_valid_file), xios(is_valid_filegroup), xios(is_valid_grid),         &
          xios(is_valid_gridgroup), xios(is_valid_axis), xios(is_valid_axisgroup)
          
 PUBLIC :: xios(set_current_context)  
 PUBLIC :: xios(set_timestep),xios(update_calendar)
 PUBLIC :: xios(initialize), xios(init_server), xios(finalize), xios(context_initialize),                       &
           xios(close_context_definition), xios(context_finalize), xios(send_field),xios(field_is_active)

END MODULE XIOS
