

MODULE XIOS_INTERFACES

USE icontext, ONLY : xios_get_context_handle

USE icontext_attr, ONLY : xios_set_context_attr_hdl, xios_get_context_attr_hdl, xios_is_defined_context_attr_hdl

USE idata, ONLY : xios_send_field_r8_0d, xios_send_field_r8_1d, xios_send_field_r8_2d, xios_send_field_r8_3d, &
                  xios_send_field_r8_4d, xios_send_field_r8_5d, xios_send_field_r8_6d, xios_send_field_r8_7d, &
                  xios_send_field_r4_0d, xios_send_field_r4_1d, xios_send_field_r4_2d, xios_send_field_r4_3d, &
                  xios_send_field_r4_4d, xios_send_field_r4_5d, xios_send_field_r4_6d, xios_send_field_r4_7d, &
                  xios_recv_field_r8_0d, xios_recv_field_r8_1d, xios_recv_field_r8_2d, xios_recv_field_r8_3d, &
                  xios_recv_field_r8_4d, xios_recv_field_r8_5d, xios_recv_field_r8_6d, xios_recv_field_r8_7d, &
                  xios_recv_field_r4_0d, xios_recv_field_r4_1d, xios_recv_field_r4_2d, xios_recv_field_r4_3d, &
                  xios_recv_field_r4_4d, xios_recv_field_r4_5d, xios_recv_field_r4_6d, xios_recv_field_r4_7d, &
                  xios_getVar_k8, xios_getVar_k4, xios_getVar_int, xios_getVar_logic, xios_getVar_char, &
                  xios_setVar_k8, xios_setVar_k4, xios_setVar_int, xios_setVar_logic, xios_setVar_char

USE idomain, ONLY : xios_get_domain_handle, xios_get_domaingroup_handle

USE idomain_attr, ONLY : xios_set_domain_attr_hdl, xios_get_domain_attr_hdl, xios_is_defined_domain_attr_hdl

USE idomaingroup_attr, ONLY : xios_set_domaingroup_attr_hdl, xios_get_domaingroup_attr_hdl, xios_is_defined_domaingroup_attr_hdl

USE ifield, ONLY : xios_get_field_handle, xios_get_fieldgroup_handle, xios_field_is_active_id, xios_field_is_active_hdl, &
                   xios_field_get_domain_handle, xios_field_get_axis_handle, xios_field_get_scalar_handle, &
                   xios_field_id_get_domain_handle, xios_field_id_get_axis_handle, xios_field_id_get_scalar_handle

USE ifield_attr, ONLY : xios_set_field_attr_hdl, xios_get_field_attr_hdl, xios_is_defined_field_attr_hdl

USE ifieldgroup_attr, ONLY : xios_set_fieldgroup_attr_hdl, xios_get_fieldgroup_attr_hdl, xios_is_defined_fieldgroup_attr_hdl

USE ivariable, ONLY : xios_get_variable_handle, xios_get_variablegroup_handle

USE ivariable_attr, ONLY : xios_set_variable_attr_hdl, xios_get_variable_attr_hdl, xios_is_defined_variable_attr_hdl

USE ivariablegroup_attr, ONLY : xios_set_variablegroup_attr_hdl, xios_get_variablegroup_attr_hdl, &
                                xios_is_defined_variablegroup_attr_hdl

USE ifile, ONLY : xios_get_file_handle, xios_get_filegroup_handle

USE ifile_attr, ONLY : xios_set_file_attr_hdl, xios_get_file_attr_hdl, xios_is_defined_file_attr_hdl

USE ifilegroup_attr, ONLY : xios_set_filegroup_attr_hdl, xios_get_filegroup_attr_hdl, xios_is_defined_filegroup_attr_hdl

USE igrid, ONLY : xios_get_grid_handle, xios_get_gridgroup_handle

USE igrid_attr, ONLY : xios_set_grid_attr_hdl, xios_get_grid_attr_hdl, xios_is_defined_grid_attr_hdl

USE igridgroup_attr, ONLY : xios_set_gridgroup_attr_hdl, xios_get_gridgroup_attr_hdl, xios_is_defined_gridgroup_attr_hdl

USE iaxis, ONLY : xios_get_axis_handle, xios_get_axisgroup_handle

USE iaxis_attr, ONLY : xios_set_axis_attr_hdl, xios_get_axis_attr_hdl, xios_is_defined_axis_attr_hdl

USE iaxisgroup_attr, ONLY : xios_set_axisgroup_attr_hdl, xios_get_axisgroup_attr_hdl, xios_is_defined_axisgroup_attr_hdl

USE iscalar, ONLY : xios_get_scalar_handle, xios_get_scalargroup_handle

USE iscalar_attr, ONLY : xios_set_scalar_attr_hdl, xios_get_scalar_attr_hdl, xios_is_defined_scalar_attr_hdl

USE iscalargroup_attr, ONLY : xios_set_scalargroup_attr_hdl, xios_get_scalargroup_attr_hdl, xios_is_defined_scalargroup_attr_hdl

!-------------------------------------------------------------------------------
!!! Transformation INTERFACES
!-------------------------------------------------------------------------------
!!! DOMAIN TRANSFORMATIONS
USE izoom_domain, ONLY : xios_get_zoom_domain_handle

USE izoom_domain_attr, ONLY : xios_set_zoom_domain_attr_hdl, xios_get_zoom_domain_attr_hdl, xios_is_defined_zoom_domain_attr_hdl

USE iinterpolate_domain, ONLY : xios_get_interpolate_domain_handle

USE iinterpolate_domain_attr, ONLY : xios_set_interpolate_domain_attr_hdl, xios_get_interpolate_domain_attr_hdl, &
                                     xios_is_defined_interpolate_domain_attr_hdl

USE igenerate_rectilinear_domain, ONLY : xios_get_generate_rectilinear_domain_handle

USE igenerate_rectilinear_domain_attr, ONLY : xios_set_generate_rectilinear_domain_attr_hdl, &
                                              xios_get_generate_rectilinear_domain_attr_hdl, &
                                              xios_is_defined_generate_rectilinear_domain_attr_hdl

USE icompute_connectivity_domain, ONLY : xios_get_compute_connectivity_domain_handle

USE icompute_connectivity_domain_attr, ONLY : xios_set_compute_connectivity_domain_attr_hdl, &
                                              xios_get_compute_connectivity_domain_attr_hdl, &
                                              xios_is_defined_compute_connectivity_domain_attr_hdl

USE iexpand_domain, ONLY : xios_get_expand_domain_handle

USE iexpand_domain_attr, ONLY : xios_set_expand_domain_attr_hdl, &
                                xios_get_expand_domain_attr_hdl, &
                                xios_is_defined_expand_domain_attr_hdl

!!! AXIS TRANSFORMATIONS
USE izoom_axis, ONLY : xios_get_zoom_axis_handle

USE izoom_axis_attr, ONLY : xios_set_zoom_axis_attr_hdl, xios_get_zoom_axis_attr_hdl, xios_is_defined_zoom_axis_attr_hdl

USE iinterpolate_axis, ONLY : xios_get_interpolate_axis_handle

USE iinterpolate_axis_attr, ONLY : xios_set_interpolate_axis_attr_hdl, &
                                   xios_get_interpolate_axis_attr_hdl, &
                                   xios_is_defined_interpolate_axis_attr_hdl

USE iinverse_axis, ONLY : xios_get_inverse_axis_handle

USE iinverse_axis_attr, ONLY : xios_set_inverse_axis_attr_hdl, &
                               xios_get_inverse_axis_attr_hdl, &
                               xios_is_defined_inverse_axis_attr_hdl

USE ireduce_domain_to_axis, ONLY : xios_get_reduce_domain_to_axis_handle

USE ireduce_domain_to_axis_attr, ONLY : xios_set_reduce_domain_to_axis_attr_hdl, &
                                        xios_get_reduce_domain_to_axis_attr_hdl, &
                                        xios_is_defined_reduce_domain_to_axis_attr_hdl

USE iextract_domain_to_axis, ONLY : xios_get_extract_domain_to_axis_handle

USE iextract_domain_to_axis_attr, ONLY : xios_set_extract_domain_to_axis_attr_hdl, &
                                         xios_get_extract_domain_to_axis_attr_hdl, &
                                         xios_is_defined_extract_domain_to_axis_attr_hdl

!!! SCALAR TRANSFORMATIONS


USE ireduce_axis_to_scalar, ONLY : xios_get_reduce_axis_to_scalar_handle

USE ireduce_axis_to_scalar_attr, ONLY : xios_set_reduce_axis_to_scalar_attr_hdl, &
                                        xios_get_reduce_axis_to_scalar_attr_hdl, &
                                        xios_is_defined_reduce_axis_to_scalar_attr_hdl

USE ireduce_domain_to_scalar, ONLY : xios_get_reduce_domain_to_scalar_handle

USE ireduce_domain_to_scalar_attr, ONLY : xios_set_reduce_domain_to_scalar_attr_hdl, &
                                          xios_get_reduce_domain_to_scalar_attr_hdl, &
                                          xios_is_defined_reduce_domain_to_scalar_attr_hdl

USE iextract_axis_to_scalar, ONLY : xios_get_extract_axis_to_scalar_handle

USE iextract_axis_to_scalar_attr, ONLY : xios_set_extract_axis_to_scalar_attr_hdl, &
                                         xios_get_extract_axis_to_scalar_attr_hdl, &
                                         xios_is_defined_extract_axis_to_scalar_attr_hdl

USE ixml_tree, ONLY : xios_add_axis, xios_add_file, xios_add_grid, xios_add_field, xios_add_domain, &
                      xios_add_fieldtofile, xios_add_variabletofile, xios_add_variabletofield, &
                      xios_add_axisgroup, xios_add_filegroup, xios_add_gridgroup, xios_add_fieldgroup, &
                      xios_add_domaingroup, xios_add_fieldgrouptofile, xios_add_variablegrouptofile, &
                      xios_add_variablegrouptofield, xios_add_axistogrid, xios_add_domaintogrid, &
                      xios_add_zoomdomaintodomain, xios_add_interpolatedomaintodomain, &
                      xios_add_generatedomaintodomain, xios_add_zoomaxistoaxis, &
                      xios_add_interpolateaxistoaxis, xios_add_inverseaxistoaxis, xios_add_scalar, &
                      xios_add_scalargroup, xios_add_scalartogrid, xios_add_reduceaxistoscalartoscalar, &
                      xios_add_computeconnectivitydomaintodomain, xios_add_reducedomaintoaxistoaxis, &
                      xios_add_extractdomaintoaxistoaxis, xios_add_reducedomaintoscalartoscalar, &
                      xios_add_extractaxistoscalartoscalar, xios_add_expanddomaintodomain

PRIVATE

INTERFACE xios_set_attr
  MODULE PROCEDURE xios_set_domaingroup_attr_hdl, xios_set_domain_attr_hdl, xios_set_fieldgroup_attr_hdl, &
                   xios_set_field_attr_hdl,xios_set_variable_attr_hdl, xios_set_variablegroup_attr_hdl, &
                   xios_set_file_attr_hdl, xios_set_filegroup_attr_hdl, &
                   xios_set_grid_attr_hdl, xios_set_gridgroup_attr_hdl, xios_set_axis_attr_hdl , &
                   xios_set_axisgroup_attr_hdl, xios_set_context_attr_hdl, xios_set_zoom_axis_attr_hdl, &
                   xios_set_zoom_domain_attr_hdl, xios_set_interpolate_axis_attr_hdl, &
                   xios_set_interpolate_domain_attr_hdl, xios_set_inverse_axis_attr_hdl, &
                   xios_set_generate_rectilinear_domain_attr_hdl, xios_set_scalar_attr_hdl, &
                   xios_set_scalargroup_attr_hdl, xios_set_reduce_axis_to_scalar_attr_hdl, &
                   xios_set_compute_connectivity_domain_attr_hdl, xios_set_reduce_domain_to_scalar_attr_hdl, &
                   xios_set_reduce_domain_to_axis_attr_hdl, xios_set_extract_domain_to_axis_attr_hdl, &
                   xios_set_extract_axis_to_scalar_attr_hdl, xios_set_expand_domain_attr_hdl
END INTERFACE xios_set_attr


INTERFACE xios_get_attr
  MODULE PROCEDURE xios_get_domaingroup_attr_hdl, xios_get_domain_attr_hdl, xios_get_fieldgroup_attr_hdl, &
                   xios_get_field_attr_hdl, xios_get_variable_attr_hdl, xios_get_variablegroup_attr_hdl, &
                   xios_get_file_attr_hdl, xios_get_filegroup_attr_hdl, &
                   xios_get_grid_attr_hdl, xios_get_gridgroup_attr_hdl, xios_get_axis_attr_hdl , &
                   xios_get_axisgroup_attr_hdl, xios_get_context_attr_hdl, xios_get_zoom_axis_attr_hdl, &
                   xios_get_zoom_domain_attr_hdl, xios_get_interpolate_axis_attr_hdl, &
                   xios_get_interpolate_domain_attr_hdl, xios_get_inverse_axis_attr_hdl, &
                   xios_get_generate_rectilinear_domain_attr_hdl, xios_get_scalar_attr_hdl, &
                   xios_get_scalargroup_attr_hdl, xios_get_reduce_axis_to_scalar_attr_hdl, &
                   xios_get_compute_connectivity_domain_attr_hdl, xios_get_reduce_domain_to_scalar_attr_hdl, &
                   xios_get_reduce_domain_to_axis_attr_hdl, xios_get_extract_domain_to_axis_attr_hdl, &
                   xios_get_extract_axis_to_scalar_attr_hdl, xios_get_expand_domain_attr_hdl
END INTERFACE xios_get_attr

INTERFACE xios_is_defined_attr
  MODULE PROCEDURE xios_is_defined_domaingroup_attr_hdl, xios_is_defined_domain_attr_hdl, xios_is_defined_fieldgroup_attr_hdl, &
                   xios_is_defined_field_attr_hdl, xios_is_defined_variable_attr_hdl, xios_is_defined_variablegroup_attr_hdl, &
                   xios_is_defined_file_attr_hdl, xios_is_defined_filegroup_attr_hdl, &
                   xios_is_defined_grid_attr_hdl, xios_is_defined_gridgroup_attr_hdl, xios_is_defined_axis_attr_hdl , &
                   xios_is_defined_axisgroup_attr_hdl, xios_is_defined_context_attr_hdl, &
                   xios_is_defined_zoom_axis_attr_hdl, xios_is_defined_zoom_domain_attr_hdl, &
                   xios_is_defined_interpolate_axis_attr_hdl, xios_is_defined_interpolate_domain_attr_hdl, &
                   xios_is_defined_inverse_axis_attr_hdl, xios_is_defined_generate_rectilinear_domain_attr_hdl, &
                   xios_is_defined_scalar_attr_hdl, xios_is_defined_scalargroup_attr_hdl, &
                   xios_is_defined_reduce_axis_to_scalar_attr_hdl, xios_is_defined_compute_connectivity_domain_attr_hdl, &
                   xios_is_defined_reduce_domain_to_scalar_attr_hdl, xios_is_defined_reduce_domain_to_axis_attr_hdl, &
                   xios_is_defined_extract_domain_to_axis_attr_hdl, xios_is_defined_extract_axis_to_scalar_attr_hdl, &
                   xios_is_defined_expand_domain_attr_hdl
END INTERFACE xios_is_defined_attr

INTERFACE xios_get_handle
  MODULE PROCEDURE xios_get_context_handle, xios_get_domain_handle, xios_get_domaingroup_handle, &
                   xios_get_file_handle, xios_get_filegroup_handle, xios_get_grid_handle, &
                   xios_get_gridgroup_handle, xios_get_axis_handle, xios_get_axisgroup_handle, &
                   xios_get_field_handle, xios_get_fieldgroup_handle,xios_get_variable_handle, &
                   xios_get_variablegroup_handle, xios_get_zoom_axis_handle, &
                   xios_get_zoom_domain_handle, xios_get_interpolate_axis_handle, &
                   xios_get_interpolate_domain_handle, xios_get_inverse_axis_handle, &
                   xios_get_generate_rectilinear_domain_handle, xios_get_scalar_handle, &
                   xios_get_scalargroup_handle, xios_get_reduce_axis_to_scalar_handle, &
                   xios_get_compute_connectivity_domain_handle, xios_get_reduce_domain_to_scalar_handle, &
                   xios_get_reduce_domain_to_axis_handle, xios_get_extract_domain_to_axis_handle, &
                   xios_get_extract_axis_to_scalar_handle, xios_get_expand_domain_handle
END INTERFACE xios_get_handle

INTERFACE xios_add_child
  MODULE PROCEDURE xios_add_axis, xios_add_file, xios_add_grid, xios_add_field, xios_add_domain, &
                   xios_add_fieldtofile, xios_add_variabletofile, xios_add_variabletofield, xios_add_axisgroup, &
                   xios_add_filegroup, xios_add_gridgroup, xios_add_fieldgroup, xios_add_domaingroup, &
                   xios_add_fieldgrouptofile, xios_add_variablegrouptofile,xios_add_variablegrouptofield, &
                   xios_add_axistogrid, xios_add_domaintogrid, xios_add_zoomdomaintodomain, &
                   xios_add_interpolatedomaintodomain, xios_add_generatedomaintodomain, &
                   xios_add_zoomaxistoaxis, xios_add_interpolateaxistoaxis, xios_add_inverseaxistoaxis, &
                   xios_add_scalar, xios_add_scalartogrid, xios_add_reduceaxistoscalartoscalar, &
                   xios_add_computeconnectivitydomaintodomain, xios_add_reducedomaintoscalartoscalar, &
                   xios_add_reducedomaintoaxistoaxis, xios_add_extractdomaintoaxistoaxis, &
                   xios_add_extractaxistoscalartoscalar, xios_add_expanddomaintodomain
END INTERFACE xios_add_child

INTERFACE xios_send_field
  MODULE PROCEDURE xios_send_field_r8_0d, xios_send_field_r8_1d, xios_send_field_r8_2d, xios_send_field_r8_3d, &
                   xios_send_field_r8_4d, xios_send_field_r8_5d, xios_send_field_r8_6d, xios_send_field_r8_7d, &
                   xios_send_field_r4_0d, xios_send_field_r4_1d, xios_send_field_r4_2d, xios_send_field_r4_3d, &
                   xios_send_field_r4_4d, xios_send_field_r4_5d, xios_send_field_r4_6d, xios_send_field_r4_7d
END INTERFACE xios_send_field

INTERFACE xios_recv_field
  MODULE PROCEDURE xios_recv_field_r8_0d, xios_recv_field_r8_1d, xios_recv_field_r8_2d, xios_recv_field_r8_3d, &
                   xios_recv_field_r8_4d, xios_recv_field_r8_5d, xios_recv_field_r8_6d, xios_recv_field_r8_7d, &
                   xios_recv_field_r4_0d, xios_recv_field_r4_1d, xios_recv_field_r4_2d, xios_recv_field_r4_3d, &
                   xios_recv_field_r4_4d, xios_recv_field_r4_5d, xios_recv_field_r4_6d, xios_recv_field_r4_7d
END INTERFACE xios_recv_field

INTERFACE xios_field_is_active
  MODULE PROCEDURE xios_field_is_active_id,xios_field_is_active_hdl
END INTERFACE xios_field_is_active

INTERFACE xios_field_get_domain
  MODULE PROCEDURE xios_field_get_domain_handle, xios_field_id_get_domain_handle
END INTERFACE xios_field_get_domain

INTERFACE xios_field_get_axis
  MODULE PROCEDURE xios_field_get_axis_handle,xios_field_id_get_axis_handle
END INTERFACE xios_field_get_axis

INTERFACE xios_field_get_scalar
  MODULE PROCEDURE xios_field_get_scalar_handle,xios_field_id_get_scalar_handle
END INTERFACE xios_field_get_scalar

INTERFACE xios_getVar
  MODULE PROCEDURE xios_getVar_k8, xios_getVar_k4, xios_getVar_int, xios_getVar_logic, xios_getVar_char
END INTERFACE xios_getVar

INTERFACE xios_setVar
  MODULE PROCEDURE xios_setVar_k8, xios_setVar_k4, xios_setVar_int, xios_setVar_logic, xios_setVar_char
END INTERFACE xios_setVar

PUBLIC :: xios_set_attr, xios_get_attr, xios_is_defined_attr, xios_get_handle, xios_add_child, &
          xios_send_field, xios_recv_field, xios_field_is_active, xios_getVar, xios_setVar, &
          xios_field_get_domain,xios_field_get_axis,xios_field_get_scalar

END MODULE XIOS_INTERFACES
