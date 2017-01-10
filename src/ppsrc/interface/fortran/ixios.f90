

MODULE XIOS

USE icalendar, ONLY : xios_define_calendar, xios_set_timestep, xios_set_start_date, xios_set_time_origin, &
                      xios_get_calendar_type, xios_get_timestep, xios_get_start_date, xios_get_time_origin, &
                      xios_update_calendar, xios_get_current_date, &
                      xios_get_year_length_in_seconds, xios_get_day_length_in_seconds

USE icontext, ONLY : xios_context, xios_set_current_context, xios_is_valid_context

USE icontext_attr, ONLY : xios_set_context_attr, xios_get_context_attr, xios_is_defined_context_attr

USE idata, ONLY : xios_initialize, xios_init_server, xios_finalize, xios_context_initialize, xios_context_is_initialized, &
                  xios_close_context_definition, xios_context_finalize, xios_solve_inheritance

USE idomain, ONLY : xios_domain, xios_domaingroup, xios_is_valid_domain, xios_is_valid_domaingroup

USE idomain_attr, ONLY : xios_set_domain_attr, xios_get_domain_attr, xios_is_defined_domain_attr

USE idomaingroup_attr, ONLY : xios_set_domaingroup_attr, xios_get_domaingroup_attr, xios_is_defined_domaingroup_attr

USE iduration, ONLY: xios_duration, &
                     xios_year, xios_month, xios_day, xios_hour, xios_minute, xios_second, xios_timestep, &
                     xios_duration_convert_to_string, xios_duration_convert_from_string, &
                     xios_duration_add, xios_duration_sub, xios_duration_mult, xios_duration_neg, &
                     xios_duration_eq, xios_duration_neq, &
                     OPERATOR(+), OPERATOR(-), OPERATOR(*)

USE idate, ONLY : xios_date, &
                  xios_date_convert_to_seconds, xios_date_convert_to_string, xios_date_convert_from_string, &
                  xios_date_add_duration, xios_date_sub_duration, xios_date_sub, &
                  xios_date_eq, xios_date_neq, xios_date_lt, xios_date_le, xios_date_gt, xios_date_ge, &
                  xios_date_get_second_of_year, xios_date_get_day_of_year, xios_date_get_fraction_of_year, &
                  xios_date_get_second_of_day, xios_date_get_fraction_of_day, &
                  OPERATOR(+), OPERATOR(-), &
                  OPERATOR(==), OPERATOR(/=), OPERATOR(<), OPERATOR(<=), OPERATOR(>), OPERATOR(>=), &
                  ASSIGNMENT(=)

USE ifield, ONLY : xios_field, xios_fieldgroup, xios_is_valid_field, xios_is_valid_fieldgroup

USE ifield_attr, ONLY : xios_set_field_attr, xios_get_field_attr, xios_is_defined_field_attr

USE ifieldgroup_attr, ONLY : xios_set_fieldgroup_attr, xios_get_fieldgroup_attr, xios_is_defined_fieldgroup_attr

USE ivariable, ONLY : xios_variable, xios_variablegroup, xios_is_valid_variable, xios_is_valid_variablegroup

USE ivariable_attr, ONLY : xios_set_variable_attr, xios_get_variable_attr, xios_is_defined_variable_attr

USE ivariablegroup_attr, ONLY : xios_set_variablegroup_attr, xios_get_variablegroup_attr, xios_is_defined_variablegroup_attr

USE ifile, ONLY : xios_file, xios_filegroup, xios_is_valid_file, xios_is_valid_filegroup

USE ifile_attr, ONLY : xios_set_file_attr, xios_get_file_attr, xios_is_defined_file_attr

USE ifilegroup_attr, ONLY : xios_set_filegroup_attr, xios_get_filegroup_attr, xios_is_defined_filegroup_attr

USE igrid, ONLY : xios_grid, xios_gridgroup, xios_is_valid_grid, xios_is_valid_gridgroup

USE igrid_attr, ONLY : xios_set_grid_attr, xios_get_grid_attr, xios_is_defined_grid_attr

USE igridgroup_attr, ONLY : xios_set_gridgroup_attr, xios_get_gridgroup_attr, xios_is_defined_gridgroup_attr

USE iaxis, ONLY : xios_axis, xios_axisgroup, xios_is_valid_axis, xios_is_valid_axisgroup

USE iaxis_attr, ONLY : xios_set_axis_attr, xios_get_axis_attr, xios_is_defined_axis_attr

USE iaxisgroup_attr, ONLY : xios_set_axisgroup_attr, xios_get_axisgroup_attr, xios_is_defined_axisgroup_attr

USE iscalar, ONLY : xios_scalar, xios_scalargroup, xios_is_valid_scalar, xios_is_valid_scalargroup

USE iscalar_attr, ONLY : xios_set_scalar_attr, xios_get_scalar_attr, xios_is_defined_scalar_attr

USE iscalargroup_attr, ONLY : xios_set_scalargroup_attr, xios_get_scalargroup_attr, xios_is_defined_scalargroup_attr

!-------------------------------------------------------------------------------
!!! Transformation INTERFACES
!-------------------------------------------------------------------------------
!!! DOMAIN TRANSFORMATIONS
USE izoom_domain, ONLY : xios_zoom_domain, xios_is_valid_zoom_domain

USE izoom_domain_attr, ONLY : xios_set_zoom_domain_attr, xios_get_zoom_domain_attr, xios_is_defined_zoom_domain_attr

USE iinterpolate_domain, ONLY : xios_interpolate_domain, xios_is_valid_interpolate_domain

USE iinterpolate_domain_attr, ONLY : xios_set_interpolate_domain_attr, xios_get_interpolate_domain_attr, &
                                     xios_is_defined_interpolate_domain_attr

USE igenerate_rectilinear_domain, ONLY : xios_generate_rectilinear_domain, xios_is_valid_generate_rectilinear_domain

USE igenerate_rectilinear_domain_attr, ONLY : xios_set_generate_rectilinear_domain_attr, &
                                              xios_get_generate_rectilinear_domain_attr, &
                                              xios_is_defined_generate_rectilinear_domain_attr

USE icompute_connectivity_domain, ONLY : xios_compute_connectivity_domain, xios_is_valid_compute_connectivity_domain

USE icompute_connectivity_domain_attr, ONLY : xios_set_compute_connectivity_domain_attr, &
                                              xios_get_compute_connectivity_domain_attr, &
                                              xios_is_defined_compute_connectivity_domain_attr

USE iexpand_domain, ONLY : xios_expand_domain, xios_is_valid_expand_domain

USE iexpand_domain_attr, ONLY : xios_set_expand_domain_attr, &
                                xios_get_expand_domain_attr, &
                                xios_is_defined_expand_domain_attr

!!! AXIS TRANSFORMATIONS
USE izoom_axis, ONLY : xios_zoom_axis, xios_is_valid_zoom_axis

USE izoom_axis_attr, ONLY : xios_set_zoom_axis_attr, xios_get_zoom_axis_attr, xios_is_defined_zoom_axis_attr


USE iinterpolate_axis, ONLY : xios_interpolate_axis, xios_is_valid_interpolate_axis

USE iinterpolate_axis_attr, ONLY : xios_set_interpolate_axis_attr, xios_get_interpolate_axis_attr, &
                                   xios_is_defined_interpolate_axis_attr

USE iinverse_axis, ONLY : xios_inverse_axis, xios_is_valid_inverse_axis

USE iinverse_axis_attr, ONLY : xios_set_inverse_axis_attr, xios_get_inverse_axis_attr, xios_is_defined_inverse_axis_attr

USE ireduce_domain_to_axis, ONLY : xios_reduce_domain_to_axis, xios_is_valid_reduce_domain_to_axis

USE ireduce_domain_to_axis_attr, ONLY : xios_set_reduce_domain_to_axis_attr, &
                                        xios_get_reduce_domain_to_axis_attr, &
                                        xios_is_defined_reduce_domain_to_axis_attr

USE iextract_domain_to_axis, ONLY : xios_extract_domain_to_axis, xios_is_valid_extract_domain_to_axis

USE iextract_domain_to_axis_attr, ONLY : xios_set_extract_domain_to_axis_attr, &
                                         xios_get_extract_domain_to_axis_attr, &
                                         xios_is_defined_extract_domain_to_axis_attr

!!! SCALAR TRANSFORMATIONS
USE ireduce_domain_to_scalar, ONLY : xios_reduce_domain_to_scalar, xios_is_valid_reduce_domain_to_scalar

USE ireduce_domain_to_scalar_attr, ONLY : xios_set_reduce_domain_to_scalar_attr, xios_get_reduce_domain_to_scalar_attr, &
                                          xios_is_defined_reduce_domain_to_scalar_attr

USE ireduce_axis_to_scalar, ONLY : xios_reduce_axis_to_scalar, xios_is_valid_reduce_axis_to_scalar

USE ireduce_axis_to_scalar_attr, ONLY : xios_set_reduce_axis_to_scalar_attr, xios_get_reduce_axis_to_scalar_attr, &
                                        xios_is_defined_reduce_axis_to_scalar_attr

USE iextract_axis_to_scalar, ONLY : xios_extract_axis_to_scalar, xios_is_valid_extract_axis_to_scalar

USE iextract_axis_to_scalar_attr, ONLY : xios_set_extract_axis_to_scalar_attr, &
                                         xios_get_extract_axis_to_scalar_attr, &
                                         xios_is_defined_extract_axis_to_scalar_attr

USE XIOS_INTERFACES, ONLY : xios_set_attr, xios_get_attr, xios_is_defined_attr, xios_get_handle, xios_add_child, &
                            xios_send_field, xios_recv_field, xios_field_is_active, xios_getVar, xios_setVar, &
                            xios_field_get_domain,xios_field_get_axis,xios_field_get_scalar

END MODULE XIOS
