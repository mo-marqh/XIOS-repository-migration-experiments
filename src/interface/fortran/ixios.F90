#include "xios_fortran_prefix.hpp"

MODULE XIOS

USE icalendar, ONLY : xios(define_calendar), xios(set_timestep), xios(set_start_date), xios(set_time_origin),   &
                      xios(get_calendar_type), xios(get_timestep), xios(get_start_date), xios(get_time_origin), &
                      xios(update_calendar), xios(get_current_date),                                            &
                      xios(get_year_length_in_seconds), xios(get_day_length_in_seconds)

USE icontext, ONLY : txios(context), xios(set_current_context), xios(is_valid_context)

USE icontext_attr, ONLY : xios(set_context_attr), xios(get_context_attr), xios(is_defined_context_attr)

USE idata, ONLY : xios(initialize), xios(init_server), xios(finalize), xios(context_initialize), xios(context_is_initialized), &
                  xios(close_context_definition), xios(context_finalize), xios(solve_inheritance)

USE idomain, ONLY : txios(domain), txios(domaingroup), xios(is_valid_domain), xios(is_valid_domaingroup)

USE idomain_attr, ONLY : xios(set_domain_attr), xios(get_domain_attr), xios(is_defined_domain_attr)

USE idomaingroup_attr, ONLY : xios(set_domaingroup_attr), xios(get_domaingroup_attr), xios(is_defined_domaingroup_attr)

USE iduration, ONLY: txios(duration),                                                                            &
                     xios(year), xios(month), xios(day), xios(hour), xios(minute), xios(second), xios(timestep), &
                     xios(duration_add), xios(duration_sub), xios(duration_mult), xios(duration_neg),            &
                     xios(duration_eq), xios(duration_neq),                                                      &
                     OPERATOR(+), OPERATOR(-), OPERATOR(*)

USE idate, ONLY : txios(date),                                                                                &
                  xios(date_convert_to_seconds),                                                              &
                  xios(date_add_duration), xios(date_sub_duration), xios(date_sub),                           &
                  xios(date_eq), xios(date_neq), xios(date_lt), xios(date_le), xios(date_gt), xios(date_ge),  &
                  xios(date_get_second_of_year), xios(date_get_day_of_year), xios(date_get_fraction_of_year), &
                  xios(date_get_second_of_day), xios(date_get_fraction_of_day),                               &
                  OPERATOR(+), OPERATOR(-),                                                                   &
                  OPERATOR(==), OPERATOR(/=), OPERATOR(<), OPERATOR(<=), OPERATOR(>), OPERATOR(>=),           &
                  ASSIGNMENT(=)

USE ifield, ONLY : txios(field), txios(fieldgroup), xios(is_valid_field), xios(is_valid_fieldgroup)

USE ifield_attr, ONLY : xios(set_field_attr), xios(get_field_attr), xios(is_defined_field_attr)

USE ifieldgroup_attr, ONLY : xios(set_fieldgroup_attr), xios(get_fieldgroup_attr), xios(is_defined_fieldgroup_attr)

USE ivariable, ONLY : txios(variable), txios(variablegroup), xios(is_valid_variable), xios(is_valid_variablegroup)

USE ivariable_attr, ONLY : xios(set_variable_attr), xios(get_variable_attr), xios(is_defined_variable_attr)

USE ivariablegroup_attr, ONLY : xios(set_variablegroup_attr), xios(get_variablegroup_attr), xios(is_defined_variablegroup_attr)

USE ifile, ONLY : txios(file), txios(filegroup), xios(is_valid_file), xios(is_valid_filegroup)

USE ifile_attr, ONLY : xios(set_file_attr), xios(get_file_attr), xios(is_defined_file_attr)

USE ifilegroup_attr, ONLY : xios(set_filegroup_attr), xios(get_filegroup_attr), xios(is_defined_filegroup_attr)

USE igrid, ONLY : txios(grid), txios(gridgroup), xios(is_valid_grid), xios(is_valid_gridgroup)

USE igrid_attr, ONLY : xios(set_grid_attr), xios(get_grid_attr), xios(is_defined_grid_attr)

USE igridgroup_attr, ONLY : xios(set_gridgroup_attr), xios(get_gridgroup_attr), xios(is_defined_gridgroup_attr)

USE iaxis, ONLY : txios(axis), txios(axisgroup), xios(is_valid_axis), xios(is_valid_axisgroup)

USE iaxis_attr, ONLY : xios(set_axis_attr), xios(get_axis_attr), xios(is_defined_axis_attr)

USE iaxisgroup_attr, ONLY : xios(set_axisgroup_attr), xios(get_axisgroup_attr), xios(is_defined_axisgroup_attr)

USE izoom_axis, ONLY : txios(zoom_axis), xios(is_valid_zoom_axis)

USE izoom_axis_attr, ONLY : xios(set_zoom_axis_attr), xios(get_zoom_axis_attr), xios(is_defined_zoom_axis_attr)

USE izoom_domain, ONLY : txios(zoom_domain), xios(is_valid_zoom_domain)

USE izoom_domain_attr, ONLY : xios(set_zoom_domain_attr), xios(get_zoom_domain_attr), xios(is_defined_zoom_domain_attr)

USE iinterpolate_axis, ONLY : txios(interpolate_axis), xios(is_valid_interpolate_axis)

USE iinterpolate_axis_attr, ONLY : xios(set_interpolate_axis_attr), xios(get_interpolate_axis_attr), &
                                   xios(is_defined_interpolate_axis_attr)

USE iinterpolate_domain, ONLY : txios(interpolate_domain), xios(is_valid_interpolate_domain)

USE iinterpolate_domain_attr, ONLY : xios(set_interpolate_domain_attr), xios(get_interpolate_domain_attr), &
                                     xios(is_defined_interpolate_domain_attr)

USE iinverse_axis, ONLY : txios(inverse_axis), xios(is_valid_inverse_axis)

USE iinverse_axis_attr, ONLY : xios(set_inverse_axis_attr), xios(get_inverse_axis_attr), xios(is_defined_inverse_axis_attr)

USE igenerate_rectilinear_domain, ONLY : txios(generate_rectilinear_domain), xios(is_valid_generate_rectilinear_domain)

USE igenerate_rectilinear_domain_attr, ONLY : xios(set_generate_rectilinear_domain_attr), xios(get_generate_rectilinear_domain_attr), &
                                              xios(is_defined_generate_rectilinear_domain_attr)

USE XIOS_INTERFACES, ONLY : xios(set_attr), xios(get_attr), xios(is_defined_attr), xios(get_handle), xios(add_child), &
                            xios(send_field), xios(send_scalar), xios(recv_field), xios(field_is_active), xios(getVar), xios(setVar)

END MODULE XIOS
