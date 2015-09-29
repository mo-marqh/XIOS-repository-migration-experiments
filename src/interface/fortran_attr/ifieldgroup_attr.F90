! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE ifieldgroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ifield
  USE fieldgroup_interface_attr

CONTAINS

  SUBROUTINE xios(set_fieldgroup_attr)  &
    ( fieldgroup_id, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup))  :: fieldgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::fieldgroup_id
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: add_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: compression_level
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value
      LOGICAL  , OPTIONAL, INTENT(IN) :: detect_missing_value
      LOGICAL (KIND=C_BOOL) :: detect_missing_value_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_offset
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: indexed_output
      LOGICAL (KIND=C_BOOL) :: indexed_output_tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      LOGICAL  , OPTIONAL, INTENT(IN) :: read_access
      LOGICAL (KIND=C_BOOL) :: read_access_tmp
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: scale_factor
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      LOGICAL  , OPTIONAL, INTENT(IN) :: ts_enabled
      LOGICAL (KIND=C_BOOL) :: ts_enabled_tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: ts_split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_max
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_min

      CALL xios(get_fieldgroup_handle)(fieldgroup_id,fieldgroup_hdl)
      CALL xios(set_fieldgroup_attr_hdl_)   &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(set_fieldgroup_attr)

  SUBROUTINE xios(set_fieldgroup_attr_hdl)  &
    ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: add_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: compression_level
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value
      LOGICAL  , OPTIONAL, INTENT(IN) :: detect_missing_value
      LOGICAL (KIND=C_BOOL) :: detect_missing_value_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_offset
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: indexed_output
      LOGICAL (KIND=C_BOOL) :: indexed_output_tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      LOGICAL  , OPTIONAL, INTENT(IN) :: read_access
      LOGICAL (KIND=C_BOOL) :: read_access_tmp
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: scale_factor
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      LOGICAL  , OPTIONAL, INTENT(IN) :: ts_enabled
      LOGICAL (KIND=C_BOOL) :: ts_enabled_tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: ts_split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_max
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_min

      CALL xios(set_fieldgroup_attr_hdl_)  &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(set_fieldgroup_attr_hdl)

  SUBROUTINE xios(set_fieldgroup_attr_hdl_)   &
    ( fieldgroup_hdl, add_offset_, axis_ref_, compression_level_, default_value_, detect_missing_value_  &
    , domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_, grid_ref_, group_ref_, indexed_output_  &
    , level_, long_name_, name_, operation_, prec_, read_access_, scale_factor_, standard_name_  &
    , ts_enabled_, ts_split_freq_, unit_, valid_max_, valid_min_ )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: add_offset_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      INTEGER  , OPTIONAL, INTENT(IN) :: compression_level_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: default_value_
      LOGICAL  , OPTIONAL, INTENT(IN) :: detect_missing_value_
      LOGICAL (KIND=C_BOOL) :: detect_missing_value__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: field_ref_
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_offset_
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: freq_op_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: grid_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: indexed_output_
      LOGICAL (KIND=C_BOOL) :: indexed_output__tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: level_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation_
      INTEGER  , OPTIONAL, INTENT(IN) :: prec_
      LOGICAL  , OPTIONAL, INTENT(IN) :: read_access_
      LOGICAL (KIND=C_BOOL) :: read_access__tmp
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: scale_factor_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      LOGICAL  , OPTIONAL, INTENT(IN) :: ts_enabled_
      LOGICAL (KIND=C_BOOL) :: ts_enabled__tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: ts_split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_max_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: valid_min_

      IF (PRESENT(add_offset_)) THEN
        CALL cxios_set_fieldgroup_add_offset(fieldgroup_hdl%daddr, add_offset_)
      ENDIF

      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_fieldgroup_axis_ref(fieldgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF

      IF (PRESENT(compression_level_)) THEN
        CALL cxios_set_fieldgroup_compression_level(fieldgroup_hdl%daddr, compression_level_)
      ENDIF

      IF (PRESENT(default_value_)) THEN
        CALL cxios_set_fieldgroup_default_value(fieldgroup_hdl%daddr, default_value_)
      ENDIF

      IF (PRESENT(detect_missing_value_)) THEN
        detect_missing_value__tmp = detect_missing_value_
        CALL cxios_set_fieldgroup_detect_missing_value(fieldgroup_hdl%daddr, detect_missing_value__tmp)
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_fieldgroup_domain_ref(fieldgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF

      IF (PRESENT(enabled_)) THEN
        enabled__tmp = enabled_
        CALL cxios_set_fieldgroup_enabled(fieldgroup_hdl%daddr, enabled__tmp)
      ENDIF

      IF (PRESENT(field_ref_)) THEN
        CALL cxios_set_fieldgroup_field_ref(fieldgroup_hdl%daddr, field_ref_, len(field_ref_))
      ENDIF

      IF (PRESENT(freq_offset_)) THEN
        CALL cxios_set_fieldgroup_freq_offset(fieldgroup_hdl%daddr, freq_offset_)
      ENDIF

      IF (PRESENT(freq_op_)) THEN
        CALL cxios_set_fieldgroup_freq_op(fieldgroup_hdl%daddr, freq_op_)
      ENDIF

      IF (PRESENT(grid_ref_)) THEN
        CALL cxios_set_fieldgroup_grid_ref(fieldgroup_hdl%daddr, grid_ref_, len(grid_ref_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_fieldgroup_group_ref(fieldgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(indexed_output_)) THEN
        indexed_output__tmp = indexed_output_
        CALL cxios_set_fieldgroup_indexed_output(fieldgroup_hdl%daddr, indexed_output__tmp)
      ENDIF

      IF (PRESENT(level_)) THEN
        CALL cxios_set_fieldgroup_level(fieldgroup_hdl%daddr, level_)
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_fieldgroup_long_name(fieldgroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_set_fieldgroup_name(fieldgroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(operation_)) THEN
        CALL cxios_set_fieldgroup_operation(fieldgroup_hdl%daddr, operation_, len(operation_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_set_fieldgroup_prec(fieldgroup_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(read_access_)) THEN
        read_access__tmp = read_access_
        CALL cxios_set_fieldgroup_read_access(fieldgroup_hdl%daddr, read_access__tmp)
      ENDIF

      IF (PRESENT(scale_factor_)) THEN
        CALL cxios_set_fieldgroup_scale_factor(fieldgroup_hdl%daddr, scale_factor_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_fieldgroup_standard_name(fieldgroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(ts_enabled_)) THEN
        ts_enabled__tmp = ts_enabled_
        CALL cxios_set_fieldgroup_ts_enabled(fieldgroup_hdl%daddr, ts_enabled__tmp)
      ENDIF

      IF (PRESENT(ts_split_freq_)) THEN
        CALL cxios_set_fieldgroup_ts_split_freq(fieldgroup_hdl%daddr, ts_split_freq_)
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_set_fieldgroup_unit(fieldgroup_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(valid_max_)) THEN
        CALL cxios_set_fieldgroup_valid_max(fieldgroup_hdl%daddr, valid_max_)
      ENDIF

      IF (PRESENT(valid_min_)) THEN
        CALL cxios_set_fieldgroup_valid_min(fieldgroup_hdl%daddr, valid_min_)
      ENDIF

  END SUBROUTINE xios(set_fieldgroup_attr_hdl_)

  SUBROUTINE xios(get_fieldgroup_attr)  &
    ( fieldgroup_id, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup))  :: fieldgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::fieldgroup_id
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: add_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: compression_level
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL  , OPTIONAL, INTENT(OUT) :: detect_missing_value
      LOGICAL (KIND=C_BOOL) :: detect_missing_value_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_offset
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: indexed_output
      LOGICAL (KIND=C_BOOL) :: indexed_output_tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      LOGICAL  , OPTIONAL, INTENT(OUT) :: read_access
      LOGICAL (KIND=C_BOOL) :: read_access_tmp
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: scale_factor
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL  , OPTIONAL, INTENT(OUT) :: ts_enabled
      LOGICAL (KIND=C_BOOL) :: ts_enabled_tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: ts_split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_max
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_min

      CALL xios(get_fieldgroup_handle)(fieldgroup_id,fieldgroup_hdl)
      CALL xios(get_fieldgroup_attr_hdl_)   &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(get_fieldgroup_attr)

  SUBROUTINE xios(get_fieldgroup_attr_hdl)  &
    ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: add_offset
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: compression_level
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL  , OPTIONAL, INTENT(OUT) :: detect_missing_value
      LOGICAL (KIND=C_BOOL) :: detect_missing_value_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_offset
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_op
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: indexed_output
      LOGICAL (KIND=C_BOOL) :: indexed_output_tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      LOGICAL  , OPTIONAL, INTENT(OUT) :: read_access
      LOGICAL (KIND=C_BOOL) :: read_access_tmp
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: scale_factor
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL  , OPTIONAL, INTENT(OUT) :: ts_enabled
      LOGICAL (KIND=C_BOOL) :: ts_enabled_tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: ts_split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_max
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_min

      CALL xios(get_fieldgroup_attr_hdl_)  &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(get_fieldgroup_attr_hdl)

  SUBROUTINE xios(get_fieldgroup_attr_hdl_)   &
    ( fieldgroup_hdl, add_offset_, axis_ref_, compression_level_, default_value_, detect_missing_value_  &
    , domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_, grid_ref_, group_ref_, indexed_output_  &
    , level_, long_name_, name_, operation_, prec_, read_access_, scale_factor_, standard_name_  &
    , ts_enabled_, ts_split_freq_, unit_, valid_max_, valid_min_ )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: add_offset_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      INTEGER  , OPTIONAL, INTENT(OUT) :: compression_level_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: default_value_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: detect_missing_value_
      LOGICAL (KIND=C_BOOL) :: detect_missing_value__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: field_ref_
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_offset_
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: freq_op_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: grid_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: indexed_output_
      LOGICAL (KIND=C_BOOL) :: indexed_output__tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: level_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation_
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: read_access_
      LOGICAL (KIND=C_BOOL) :: read_access__tmp
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: scale_factor_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: ts_enabled_
      LOGICAL (KIND=C_BOOL) :: ts_enabled__tmp
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: ts_split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_max_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: valid_min_

      IF (PRESENT(add_offset_)) THEN
        CALL cxios_get_fieldgroup_add_offset(fieldgroup_hdl%daddr, add_offset_)
      ENDIF

      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_fieldgroup_axis_ref(fieldgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF

      IF (PRESENT(compression_level_)) THEN
        CALL cxios_get_fieldgroup_compression_level(fieldgroup_hdl%daddr, compression_level_)
      ENDIF

      IF (PRESENT(default_value_)) THEN
        CALL cxios_get_fieldgroup_default_value(fieldgroup_hdl%daddr, default_value_)
      ENDIF

      IF (PRESENT(detect_missing_value_)) THEN
        CALL cxios_get_fieldgroup_detect_missing_value(fieldgroup_hdl%daddr, detect_missing_value__tmp)
        detect_missing_value_ = detect_missing_value__tmp
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_fieldgroup_domain_ref(fieldgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF

      IF (PRESENT(enabled_)) THEN
        CALL cxios_get_fieldgroup_enabled(fieldgroup_hdl%daddr, enabled__tmp)
        enabled_ = enabled__tmp
      ENDIF

      IF (PRESENT(field_ref_)) THEN
        CALL cxios_get_fieldgroup_field_ref(fieldgroup_hdl%daddr, field_ref_, len(field_ref_))
      ENDIF

      IF (PRESENT(freq_offset_)) THEN
        CALL cxios_get_fieldgroup_freq_offset(fieldgroup_hdl%daddr, freq_offset_)
      ENDIF

      IF (PRESENT(freq_op_)) THEN
        CALL cxios_get_fieldgroup_freq_op(fieldgroup_hdl%daddr, freq_op_)
      ENDIF

      IF (PRESENT(grid_ref_)) THEN
        CALL cxios_get_fieldgroup_grid_ref(fieldgroup_hdl%daddr, grid_ref_, len(grid_ref_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_fieldgroup_group_ref(fieldgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(indexed_output_)) THEN
        CALL cxios_get_fieldgroup_indexed_output(fieldgroup_hdl%daddr, indexed_output__tmp)
        indexed_output_ = indexed_output__tmp
      ENDIF

      IF (PRESENT(level_)) THEN
        CALL cxios_get_fieldgroup_level(fieldgroup_hdl%daddr, level_)
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_fieldgroup_long_name(fieldgroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_get_fieldgroup_name(fieldgroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(operation_)) THEN
        CALL cxios_get_fieldgroup_operation(fieldgroup_hdl%daddr, operation_, len(operation_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_get_fieldgroup_prec(fieldgroup_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(read_access_)) THEN
        CALL cxios_get_fieldgroup_read_access(fieldgroup_hdl%daddr, read_access__tmp)
        read_access_ = read_access__tmp
      ENDIF

      IF (PRESENT(scale_factor_)) THEN
        CALL cxios_get_fieldgroup_scale_factor(fieldgroup_hdl%daddr, scale_factor_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_fieldgroup_standard_name(fieldgroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(ts_enabled_)) THEN
        CALL cxios_get_fieldgroup_ts_enabled(fieldgroup_hdl%daddr, ts_enabled__tmp)
        ts_enabled_ = ts_enabled__tmp
      ENDIF

      IF (PRESENT(ts_split_freq_)) THEN
        CALL cxios_get_fieldgroup_ts_split_freq(fieldgroup_hdl%daddr, ts_split_freq_)
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_get_fieldgroup_unit(fieldgroup_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(valid_max_)) THEN
        CALL cxios_get_fieldgroup_valid_max(fieldgroup_hdl%daddr, valid_max_)
      ENDIF

      IF (PRESENT(valid_min_)) THEN
        CALL cxios_get_fieldgroup_valid_min(fieldgroup_hdl%daddr, valid_min_)
      ENDIF

  END SUBROUTINE xios(get_fieldgroup_attr_hdl_)

  SUBROUTINE xios(is_defined_fieldgroup_attr)  &
    ( fieldgroup_id, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup))  :: fieldgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::fieldgroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: add_offset
      LOGICAL(KIND=C_BOOL) :: add_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: compression_level
      LOGICAL(KIND=C_BOOL) :: compression_level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL(KIND=C_BOOL) :: default_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: detect_missing_value
      LOGICAL(KIND=C_BOOL) :: detect_missing_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref
      LOGICAL(KIND=C_BOOL) :: field_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset
      LOGICAL(KIND=C_BOOL) :: freq_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op
      LOGICAL(KIND=C_BOOL) :: freq_op_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref
      LOGICAL(KIND=C_BOOL) :: grid_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: indexed_output
      LOGICAL(KIND=C_BOOL) :: indexed_output_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level
      LOGICAL(KIND=C_BOOL) :: level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: read_access
      LOGICAL(KIND=C_BOOL) :: read_access_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scale_factor
      LOGICAL(KIND=C_BOOL) :: scale_factor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_enabled
      LOGICAL(KIND=C_BOOL) :: ts_enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_split_freq
      LOGICAL(KIND=C_BOOL) :: ts_split_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_max
      LOGICAL(KIND=C_BOOL) :: valid_max_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_min
      LOGICAL(KIND=C_BOOL) :: valid_min_tmp

      CALL xios(get_fieldgroup_handle)(fieldgroup_id,fieldgroup_hdl)
      CALL xios(is_defined_fieldgroup_attr_hdl_)   &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(is_defined_fieldgroup_attr)

  SUBROUTINE xios(is_defined_fieldgroup_attr_hdl)  &
    ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
    , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
    , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
    , ts_split_freq, unit, valid_max, valid_min )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: add_offset
      LOGICAL(KIND=C_BOOL) :: add_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: compression_level
      LOGICAL(KIND=C_BOOL) :: compression_level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value
      LOGICAL(KIND=C_BOOL) :: default_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: detect_missing_value
      LOGICAL(KIND=C_BOOL) :: detect_missing_value_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL(KIND=C_BOOL) :: enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref
      LOGICAL(KIND=C_BOOL) :: field_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset
      LOGICAL(KIND=C_BOOL) :: freq_offset_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op
      LOGICAL(KIND=C_BOOL) :: freq_op_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref
      LOGICAL(KIND=C_BOOL) :: grid_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: indexed_output
      LOGICAL(KIND=C_BOOL) :: indexed_output_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level
      LOGICAL(KIND=C_BOOL) :: level_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: read_access
      LOGICAL(KIND=C_BOOL) :: read_access_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scale_factor
      LOGICAL(KIND=C_BOOL) :: scale_factor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_enabled
      LOGICAL(KIND=C_BOOL) :: ts_enabled_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_split_freq
      LOGICAL(KIND=C_BOOL) :: ts_split_freq_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_max
      LOGICAL(KIND=C_BOOL) :: valid_max_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_min
      LOGICAL(KIND=C_BOOL) :: valid_min_tmp

      CALL xios(is_defined_fieldgroup_attr_hdl_)  &
      ( fieldgroup_hdl, add_offset, axis_ref, compression_level, default_value, detect_missing_value  &
      , domain_ref, enabled, field_ref, freq_offset, freq_op, grid_ref, group_ref, indexed_output  &
      , level, long_name, name, operation, prec, read_access, scale_factor, standard_name, ts_enabled  &
      , ts_split_freq, unit, valid_max, valid_min )

  END SUBROUTINE xios(is_defined_fieldgroup_attr_hdl)

  SUBROUTINE xios(is_defined_fieldgroup_attr_hdl_)   &
    ( fieldgroup_hdl, add_offset_, axis_ref_, compression_level_, default_value_, detect_missing_value_  &
    , domain_ref_, enabled_, field_ref_, freq_offset_, freq_op_, grid_ref_, group_ref_, indexed_output_  &
    , level_, long_name_, name_, operation_, prec_, read_access_, scale_factor_, standard_name_  &
    , ts_enabled_, ts_split_freq_, unit_, valid_max_, valid_min_ )

    IMPLICIT NONE
      TYPE(txios(fieldgroup)) , INTENT(IN) :: fieldgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: add_offset_
      LOGICAL(KIND=C_BOOL) :: add_offset__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref_
      LOGICAL(KIND=C_BOOL) :: axis_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: compression_level_
      LOGICAL(KIND=C_BOOL) :: compression_level__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_value_
      LOGICAL(KIND=C_BOOL) :: default_value__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: detect_missing_value_
      LOGICAL(KIND=C_BOOL) :: detect_missing_value__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL(KIND=C_BOOL) :: domain_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL(KIND=C_BOOL) :: enabled__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: field_ref_
      LOGICAL(KIND=C_BOOL) :: field_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_offset_
      LOGICAL(KIND=C_BOOL) :: freq_offset__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: freq_op_
      LOGICAL(KIND=C_BOOL) :: freq_op__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: grid_ref_
      LOGICAL(KIND=C_BOOL) :: grid_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: indexed_output_
      LOGICAL(KIND=C_BOOL) :: indexed_output__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: level_
      LOGICAL(KIND=C_BOOL) :: level__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL(KIND=C_BOOL) :: long_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation_
      LOGICAL(KIND=C_BOOL) :: operation__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec_
      LOGICAL(KIND=C_BOOL) :: prec__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: read_access_
      LOGICAL(KIND=C_BOOL) :: read_access__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: scale_factor_
      LOGICAL(KIND=C_BOOL) :: scale_factor__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL(KIND=C_BOOL) :: standard_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_enabled_
      LOGICAL(KIND=C_BOOL) :: ts_enabled__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ts_split_freq_
      LOGICAL(KIND=C_BOOL) :: ts_split_freq__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit_
      LOGICAL(KIND=C_BOOL) :: unit__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_max_
      LOGICAL(KIND=C_BOOL) :: valid_max__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: valid_min_
      LOGICAL(KIND=C_BOOL) :: valid_min__tmp

      IF (PRESENT(add_offset_)) THEN
        add_offset__tmp = cxios_is_defined_fieldgroup_add_offset(fieldgroup_hdl%daddr)
        add_offset_ = add_offset__tmp
      ENDIF

      IF (PRESENT(axis_ref_)) THEN
        axis_ref__tmp = cxios_is_defined_fieldgroup_axis_ref(fieldgroup_hdl%daddr)
        axis_ref_ = axis_ref__tmp
      ENDIF

      IF (PRESENT(compression_level_)) THEN
        compression_level__tmp = cxios_is_defined_fieldgroup_compression_level(fieldgroup_hdl%daddr)
        compression_level_ = compression_level__tmp
      ENDIF

      IF (PRESENT(default_value_)) THEN
        default_value__tmp = cxios_is_defined_fieldgroup_default_value(fieldgroup_hdl%daddr)
        default_value_ = default_value__tmp
      ENDIF

      IF (PRESENT(detect_missing_value_)) THEN
        detect_missing_value__tmp = cxios_is_defined_fieldgroup_detect_missing_value(fieldgroup_hdl%daddr)
        detect_missing_value_ = detect_missing_value__tmp
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        domain_ref__tmp = cxios_is_defined_fieldgroup_domain_ref(fieldgroup_hdl%daddr)
        domain_ref_ = domain_ref__tmp
      ENDIF

      IF (PRESENT(enabled_)) THEN
        enabled__tmp = cxios_is_defined_fieldgroup_enabled(fieldgroup_hdl%daddr)
        enabled_ = enabled__tmp
      ENDIF

      IF (PRESENT(field_ref_)) THEN
        field_ref__tmp = cxios_is_defined_fieldgroup_field_ref(fieldgroup_hdl%daddr)
        field_ref_ = field_ref__tmp
      ENDIF

      IF (PRESENT(freq_offset_)) THEN
        freq_offset__tmp = cxios_is_defined_fieldgroup_freq_offset(fieldgroup_hdl%daddr)
        freq_offset_ = freq_offset__tmp
      ENDIF

      IF (PRESENT(freq_op_)) THEN
        freq_op__tmp = cxios_is_defined_fieldgroup_freq_op(fieldgroup_hdl%daddr)
        freq_op_ = freq_op__tmp
      ENDIF

      IF (PRESENT(grid_ref_)) THEN
        grid_ref__tmp = cxios_is_defined_fieldgroup_grid_ref(fieldgroup_hdl%daddr)
        grid_ref_ = grid_ref__tmp
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp = cxios_is_defined_fieldgroup_group_ref(fieldgroup_hdl%daddr)
        group_ref_ = group_ref__tmp
      ENDIF

      IF (PRESENT(indexed_output_)) THEN
        indexed_output__tmp = cxios_is_defined_fieldgroup_indexed_output(fieldgroup_hdl%daddr)
        indexed_output_ = indexed_output__tmp
      ENDIF

      IF (PRESENT(level_)) THEN
        level__tmp = cxios_is_defined_fieldgroup_level(fieldgroup_hdl%daddr)
        level_ = level__tmp
      ENDIF

      IF (PRESENT(long_name_)) THEN
        long_name__tmp = cxios_is_defined_fieldgroup_long_name(fieldgroup_hdl%daddr)
        long_name_ = long_name__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_fieldgroup_name(fieldgroup_hdl%daddr)
        name_ = name__tmp
      ENDIF

      IF (PRESENT(operation_)) THEN
        operation__tmp = cxios_is_defined_fieldgroup_operation(fieldgroup_hdl%daddr)
        operation_ = operation__tmp
      ENDIF

      IF (PRESENT(prec_)) THEN
        prec__tmp = cxios_is_defined_fieldgroup_prec(fieldgroup_hdl%daddr)
        prec_ = prec__tmp
      ENDIF

      IF (PRESENT(read_access_)) THEN
        read_access__tmp = cxios_is_defined_fieldgroup_read_access(fieldgroup_hdl%daddr)
        read_access_ = read_access__tmp
      ENDIF

      IF (PRESENT(scale_factor_)) THEN
        scale_factor__tmp = cxios_is_defined_fieldgroup_scale_factor(fieldgroup_hdl%daddr)
        scale_factor_ = scale_factor__tmp
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        standard_name__tmp = cxios_is_defined_fieldgroup_standard_name(fieldgroup_hdl%daddr)
        standard_name_ = standard_name__tmp
      ENDIF

      IF (PRESENT(ts_enabled_)) THEN
        ts_enabled__tmp = cxios_is_defined_fieldgroup_ts_enabled(fieldgroup_hdl%daddr)
        ts_enabled_ = ts_enabled__tmp
      ENDIF

      IF (PRESENT(ts_split_freq_)) THEN
        ts_split_freq__tmp = cxios_is_defined_fieldgroup_ts_split_freq(fieldgroup_hdl%daddr)
        ts_split_freq_ = ts_split_freq__tmp
      ENDIF

      IF (PRESENT(unit_)) THEN
        unit__tmp = cxios_is_defined_fieldgroup_unit(fieldgroup_hdl%daddr)
        unit_ = unit__tmp
      ENDIF

      IF (PRESENT(valid_max_)) THEN
        valid_max__tmp = cxios_is_defined_fieldgroup_valid_max(fieldgroup_hdl%daddr)
        valid_max_ = valid_max__tmp
      ENDIF

      IF (PRESENT(valid_min_)) THEN
        valid_min__tmp = cxios_is_defined_fieldgroup_valid_min(fieldgroup_hdl%daddr)
        valid_min_ = valid_min__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_fieldgroup_attr_hdl_)

END MODULE ifieldgroup_attr
