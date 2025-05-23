! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iaxis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iaxis
  USE axis_interface_attr
  USE LOGICAL_BOOL_CONVERSION

CONTAINS

  SUBROUTINE xios(set_axis_attr)  &
    ( axis_id, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_type
      INTEGER  , OPTIONAL, INTENT(IN) :: begin
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: bounds_name
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: chunking_weight
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: comment
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: convert_from_factor
      INTEGER  , OPTIONAL, INTENT(IN) :: data_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: dim_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_bounds
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term_bounds
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(IN) :: label(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: n
      INTEGER  , OPTIONAL, INTENT(IN) :: n_distributed_partition
      INTEGER  , OPTIONAL, INTENT(IN) :: n_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)

      CALL xios(get_axis_handle) &
      (axis_id,axis_hdl)
      CALL xios(set_axis_attr_hdl_)   &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(set_axis_attr)

  SUBROUTINE xios(set_axis_attr_hdl)  &
    ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_type
      INTEGER  , OPTIONAL, INTENT(IN) :: begin
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: bounds_name
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: chunking_weight
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: comment
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: convert_from_factor
      INTEGER  , OPTIONAL, INTENT(IN) :: data_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: dim_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_bounds
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term_bounds
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(IN) :: label(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: n
      INTEGER  , OPTIONAL, INTENT(IN) :: n_distributed_partition
      INTEGER  , OPTIONAL, INTENT(IN) :: n_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive
      INTEGER  , OPTIONAL, INTENT(IN) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)

      CALL xios(set_axis_attr_hdl_)  &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(set_axis_attr_hdl)

  SUBROUTINE xios(set_axis_attr_hdl_)   &
    ( axis_hdl, axis_ref_, axis_type_, begin_, bounds_, bounds_name_, chunking_weight_, comment_  &
    , convert_from_factor_, data_begin_, data_index_, data_n_, dim_name_, formula_, formula_bounds_  &
    , formula_term_, formula_term_bounds_, index_, label_, long_name_, mask_, n_, n_distributed_partition_  &
    , n_glo_, name_, positive_, prec_, standard_name_, unit_, value_ )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_type_
      INTEGER  , OPTIONAL, INTENT(IN) :: begin_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: bounds_name_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: chunking_weight_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: comment_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: convert_from_factor_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_begin_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: dim_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_bounds_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: formula_term_bounds_
      INTEGER  , OPTIONAL, INTENT(IN) :: index_(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(IN) :: label_(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: n_
      INTEGER  , OPTIONAL, INTENT(IN) :: n_distributed_partition_
      INTEGER  , OPTIONAL, INTENT(IN) :: n_glo_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive_
      INTEGER  , OPTIONAL, INTENT(IN) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value_(:)

      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_axis_axis_ref &
      (axis_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF

      IF (PRESENT(axis_type_)) THEN
        CALL cxios_set_axis_axis_type &
      (axis_hdl%daddr, axis_type_, len(axis_type_))
      ENDIF

      IF (PRESENT(begin_)) THEN
        CALL cxios_set_axis_begin &
      (axis_hdl%daddr, begin_)
      ENDIF

      IF (PRESENT(bounds_)) THEN
        CALL cxios_set_axis_bounds &
      (axis_hdl%daddr, bounds_, SHAPE(bounds_))
      ENDIF

      IF (PRESENT(bounds_name_)) THEN
        CALL cxios_set_axis_bounds_name &
      (axis_hdl%daddr, bounds_name_, len(bounds_name_))
      ENDIF

      IF (PRESENT(chunking_weight_)) THEN
        CALL cxios_set_axis_chunking_weight &
      (axis_hdl%daddr, chunking_weight_)
      ENDIF

      IF (PRESENT(comment_)) THEN
        CALL cxios_set_axis_comment &
      (axis_hdl%daddr, comment_, len(comment_))
      ENDIF

      IF (PRESENT(convert_from_factor_)) THEN
        CALL cxios_set_axis_convert_from_factor &
      (axis_hdl%daddr, convert_from_factor_)
      ENDIF

      IF (PRESENT(data_begin_)) THEN
        CALL cxios_set_axis_data_begin &
      (axis_hdl%daddr, data_begin_)
      ENDIF

      IF (PRESENT(data_index_)) THEN
        CALL cxios_set_axis_data_index &
      (axis_hdl%daddr, data_index_, SHAPE(data_index_))
      ENDIF

      IF (PRESENT(data_n_)) THEN
        CALL cxios_set_axis_data_n &
      (axis_hdl%daddr, data_n_)
      ENDIF

      IF (PRESENT(dim_name_)) THEN
        CALL cxios_set_axis_dim_name &
      (axis_hdl%daddr, dim_name_, len(dim_name_))
      ENDIF

      IF (PRESENT(formula_)) THEN
        CALL cxios_set_axis_formula &
      (axis_hdl%daddr, formula_, len(formula_))
      ENDIF

      IF (PRESENT(formula_bounds_)) THEN
        CALL cxios_set_axis_formula_bounds &
      (axis_hdl%daddr, formula_bounds_, len(formula_bounds_))
      ENDIF

      IF (PRESENT(formula_term_)) THEN
        CALL cxios_set_axis_formula_term &
      (axis_hdl%daddr, formula_term_, len(formula_term_))
      ENDIF

      IF (PRESENT(formula_term_bounds_)) THEN
        CALL cxios_set_axis_formula_term_bounds &
      (axis_hdl%daddr, formula_term_bounds_, len(formula_term_bounds_))
      ENDIF

      IF (PRESENT(index_)) THEN
        CALL cxios_set_axis_index &
      (axis_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(label_)) THEN
        CALL cxios_set_axis_label &
      (axis_hdl%daddr, label_, LEN(label_), LEN_TRIM(label_), SHAPE(label_))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_axis_long_name &
      (axis_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        mask__tmp = mask_
        CALL xios_logical_to_bool_1d(mask__tmp, SHAPE(mask__tmp))
        CALL cxios_set_axis_mask &
      (axis_hdl%daddr, mask__tmp, SHAPE(mask_))
      ENDIF

      IF (PRESENT(n_)) THEN
        CALL cxios_set_axis_n &
      (axis_hdl%daddr, n_)
      ENDIF

      IF (PRESENT(n_distributed_partition_)) THEN
        CALL cxios_set_axis_n_distributed_partition &
      (axis_hdl%daddr, n_distributed_partition_)
      ENDIF

      IF (PRESENT(n_glo_)) THEN
        CALL cxios_set_axis_n_glo &
      (axis_hdl%daddr, n_glo_)
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_set_axis_name &
      (axis_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(positive_)) THEN
        CALL cxios_set_axis_positive &
      (axis_hdl%daddr, positive_, len(positive_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_set_axis_prec &
      (axis_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_axis_standard_name &
      (axis_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_set_axis_unit &
      (axis_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(value_)) THEN
        CALL cxios_set_axis_value &
      (axis_hdl%daddr, value_, SHAPE(value_))
      ENDIF

  END SUBROUTINE xios(set_axis_attr_hdl_)

  SUBROUTINE xios(get_axis_attr)  &
    ( axis_id, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_type
      INTEGER  , OPTIONAL, INTENT(OUT) :: begin
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: bounds_name
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: chunking_weight
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: comment
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: convert_from_factor
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: dim_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_bounds
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term_bounds
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(OUT) :: label(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: n
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_distributed_partition
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)

      CALL xios(get_axis_handle) &
      (axis_id,axis_hdl)
      CALL xios(get_axis_attr_hdl_)   &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(get_axis_attr)

  SUBROUTINE xios(get_axis_attr_hdl)  &
    ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_type
      INTEGER  , OPTIONAL, INTENT(OUT) :: begin
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: bounds_name
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: chunking_weight
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: comment
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: convert_from_factor
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: dim_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_bounds
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term_bounds
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(OUT) :: label(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: n
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_distributed_partition
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)

      CALL xios(get_axis_attr_hdl_)  &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(get_axis_attr_hdl)

  SUBROUTINE xios(get_axis_attr_hdl_)   &
    ( axis_hdl, axis_ref_, axis_type_, begin_, bounds_, bounds_name_, chunking_weight_, comment_  &
    , convert_from_factor_, data_begin_, data_index_, data_n_, dim_name_, formula_, formula_bounds_  &
    , formula_term_, formula_term_bounds_, index_, label_, long_name_, mask_, n_, n_distributed_partition_  &
    , n_glo_, name_, positive_, prec_, standard_name_, unit_, value_ )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_type_
      INTEGER  , OPTIONAL, INTENT(OUT) :: begin_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: bounds_name_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: chunking_weight_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: comment_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: convert_from_factor_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_begin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: dim_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_bounds_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: formula_term_bounds_
      INTEGER  , OPTIONAL, INTENT(OUT) :: index_(:)
      CHARACTER(len=*) , OPTIONAL, INTENT(OUT) :: label_(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_distributed_partition_
      INTEGER  , OPTIONAL, INTENT(OUT) :: n_glo_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive_
      INTEGER  , OPTIONAL, INTENT(OUT) :: prec_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value_(:)

      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_axis_axis_ref &
      (axis_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF

      IF (PRESENT(axis_type_)) THEN
        CALL cxios_get_axis_axis_type &
      (axis_hdl%daddr, axis_type_, len(axis_type_))
      ENDIF

      IF (PRESENT(begin_)) THEN
        CALL cxios_get_axis_begin &
      (axis_hdl%daddr, begin_)
      ENDIF

      IF (PRESENT(bounds_)) THEN
        CALL cxios_get_axis_bounds &
      (axis_hdl%daddr, bounds_, SHAPE(bounds_))
      ENDIF

      IF (PRESENT(bounds_name_)) THEN
        CALL cxios_get_axis_bounds_name &
      (axis_hdl%daddr, bounds_name_, len(bounds_name_))
      ENDIF

      IF (PRESENT(chunking_weight_)) THEN
        CALL cxios_get_axis_chunking_weight &
      (axis_hdl%daddr, chunking_weight_)
      ENDIF

      IF (PRESENT(comment_)) THEN
        CALL cxios_get_axis_comment &
      (axis_hdl%daddr, comment_, len(comment_))
      ENDIF

      IF (PRESENT(convert_from_factor_)) THEN
        CALL cxios_get_axis_convert_from_factor &
      (axis_hdl%daddr, convert_from_factor_)
      ENDIF

      IF (PRESENT(data_begin_)) THEN
        CALL cxios_get_axis_data_begin &
      (axis_hdl%daddr, data_begin_)
      ENDIF

      IF (PRESENT(data_index_)) THEN
        CALL cxios_get_axis_data_index &
      (axis_hdl%daddr, data_index_, SHAPE(data_index_))
      ENDIF

      IF (PRESENT(data_n_)) THEN
        CALL cxios_get_axis_data_n &
      (axis_hdl%daddr, data_n_)
      ENDIF

      IF (PRESENT(dim_name_)) THEN
        CALL cxios_get_axis_dim_name &
      (axis_hdl%daddr, dim_name_, len(dim_name_))
      ENDIF

      IF (PRESENT(formula_)) THEN
        CALL cxios_get_axis_formula &
      (axis_hdl%daddr, formula_, len(formula_))
      ENDIF

      IF (PRESENT(formula_bounds_)) THEN
        CALL cxios_get_axis_formula_bounds &
      (axis_hdl%daddr, formula_bounds_, len(formula_bounds_))
      ENDIF

      IF (PRESENT(formula_term_)) THEN
        CALL cxios_get_axis_formula_term &
      (axis_hdl%daddr, formula_term_, len(formula_term_))
      ENDIF

      IF (PRESENT(formula_term_bounds_)) THEN
        CALL cxios_get_axis_formula_term_bounds &
      (axis_hdl%daddr, formula_term_bounds_, len(formula_term_bounds_))
      ENDIF

      IF (PRESENT(index_)) THEN
        CALL cxios_get_axis_index &
      (axis_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(label_)) THEN
        CALL cxios_get_axis_label &
      (axis_hdl%daddr, label_, LEN(label_), SHAPE(label_))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_axis_long_name &
      (axis_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        CALL cxios_get_axis_mask &
      (axis_hdl%daddr, mask__tmp, SHAPE(mask_))
        CALL xios_bool_to_logical_1d(mask__tmp, SHAPE(mask__tmp))
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(n_)) THEN
        CALL cxios_get_axis_n &
      (axis_hdl%daddr, n_)
      ENDIF

      IF (PRESENT(n_distributed_partition_)) THEN
        CALL cxios_get_axis_n_distributed_partition &
      (axis_hdl%daddr, n_distributed_partition_)
      ENDIF

      IF (PRESENT(n_glo_)) THEN
        CALL cxios_get_axis_n_glo &
      (axis_hdl%daddr, n_glo_)
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_get_axis_name &
      (axis_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(positive_)) THEN
        CALL cxios_get_axis_positive &
      (axis_hdl%daddr, positive_, len(positive_))
      ENDIF

      IF (PRESENT(prec_)) THEN
        CALL cxios_get_axis_prec &
      (axis_hdl%daddr, prec_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_axis_standard_name &
      (axis_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(unit_)) THEN
        CALL cxios_get_axis_unit &
      (axis_hdl%daddr, unit_, len(unit_))
      ENDIF

      IF (PRESENT(value_)) THEN
        CALL cxios_get_axis_value &
      (axis_hdl%daddr, value_, SHAPE(value_))
      ENDIF

  END SUBROUTINE xios(get_axis_attr_hdl_)

  SUBROUTINE xios(is_defined_axis_attr)  &
    ( axis_id, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_type
      LOGICAL(KIND=C_BOOL) :: axis_type_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: begin
      LOGICAL(KIND=C_BOOL) :: begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds
      LOGICAL(KIND=C_BOOL) :: bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_name
      LOGICAL(KIND=C_BOOL) :: bounds_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: chunking_weight
      LOGICAL(KIND=C_BOOL) :: chunking_weight_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: comment
      LOGICAL(KIND=C_BOOL) :: comment_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: convert_from_factor
      LOGICAL(KIND=C_BOOL) :: convert_from_factor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_begin
      LOGICAL(KIND=C_BOOL) :: data_begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_index
      LOGICAL(KIND=C_BOOL) :: data_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n
      LOGICAL(KIND=C_BOOL) :: data_n_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: dim_name
      LOGICAL(KIND=C_BOOL) :: dim_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula
      LOGICAL(KIND=C_BOOL) :: formula_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_bounds
      LOGICAL(KIND=C_BOOL) :: formula_bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term
      LOGICAL(KIND=C_BOOL) :: formula_term_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term_bounds
      LOGICAL(KIND=C_BOOL) :: formula_term_bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: label
      LOGICAL(KIND=C_BOOL) :: label_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n
      LOGICAL(KIND=C_BOOL) :: n_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_distributed_partition
      LOGICAL(KIND=C_BOOL) :: n_distributed_partition_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_glo
      LOGICAL(KIND=C_BOOL) :: n_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: positive
      LOGICAL(KIND=C_BOOL) :: positive_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value
      LOGICAL(KIND=C_BOOL) :: value_tmp

      CALL xios(get_axis_handle) &
      (axis_id,axis_hdl)
      CALL xios(is_defined_axis_attr_hdl_)   &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(is_defined_axis_attr)

  SUBROUTINE xios(is_defined_axis_attr_hdl)  &
    ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
    , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
    , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
    , unit, value )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref
      LOGICAL(KIND=C_BOOL) :: axis_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_type
      LOGICAL(KIND=C_BOOL) :: axis_type_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: begin
      LOGICAL(KIND=C_BOOL) :: begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds
      LOGICAL(KIND=C_BOOL) :: bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_name
      LOGICAL(KIND=C_BOOL) :: bounds_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: chunking_weight
      LOGICAL(KIND=C_BOOL) :: chunking_weight_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: comment
      LOGICAL(KIND=C_BOOL) :: comment_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: convert_from_factor
      LOGICAL(KIND=C_BOOL) :: convert_from_factor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_begin
      LOGICAL(KIND=C_BOOL) :: data_begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_index
      LOGICAL(KIND=C_BOOL) :: data_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n
      LOGICAL(KIND=C_BOOL) :: data_n_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: dim_name
      LOGICAL(KIND=C_BOOL) :: dim_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula
      LOGICAL(KIND=C_BOOL) :: formula_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_bounds
      LOGICAL(KIND=C_BOOL) :: formula_bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term
      LOGICAL(KIND=C_BOOL) :: formula_term_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term_bounds
      LOGICAL(KIND=C_BOOL) :: formula_term_bounds_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: label
      LOGICAL(KIND=C_BOOL) :: label_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n
      LOGICAL(KIND=C_BOOL) :: n_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_distributed_partition
      LOGICAL(KIND=C_BOOL) :: n_distributed_partition_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_glo
      LOGICAL(KIND=C_BOOL) :: n_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: positive
      LOGICAL(KIND=C_BOOL) :: positive_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec
      LOGICAL(KIND=C_BOOL) :: prec_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit
      LOGICAL(KIND=C_BOOL) :: unit_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value
      LOGICAL(KIND=C_BOOL) :: value_tmp

      CALL xios(is_defined_axis_attr_hdl_)  &
      ( axis_hdl, axis_ref, axis_type, begin, bounds, bounds_name, chunking_weight, comment, convert_from_factor  &
      , data_begin, data_index, data_n, dim_name, formula, formula_bounds, formula_term, formula_term_bounds  &
      , index, label, long_name, mask, n, n_distributed_partition, n_glo, name, positive, prec, standard_name  &
      , unit, value )

  END SUBROUTINE xios(is_defined_axis_attr_hdl)

  SUBROUTINE xios(is_defined_axis_attr_hdl_)   &
    ( axis_hdl, axis_ref_, axis_type_, begin_, bounds_, bounds_name_, chunking_weight_, comment_  &
    , convert_from_factor_, data_begin_, data_index_, data_n_, dim_name_, formula_, formula_bounds_  &
    , formula_term_, formula_term_bounds_, index_, label_, long_name_, mask_, n_, n_distributed_partition_  &
    , n_glo_, name_, positive_, prec_, standard_name_, unit_, value_ )

    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_ref_
      LOGICAL(KIND=C_BOOL) :: axis_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_type_
      LOGICAL(KIND=C_BOOL) :: axis_type__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: begin_
      LOGICAL(KIND=C_BOOL) :: begin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_
      LOGICAL(KIND=C_BOOL) :: bounds__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_name_
      LOGICAL(KIND=C_BOOL) :: bounds_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: chunking_weight_
      LOGICAL(KIND=C_BOOL) :: chunking_weight__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: comment_
      LOGICAL(KIND=C_BOOL) :: comment__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: convert_from_factor_
      LOGICAL(KIND=C_BOOL) :: convert_from_factor__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_begin_
      LOGICAL(KIND=C_BOOL) :: data_begin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_index_
      LOGICAL(KIND=C_BOOL) :: data_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n_
      LOGICAL(KIND=C_BOOL) :: data_n__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: dim_name_
      LOGICAL(KIND=C_BOOL) :: dim_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_
      LOGICAL(KIND=C_BOOL) :: formula__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_bounds_
      LOGICAL(KIND=C_BOOL) :: formula_bounds__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term_
      LOGICAL(KIND=C_BOOL) :: formula_term__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: formula_term_bounds_
      LOGICAL(KIND=C_BOOL) :: formula_term_bounds__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: index_
      LOGICAL(KIND=C_BOOL) :: index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: label_
      LOGICAL(KIND=C_BOOL) :: label__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL(KIND=C_BOOL) :: long_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_
      LOGICAL(KIND=C_BOOL) :: n__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_distributed_partition_
      LOGICAL(KIND=C_BOOL) :: n_distributed_partition__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_glo_
      LOGICAL(KIND=C_BOOL) :: n_glo__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: positive_
      LOGICAL(KIND=C_BOOL) :: positive__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: prec_
      LOGICAL(KIND=C_BOOL) :: prec__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL(KIND=C_BOOL) :: standard_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: unit_
      LOGICAL(KIND=C_BOOL) :: unit__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: value_
      LOGICAL(KIND=C_BOOL) :: value__tmp

      IF (PRESENT(axis_ref_)) THEN
        axis_ref__tmp = cxios_is_defined_axis_axis_ref &
      (axis_hdl%daddr)
        axis_ref_ = axis_ref__tmp
      ENDIF

      IF (PRESENT(axis_type_)) THEN
        axis_type__tmp = cxios_is_defined_axis_axis_type &
      (axis_hdl%daddr)
        axis_type_ = axis_type__tmp
      ENDIF

      IF (PRESENT(begin_)) THEN
        begin__tmp = cxios_is_defined_axis_begin &
      (axis_hdl%daddr)
        begin_ = begin__tmp
      ENDIF

      IF (PRESENT(bounds_)) THEN
        bounds__tmp = cxios_is_defined_axis_bounds &
      (axis_hdl%daddr)
        bounds_ = bounds__tmp
      ENDIF

      IF (PRESENT(bounds_name_)) THEN
        bounds_name__tmp = cxios_is_defined_axis_bounds_name &
      (axis_hdl%daddr)
        bounds_name_ = bounds_name__tmp
      ENDIF

      IF (PRESENT(chunking_weight_)) THEN
        chunking_weight__tmp = cxios_is_defined_axis_chunking_weight &
      (axis_hdl%daddr)
        chunking_weight_ = chunking_weight__tmp
      ENDIF

      IF (PRESENT(comment_)) THEN
        comment__tmp = cxios_is_defined_axis_comment &
      (axis_hdl%daddr)
        comment_ = comment__tmp
      ENDIF

      IF (PRESENT(convert_from_factor_)) THEN
        convert_from_factor__tmp = cxios_is_defined_axis_convert_from_factor &
      (axis_hdl%daddr)
        convert_from_factor_ = convert_from_factor__tmp
      ENDIF

      IF (PRESENT(data_begin_)) THEN
        data_begin__tmp = cxios_is_defined_axis_data_begin &
      (axis_hdl%daddr)
        data_begin_ = data_begin__tmp
      ENDIF

      IF (PRESENT(data_index_)) THEN
        data_index__tmp = cxios_is_defined_axis_data_index &
      (axis_hdl%daddr)
        data_index_ = data_index__tmp
      ENDIF

      IF (PRESENT(data_n_)) THEN
        data_n__tmp = cxios_is_defined_axis_data_n &
      (axis_hdl%daddr)
        data_n_ = data_n__tmp
      ENDIF

      IF (PRESENT(dim_name_)) THEN
        dim_name__tmp = cxios_is_defined_axis_dim_name &
      (axis_hdl%daddr)
        dim_name_ = dim_name__tmp
      ENDIF

      IF (PRESENT(formula_)) THEN
        formula__tmp = cxios_is_defined_axis_formula &
      (axis_hdl%daddr)
        formula_ = formula__tmp
      ENDIF

      IF (PRESENT(formula_bounds_)) THEN
        formula_bounds__tmp = cxios_is_defined_axis_formula_bounds &
      (axis_hdl%daddr)
        formula_bounds_ = formula_bounds__tmp
      ENDIF

      IF (PRESENT(formula_term_)) THEN
        formula_term__tmp = cxios_is_defined_axis_formula_term &
      (axis_hdl%daddr)
        formula_term_ = formula_term__tmp
      ENDIF

      IF (PRESENT(formula_term_bounds_)) THEN
        formula_term_bounds__tmp = cxios_is_defined_axis_formula_term_bounds &
      (axis_hdl%daddr)
        formula_term_bounds_ = formula_term_bounds__tmp
      ENDIF

      IF (PRESENT(index_)) THEN
        index__tmp = cxios_is_defined_axis_index &
      (axis_hdl%daddr)
        index_ = index__tmp
      ENDIF

      IF (PRESENT(label_)) THEN
        label__tmp = cxios_is_defined_axis_label &
      (axis_hdl%daddr)
        label_ = label__tmp
      ENDIF

      IF (PRESENT(long_name_)) THEN
        long_name__tmp = cxios_is_defined_axis_long_name &
      (axis_hdl%daddr)
        long_name_ = long_name__tmp
      ENDIF

      IF (PRESENT(mask_)) THEN
        mask__tmp = cxios_is_defined_axis_mask &
      (axis_hdl%daddr)
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(n_)) THEN
        n__tmp = cxios_is_defined_axis_n &
      (axis_hdl%daddr)
        n_ = n__tmp
      ENDIF

      IF (PRESENT(n_distributed_partition_)) THEN
        n_distributed_partition__tmp = cxios_is_defined_axis_n_distributed_partition &
      (axis_hdl%daddr)
        n_distributed_partition_ = n_distributed_partition__tmp
      ENDIF

      IF (PRESENT(n_glo_)) THEN
        n_glo__tmp = cxios_is_defined_axis_n_glo &
      (axis_hdl%daddr)
        n_glo_ = n_glo__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_axis_name &
      (axis_hdl%daddr)
        name_ = name__tmp
      ENDIF

      IF (PRESENT(positive_)) THEN
        positive__tmp = cxios_is_defined_axis_positive &
      (axis_hdl%daddr)
        positive_ = positive__tmp
      ENDIF

      IF (PRESENT(prec_)) THEN
        prec__tmp = cxios_is_defined_axis_prec &
      (axis_hdl%daddr)
        prec_ = prec__tmp
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        standard_name__tmp = cxios_is_defined_axis_standard_name &
      (axis_hdl%daddr)
        standard_name_ = standard_name__tmp
      ENDIF

      IF (PRESENT(unit_)) THEN
        unit__tmp = cxios_is_defined_axis_unit &
      (axis_hdl%daddr)
        unit_ = unit__tmp
      ENDIF

      IF (PRESENT(value_)) THEN
        value__tmp = cxios_is_defined_axis_value &
      (axis_hdl%daddr)
        value_ = value__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_axis_attr_hdl_)

END MODULE iaxis_attr
