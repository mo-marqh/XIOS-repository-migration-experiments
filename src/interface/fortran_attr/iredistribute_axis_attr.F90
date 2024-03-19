! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iredistribute_axis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iredistribute_axis
  USE redistribute_axis_interface_attr

CONTAINS

  SUBROUTINE xios(set_redistribute_axis_attr)  &
    ( redistribute_axis_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis))  :: redistribute_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_axis_id
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(get_redistribute_axis_handle) &
      (redistribute_axis_id,redistribute_axis_hdl)
      CALL xios(set_redistribute_axis_attr_hdl_)   &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(set_redistribute_axis_attr)

  SUBROUTINE xios(set_redistribute_axis_attr_hdl)  &
    ( redistribute_axis_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(set_redistribute_axis_attr_hdl_)  &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(set_redistribute_axis_attr_hdl)

  SUBROUTINE xios(set_redistribute_axis_attr_hdl_)   &
    ( redistribute_axis_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: index_(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(index_)) THEN
        CALL cxios_set_redistribute_axis_index &
      (redistribute_axis_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        mask__tmp = mask_
        CALL cxios_set_redistribute_axis_mask &
      (redistribute_axis_hdl%daddr, mask__tmp, SHAPE(mask_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_redistribute_axis_type &
      (redistribute_axis_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(set_redistribute_axis_attr_hdl_)

  SUBROUTINE xios(get_redistribute_axis_attr)  &
    ( redistribute_axis_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis))  :: redistribute_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_axis_id
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_redistribute_axis_handle) &
      (redistribute_axis_id,redistribute_axis_hdl)
      CALL xios(get_redistribute_axis_attr_hdl_)   &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(get_redistribute_axis_attr)

  SUBROUTINE xios(get_redistribute_axis_attr_hdl)  &
    ( redistribute_axis_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_redistribute_axis_attr_hdl_)  &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(get_redistribute_axis_attr_hdl)

  SUBROUTINE xios(get_redistribute_axis_attr_hdl_)   &
    ( redistribute_axis_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: index_(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(index_)) THEN
        CALL cxios_get_redistribute_axis_index &
      (redistribute_axis_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        CALL cxios_get_redistribute_axis_mask &
      (redistribute_axis_hdl%daddr, mask__tmp, SHAPE(mask_))
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_redistribute_axis_type &
      (redistribute_axis_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(get_redistribute_axis_attr_hdl_)

  SUBROUTINE xios(is_defined_redistribute_axis_attr)  &
    ( redistribute_axis_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis))  :: redistribute_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_axis_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(get_redistribute_axis_handle) &
      (redistribute_axis_id,redistribute_axis_hdl)
      CALL xios(is_defined_redistribute_axis_attr_hdl_)   &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(is_defined_redistribute_axis_attr)

  SUBROUTINE xios(is_defined_redistribute_axis_attr_hdl)  &
    ( redistribute_axis_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(is_defined_redistribute_axis_attr_hdl_)  &
      ( redistribute_axis_hdl, index, mask, type )

  END SUBROUTINE xios(is_defined_redistribute_axis_attr_hdl)

  SUBROUTINE xios(is_defined_redistribute_axis_attr_hdl_)   &
    ( redistribute_axis_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_axis)) , INTENT(IN) :: redistribute_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: index_
      LOGICAL(KIND=C_BOOL) :: index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(index_)) THEN
        index__tmp = cxios_is_defined_redistribute_axis_index &
      (redistribute_axis_hdl%daddr)
        index_ = index__tmp
      ENDIF

      IF (PRESENT(mask_)) THEN
        mask__tmp = cxios_is_defined_redistribute_axis_mask &
      (redistribute_axis_hdl%daddr)
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_redistribute_axis_type &
      (redistribute_axis_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_redistribute_axis_attr_hdl_)

END MODULE iredistribute_axis_attr
