! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iredistribute_scalar_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iredistribute_scalar
  USE redistribute_scalar_interface_attr

CONTAINS

  SUBROUTINE xios(set_redistribute_scalar_attr)  &
    ( redistribute_scalar_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar))  :: redistribute_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_scalar_id
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(get_redistribute_scalar_handle) &
      (redistribute_scalar_id,redistribute_scalar_hdl)
      CALL xios(set_redistribute_scalar_attr_hdl_)   &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(set_redistribute_scalar_attr)

  SUBROUTINE xios(set_redistribute_scalar_attr_hdl)  &
    ( redistribute_scalar_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(set_redistribute_scalar_attr_hdl_)  &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(set_redistribute_scalar_attr_hdl)

  SUBROUTINE xios(set_redistribute_scalar_attr_hdl_)   &
    ( redistribute_scalar_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: index_(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(index_)) THEN
        CALL cxios_set_redistribute_scalar_index &
      (redistribute_scalar_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        mask__tmp = mask_
        CALL cxios_set_redistribute_scalar_mask &
      (redistribute_scalar_hdl%daddr, mask__tmp, SHAPE(mask_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_redistribute_scalar_type &
      (redistribute_scalar_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(set_redistribute_scalar_attr_hdl_)

  SUBROUTINE xios(get_redistribute_scalar_attr)  &
    ( redistribute_scalar_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar))  :: redistribute_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_scalar_id
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_redistribute_scalar_handle) &
      (redistribute_scalar_id,redistribute_scalar_hdl)
      CALL xios(get_redistribute_scalar_attr_hdl_)   &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(get_redistribute_scalar_attr)

  SUBROUTINE xios(get_redistribute_scalar_attr_hdl)  &
    ( redistribute_scalar_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: index(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_redistribute_scalar_attr_hdl_)  &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(get_redistribute_scalar_attr_hdl)

  SUBROUTINE xios(get_redistribute_scalar_attr_hdl_)   &
    ( redistribute_scalar_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: index_(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(index_)) THEN
        CALL cxios_get_redistribute_scalar_index &
      (redistribute_scalar_hdl%daddr, index_, SHAPE(index_))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(SIZE(mask_,1)))
        CALL cxios_get_redistribute_scalar_mask &
      (redistribute_scalar_hdl%daddr, mask__tmp, SHAPE(mask_))
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_redistribute_scalar_type &
      (redistribute_scalar_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(get_redistribute_scalar_attr_hdl_)

  SUBROUTINE xios(is_defined_redistribute_scalar_attr)  &
    ( redistribute_scalar_id, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar))  :: redistribute_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::redistribute_scalar_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(get_redistribute_scalar_handle) &
      (redistribute_scalar_id,redistribute_scalar_hdl)
      CALL xios(is_defined_redistribute_scalar_attr_hdl_)   &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(is_defined_redistribute_scalar_attr)

  SUBROUTINE xios(is_defined_redistribute_scalar_attr_hdl)  &
    ( redistribute_scalar_hdl, index, mask, type )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: index
      LOGICAL(KIND=C_BOOL) :: index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(is_defined_redistribute_scalar_attr_hdl_)  &
      ( redistribute_scalar_hdl, index, mask, type )

  END SUBROUTINE xios(is_defined_redistribute_scalar_attr_hdl)

  SUBROUTINE xios(is_defined_redistribute_scalar_attr_hdl_)   &
    ( redistribute_scalar_hdl, index_, mask_, type_ )

    IMPLICIT NONE
      TYPE(txios(redistribute_scalar)) , INTENT(IN) :: redistribute_scalar_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: index_
      LOGICAL(KIND=C_BOOL) :: index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(index_)) THEN
        index__tmp = cxios_is_defined_redistribute_scalar_index &
      (redistribute_scalar_hdl%daddr)
        index_ = index__tmp
      ENDIF

      IF (PRESENT(mask_)) THEN
        mask__tmp = cxios_is_defined_redistribute_scalar_mask &
      (redistribute_scalar_hdl%daddr)
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_redistribute_scalar_type &
      (redistribute_scalar_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_redistribute_scalar_attr_hdl_)

END MODULE iredistribute_scalar_attr
