! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE iextract_axis_to_scalar_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iextract_axis_to_scalar
  USE extract_axis_to_scalar_interface_attr

CONTAINS

  SUBROUTINE xios_set_extract_axis_to_scalar_attr &
    ( extract_axis_to_scalar_id, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) :: extract_axis_to_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::extract_axis_to_scalar_id
      INTEGER , OPTIONAL, INTENT(IN) :: position

      CALL xios_get_extract_axis_to_scalar_handle &
      (extract_axis_to_scalar_id,extract_axis_to_scalar_hdl)
      CALL xios_set_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_set_extract_axis_to_scalar_attr

  SUBROUTINE xios_set_extract_axis_to_scalar_attr_hdl &
    ( extract_axis_to_scalar_hdl, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: position

      CALL xios_set_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_set_extract_axis_to_scalar_attr_hdl

  SUBROUTINE xios_set_extract_axis_to_scalar_attr_hdl_ &
    ( extract_axis_to_scalar_hdl, position_ )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: position_

      IF (PRESENT(position_)) THEN
        CALL cxios_set_extract_axis_to_scalar_position &
      (extract_axis_to_scalar_hdl%daddr, position_)
      ENDIF

  END SUBROUTINE xios_set_extract_axis_to_scalar_attr_hdl_

  SUBROUTINE xios_get_extract_axis_to_scalar_attr &
    ( extract_axis_to_scalar_id, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) :: extract_axis_to_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::extract_axis_to_scalar_id
      INTEGER , OPTIONAL, INTENT(OUT) :: position

      CALL xios_get_extract_axis_to_scalar_handle &
      (extract_axis_to_scalar_id,extract_axis_to_scalar_hdl)
      CALL xios_get_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_get_extract_axis_to_scalar_attr

  SUBROUTINE xios_get_extract_axis_to_scalar_attr_hdl &
    ( extract_axis_to_scalar_hdl, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: position

      CALL xios_get_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_get_extract_axis_to_scalar_attr_hdl

  SUBROUTINE xios_get_extract_axis_to_scalar_attr_hdl_ &
    ( extract_axis_to_scalar_hdl, position_ )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: position_

      IF (PRESENT(position_)) THEN
        CALL cxios_get_extract_axis_to_scalar_position &
      (extract_axis_to_scalar_hdl%daddr, position_)
      ENDIF

  END SUBROUTINE xios_get_extract_axis_to_scalar_attr_hdl_

  SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr &
    ( extract_axis_to_scalar_id, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) :: extract_axis_to_scalar_hdl
      CHARACTER(LEN=*), INTENT(IN) ::extract_axis_to_scalar_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: position
      LOGICAL(KIND=C_BOOL) :: position_tmp

      CALL xios_get_extract_axis_to_scalar_handle &
      (extract_axis_to_scalar_id,extract_axis_to_scalar_hdl)
      CALL xios_is_defined_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr

  SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr_hdl &
    ( extract_axis_to_scalar_hdl, position )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: position
      LOGICAL(KIND=C_BOOL) :: position_tmp

      CALL xios_is_defined_extract_axis_to_scalar_attr_hdl_ &
      ( extract_axis_to_scalar_hdl, position )

  END SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr_hdl

  SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr_hdl_ &
    ( extract_axis_to_scalar_hdl, position_ )

    IMPLICIT NONE
      TYPE(xios_extract_axis_to_scalar) , INTENT(IN) :: extract_axis_to_scalar_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: position_
      LOGICAL(KIND=C_BOOL) :: position__tmp

      IF (PRESENT(position_)) THEN
        position__tmp = cxios_is_defined_extract_axis_to_scalar_position &
      (extract_axis_to_scalar_hdl%daddr)
        position_ = position__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_extract_axis_to_scalar_attr_hdl_

END MODULE iextract_axis_to_scalar_attr
