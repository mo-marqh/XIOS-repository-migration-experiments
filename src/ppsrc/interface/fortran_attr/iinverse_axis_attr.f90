! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE iinverse_axis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iinverse_axis
  USE inverse_axis_interface_attr

CONTAINS

  SUBROUTINE xios_set_inverse_axis_attr &
    ( inverse_axis_id )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) :: inverse_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::inverse_axis_id

      CALL xios_get_inverse_axis_handle &
      (inverse_axis_id,inverse_axis_hdl)
      CALL xios_set_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_set_inverse_axis_attr

  SUBROUTINE xios_set_inverse_axis_attr_hdl &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

      CALL xios_set_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_set_inverse_axis_attr_hdl

  SUBROUTINE xios_set_inverse_axis_attr_hdl_ &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

  END SUBROUTINE xios_set_inverse_axis_attr_hdl_

  SUBROUTINE xios_get_inverse_axis_attr &
    ( inverse_axis_id )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) :: inverse_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::inverse_axis_id

      CALL xios_get_inverse_axis_handle &
      (inverse_axis_id,inverse_axis_hdl)
      CALL xios_get_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_get_inverse_axis_attr

  SUBROUTINE xios_get_inverse_axis_attr_hdl &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

      CALL xios_get_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_get_inverse_axis_attr_hdl

  SUBROUTINE xios_get_inverse_axis_attr_hdl_ &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

  END SUBROUTINE xios_get_inverse_axis_attr_hdl_

  SUBROUTINE xios_is_defined_inverse_axis_attr &
    ( inverse_axis_id )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) :: inverse_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::inverse_axis_id

      CALL xios_get_inverse_axis_handle &
      (inverse_axis_id,inverse_axis_hdl)
      CALL xios_is_defined_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_is_defined_inverse_axis_attr

  SUBROUTINE xios_is_defined_inverse_axis_attr_hdl &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

      CALL xios_is_defined_inverse_axis_attr_hdl_ &
      ( inverse_axis_hdl )

  END SUBROUTINE xios_is_defined_inverse_axis_attr_hdl

  SUBROUTINE xios_is_defined_inverse_axis_attr_hdl_ &
    ( inverse_axis_hdl )

    IMPLICIT NONE
      TYPE(xios_inverse_axis) , INTENT(IN) :: inverse_axis_hdl

  END SUBROUTINE xios_is_defined_inverse_axis_attr_hdl_

END MODULE iinverse_axis_attr
