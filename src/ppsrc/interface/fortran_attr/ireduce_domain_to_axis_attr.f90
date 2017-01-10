! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE ireduce_domain_to_axis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ireduce_domain_to_axis
  USE reduce_domain_to_axis_interface_attr

CONTAINS

  SUBROUTINE xios_set_reduce_domain_to_axis_attr &
    ( reduce_domain_to_axis_id, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) :: reduce_domain_to_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::reduce_domain_to_axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: direction
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation

      CALL xios_get_reduce_domain_to_axis_handle &
      (reduce_domain_to_axis_id,reduce_domain_to_axis_hdl)
      CALL xios_set_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_set_reduce_domain_to_axis_attr

  SUBROUTINE xios_set_reduce_domain_to_axis_attr_hdl &
    ( reduce_domain_to_axis_hdl, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: direction
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation

      CALL xios_set_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_set_reduce_domain_to_axis_attr_hdl

  SUBROUTINE xios_set_reduce_domain_to_axis_attr_hdl_ &
    ( reduce_domain_to_axis_hdl, direction_, operation_ )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: direction_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: operation_

      IF (PRESENT(direction_)) THEN
        CALL cxios_set_reduce_domain_to_axis_direction &
      (reduce_domain_to_axis_hdl%daddr, direction_, len(direction_))
      ENDIF

      IF (PRESENT(operation_)) THEN
        CALL cxios_set_reduce_domain_to_axis_operation &
      (reduce_domain_to_axis_hdl%daddr, operation_, len(operation_))
      ENDIF

  END SUBROUTINE xios_set_reduce_domain_to_axis_attr_hdl_

  SUBROUTINE xios_get_reduce_domain_to_axis_attr &
    ( reduce_domain_to_axis_id, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) :: reduce_domain_to_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::reduce_domain_to_axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: direction
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation

      CALL xios_get_reduce_domain_to_axis_handle &
      (reduce_domain_to_axis_id,reduce_domain_to_axis_hdl)
      CALL xios_get_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_get_reduce_domain_to_axis_attr

  SUBROUTINE xios_get_reduce_domain_to_axis_attr_hdl &
    ( reduce_domain_to_axis_hdl, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: direction
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation

      CALL xios_get_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_get_reduce_domain_to_axis_attr_hdl

  SUBROUTINE xios_get_reduce_domain_to_axis_attr_hdl_ &
    ( reduce_domain_to_axis_hdl, direction_, operation_ )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: direction_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: operation_

      IF (PRESENT(direction_)) THEN
        CALL cxios_get_reduce_domain_to_axis_direction &
      (reduce_domain_to_axis_hdl%daddr, direction_, len(direction_))
      ENDIF

      IF (PRESENT(operation_)) THEN
        CALL cxios_get_reduce_domain_to_axis_operation &
      (reduce_domain_to_axis_hdl%daddr, operation_, len(operation_))
      ENDIF

  END SUBROUTINE xios_get_reduce_domain_to_axis_attr_hdl_

  SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr &
    ( reduce_domain_to_axis_id, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) :: reduce_domain_to_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::reduce_domain_to_axis_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: direction
      LOGICAL(KIND=C_BOOL) :: direction_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp

      CALL xios_get_reduce_domain_to_axis_handle &
      (reduce_domain_to_axis_id,reduce_domain_to_axis_hdl)
      CALL xios_is_defined_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr

  SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr_hdl &
    ( reduce_domain_to_axis_hdl, direction, operation )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: direction
      LOGICAL(KIND=C_BOOL) :: direction_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation
      LOGICAL(KIND=C_BOOL) :: operation_tmp

      CALL xios_is_defined_reduce_domain_to_axis_attr_hdl_ &
      ( reduce_domain_to_axis_hdl, direction, operation )

  END SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr_hdl

  SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr_hdl_ &
    ( reduce_domain_to_axis_hdl, direction_, operation_ )

    IMPLICIT NONE
      TYPE(xios_reduce_domain_to_axis) , INTENT(IN) :: reduce_domain_to_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: direction_
      LOGICAL(KIND=C_BOOL) :: direction__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: operation_
      LOGICAL(KIND=C_BOOL) :: operation__tmp

      IF (PRESENT(direction_)) THEN
        direction__tmp = cxios_is_defined_reduce_domain_to_axis_direction &
      (reduce_domain_to_axis_hdl%daddr)
        direction_ = direction__tmp
      ENDIF

      IF (PRESENT(operation_)) THEN
        operation__tmp = cxios_is_defined_reduce_domain_to_axis_operation &
      (reduce_domain_to_axis_hdl%daddr)
        operation_ = operation__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_reduce_domain_to_axis_attr_hdl_

END MODULE ireduce_domain_to_axis_attr
