! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE ivariable_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ivariable
  USE variable_interface_attr

CONTAINS

  SUBROUTINE xios_set_variable_attr &
    ( variable_id, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) :: variable_hdl
      CHARACTER(LEN=*), INTENT(IN) ::variable_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_get_variable_handle &
      (variable_id,variable_hdl)
      CALL xios_set_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_set_variable_attr

  SUBROUTINE xios_set_variable_attr_hdl &
    ( variable_hdl, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_set_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_set_variable_attr_hdl

  SUBROUTINE xios_set_variable_attr_hdl_ &
    ( variable_hdl, name_, type_ )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(name_)) THEN
        CALL cxios_set_variable_name &
      (variable_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_variable_type &
      (variable_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_set_variable_attr_hdl_

  SUBROUTINE xios_get_variable_attr &
    ( variable_id, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) :: variable_hdl
      CHARACTER(LEN=*), INTENT(IN) ::variable_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_variable_handle &
      (variable_id,variable_hdl)
      CALL xios_get_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_get_variable_attr

  SUBROUTINE xios_get_variable_attr_hdl &
    ( variable_hdl, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_get_variable_attr_hdl

  SUBROUTINE xios_get_variable_attr_hdl_ &
    ( variable_hdl, name_, type_ )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(name_)) THEN
        CALL cxios_get_variable_name &
      (variable_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_variable_type &
      (variable_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_get_variable_attr_hdl_

  SUBROUTINE xios_is_defined_variable_attr &
    ( variable_id, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) :: variable_hdl
      CHARACTER(LEN=*), INTENT(IN) ::variable_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_get_variable_handle &
      (variable_id,variable_hdl)
      CALL xios_is_defined_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_is_defined_variable_attr

  SUBROUTINE xios_is_defined_variable_attr_hdl &
    ( variable_hdl, name, type )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_is_defined_variable_attr_hdl_ &
      ( variable_hdl, name, type )

  END SUBROUTINE xios_is_defined_variable_attr_hdl

  SUBROUTINE xios_is_defined_variable_attr_hdl_ &
    ( variable_hdl, name_, type_ )

    IMPLICIT NONE
      TYPE(xios_variable) , INTENT(IN) :: variable_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_variable_name &
      (variable_hdl%daddr)
        name_ = name__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_variable_type &
      (variable_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_variable_attr_hdl_

END MODULE ivariable_attr
