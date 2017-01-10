! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE iexpand_domain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iexpand_domain
  USE expand_domain_interface_attr

CONTAINS

  SUBROUTINE xios_set_expand_domain_attr &
    ( expand_domain_id, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) :: expand_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::expand_domain_id
      INTEGER , OPTIONAL, INTENT(IN) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_get_expand_domain_handle &
      (expand_domain_id,expand_domain_hdl)
      CALL xios_set_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_set_expand_domain_attr

  SUBROUTINE xios_set_expand_domain_attr_hdl &
    ( expand_domain_hdl, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_set_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_set_expand_domain_attr_hdl

  SUBROUTINE xios_set_expand_domain_attr_hdl_ &
    ( expand_domain_hdl, order_, type_ )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: order_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(order_)) THEN
        CALL cxios_set_expand_domain_order &
      (expand_domain_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_expand_domain_type &
      (expand_domain_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_set_expand_domain_attr_hdl_

  SUBROUTINE xios_get_expand_domain_attr &
    ( expand_domain_id, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) :: expand_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::expand_domain_id
      INTEGER , OPTIONAL, INTENT(OUT) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_expand_domain_handle &
      (expand_domain_id,expand_domain_hdl)
      CALL xios_get_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_get_expand_domain_attr

  SUBROUTINE xios_get_expand_domain_attr_hdl &
    ( expand_domain_hdl, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_get_expand_domain_attr_hdl

  SUBROUTINE xios_get_expand_domain_attr_hdl_ &
    ( expand_domain_hdl, order_, type_ )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: order_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(order_)) THEN
        CALL cxios_get_expand_domain_order &
      (expand_domain_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_expand_domain_type &
      (expand_domain_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_get_expand_domain_attr_hdl_

  SUBROUTINE xios_is_defined_expand_domain_attr &
    ( expand_domain_id, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) :: expand_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::expand_domain_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_get_expand_domain_handle &
      (expand_domain_id,expand_domain_hdl)
      CALL xios_is_defined_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_is_defined_expand_domain_attr

  SUBROUTINE xios_is_defined_expand_domain_attr_hdl &
    ( expand_domain_hdl, order, type )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_is_defined_expand_domain_attr_hdl_ &
      ( expand_domain_hdl, order, type )

  END SUBROUTINE xios_is_defined_expand_domain_attr_hdl

  SUBROUTINE xios_is_defined_expand_domain_attr_hdl_ &
    ( expand_domain_hdl, order_, type_ )

    IMPLICIT NONE
      TYPE(xios_expand_domain) , INTENT(IN) :: expand_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: order_
      LOGICAL(KIND=C_BOOL) :: order__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(order_)) THEN
        order__tmp = cxios_is_defined_expand_domain_order &
      (expand_domain_hdl%daddr)
        order_ = order__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_expand_domain_type &
      (expand_domain_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_expand_domain_attr_hdl_

END MODULE iexpand_domain_attr
