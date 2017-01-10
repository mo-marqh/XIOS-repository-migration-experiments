! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE izoom_domain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE izoom_domain
  USE zoom_domain_interface_attr

CONTAINS

  SUBROUTINE xios_set_zoom_domain_attr &
    ( zoom_domain_id, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      INTEGER , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER , OPTIONAL, INTENT(IN) :: ni
      INTEGER , OPTIONAL, INTENT(IN) :: nj

      CALL xios_get_zoom_domain_handle &
      (zoom_domain_id,zoom_domain_hdl)
      CALL xios_set_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_set_zoom_domain_attr

  SUBROUTINE xios_set_zoom_domain_attr_hdl &
    ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER , OPTIONAL, INTENT(IN) :: ni
      INTEGER , OPTIONAL, INTENT(IN) :: nj

      CALL xios_set_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_set_zoom_domain_attr_hdl

  SUBROUTINE xios_set_zoom_domain_attr_hdl_ &
    ( zoom_domain_hdl, ibegin_, jbegin_, ni_, nj_ )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: ibegin_
      INTEGER , OPTIONAL, INTENT(IN) :: jbegin_
      INTEGER , OPTIONAL, INTENT(IN) :: ni_
      INTEGER , OPTIONAL, INTENT(IN) :: nj_

      IF (PRESENT(ibegin_)) THEN
        CALL cxios_set_zoom_domain_ibegin &
      (zoom_domain_hdl%daddr, ibegin_)
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        CALL cxios_set_zoom_domain_jbegin &
      (zoom_domain_hdl%daddr, jbegin_)
      ENDIF

      IF (PRESENT(ni_)) THEN
        CALL cxios_set_zoom_domain_ni &
      (zoom_domain_hdl%daddr, ni_)
      ENDIF

      IF (PRESENT(nj_)) THEN
        CALL cxios_set_zoom_domain_nj &
      (zoom_domain_hdl%daddr, nj_)
      ENDIF

  END SUBROUTINE xios_set_zoom_domain_attr_hdl_

  SUBROUTINE xios_get_zoom_domain_attr &
    ( zoom_domain_id, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      INTEGER , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER , OPTIONAL, INTENT(OUT) :: jbegin
      INTEGER , OPTIONAL, INTENT(OUT) :: ni
      INTEGER , OPTIONAL, INTENT(OUT) :: nj

      CALL xios_get_zoom_domain_handle &
      (zoom_domain_id,zoom_domain_hdl)
      CALL xios_get_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_get_zoom_domain_attr

  SUBROUTINE xios_get_zoom_domain_attr_hdl &
    ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER , OPTIONAL, INTENT(OUT) :: jbegin
      INTEGER , OPTIONAL, INTENT(OUT) :: ni
      INTEGER , OPTIONAL, INTENT(OUT) :: nj

      CALL xios_get_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_get_zoom_domain_attr_hdl

  SUBROUTINE xios_get_zoom_domain_attr_hdl_ &
    ( zoom_domain_hdl, ibegin_, jbegin_, ni_, nj_ )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: ibegin_
      INTEGER , OPTIONAL, INTENT(OUT) :: jbegin_
      INTEGER , OPTIONAL, INTENT(OUT) :: ni_
      INTEGER , OPTIONAL, INTENT(OUT) :: nj_

      IF (PRESENT(ibegin_)) THEN
        CALL cxios_get_zoom_domain_ibegin &
      (zoom_domain_hdl%daddr, ibegin_)
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        CALL cxios_get_zoom_domain_jbegin &
      (zoom_domain_hdl%daddr, jbegin_)
      ENDIF

      IF (PRESENT(ni_)) THEN
        CALL cxios_get_zoom_domain_ni &
      (zoom_domain_hdl%daddr, ni_)
      ENDIF

      IF (PRESENT(nj_)) THEN
        CALL cxios_get_zoom_domain_nj &
      (zoom_domain_hdl%daddr, nj_)
      ENDIF

  END SUBROUTINE xios_get_zoom_domain_attr_hdl_

  SUBROUTINE xios_is_defined_zoom_domain_attr &
    ( zoom_domain_id, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin
      LOGICAL(KIND=C_BOOL) :: ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin
      LOGICAL(KIND=C_BOOL) :: jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni
      LOGICAL(KIND=C_BOOL) :: ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj
      LOGICAL(KIND=C_BOOL) :: nj_tmp

      CALL xios_get_zoom_domain_handle &
      (zoom_domain_id,zoom_domain_hdl)
      CALL xios_is_defined_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_is_defined_zoom_domain_attr

  SUBROUTINE xios_is_defined_zoom_domain_attr_hdl &
    ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin
      LOGICAL(KIND=C_BOOL) :: ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin
      LOGICAL(KIND=C_BOOL) :: jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni
      LOGICAL(KIND=C_BOOL) :: ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj
      LOGICAL(KIND=C_BOOL) :: nj_tmp

      CALL xios_is_defined_zoom_domain_attr_hdl_ &
      ( zoom_domain_hdl, ibegin, jbegin, ni, nj )

  END SUBROUTINE xios_is_defined_zoom_domain_attr_hdl

  SUBROUTINE xios_is_defined_zoom_domain_attr_hdl_ &
    ( zoom_domain_hdl, ibegin_, jbegin_, ni_, nj_ )

    IMPLICIT NONE
      TYPE(xios_zoom_domain) , INTENT(IN) :: zoom_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin_
      LOGICAL(KIND=C_BOOL) :: ibegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin_
      LOGICAL(KIND=C_BOOL) :: jbegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni_
      LOGICAL(KIND=C_BOOL) :: ni__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj_
      LOGICAL(KIND=C_BOOL) :: nj__tmp

      IF (PRESENT(ibegin_)) THEN
        ibegin__tmp = cxios_is_defined_zoom_domain_ibegin &
      (zoom_domain_hdl%daddr)
        ibegin_ = ibegin__tmp
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        jbegin__tmp = cxios_is_defined_zoom_domain_jbegin &
      (zoom_domain_hdl%daddr)
        jbegin_ = jbegin__tmp
      ENDIF

      IF (PRESENT(ni_)) THEN
        ni__tmp = cxios_is_defined_zoom_domain_ni &
      (zoom_domain_hdl%daddr)
        ni_ = ni__tmp
      ENDIF

      IF (PRESENT(nj_)) THEN
        nj__tmp = cxios_is_defined_zoom_domain_nj &
      (zoom_domain_hdl%daddr)
        nj_ = nj__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_zoom_domain_attr_hdl_

END MODULE izoom_domain_attr
