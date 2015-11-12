! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE izoom_domain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE izoom_domain
  USE zoom_domain_interface_attr

CONTAINS

  SUBROUTINE xios(set_zoom_domain_attr)  &
    ( zoom_domain_id, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain))  :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj

      CALL xios(get_zoom_domain_handle)(zoom_domain_id,zoom_domain_hdl)
      CALL xios(set_zoom_domain_attr_hdl_)   &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(set_zoom_domain_attr)

  SUBROUTINE xios(set_zoom_domain_attr_hdl)  &
    ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj

      CALL xios(set_zoom_domain_attr_hdl_)  &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(set_zoom_domain_attr_hdl)

  SUBROUTINE xios(set_zoom_domain_attr_hdl_)   &
    ( zoom_domain_hdl, zoom_ibegin_, zoom_jbegin_, zoom_ni_, zoom_nj_ )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj_

      IF (PRESENT(zoom_ibegin_)) THEN
        CALL cxios_set_zoom_domain_zoom_ibegin(zoom_domain_hdl%daddr, zoom_ibegin_)
      ENDIF

      IF (PRESENT(zoom_jbegin_)) THEN
        CALL cxios_set_zoom_domain_zoom_jbegin(zoom_domain_hdl%daddr, zoom_jbegin_)
      ENDIF

      IF (PRESENT(zoom_ni_)) THEN
        CALL cxios_set_zoom_domain_zoom_ni(zoom_domain_hdl%daddr, zoom_ni_)
      ENDIF

      IF (PRESENT(zoom_nj_)) THEN
        CALL cxios_set_zoom_domain_zoom_nj(zoom_domain_hdl%daddr, zoom_nj_)
      ENDIF

  END SUBROUTINE xios(set_zoom_domain_attr_hdl_)

  SUBROUTINE xios(get_zoom_domain_attr)  &
    ( zoom_domain_id, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain))  :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj

      CALL xios(get_zoom_domain_handle)(zoom_domain_id,zoom_domain_hdl)
      CALL xios(get_zoom_domain_attr_hdl_)   &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(get_zoom_domain_attr)

  SUBROUTINE xios(get_zoom_domain_attr_hdl)  &
    ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj

      CALL xios(get_zoom_domain_attr_hdl_)  &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(get_zoom_domain_attr_hdl)

  SUBROUTINE xios(get_zoom_domain_attr_hdl_)   &
    ( zoom_domain_hdl, zoom_ibegin_, zoom_jbegin_, zoom_ni_, zoom_nj_ )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj_

      IF (PRESENT(zoom_ibegin_)) THEN
        CALL cxios_get_zoom_domain_zoom_ibegin(zoom_domain_hdl%daddr, zoom_ibegin_)
      ENDIF

      IF (PRESENT(zoom_jbegin_)) THEN
        CALL cxios_get_zoom_domain_zoom_jbegin(zoom_domain_hdl%daddr, zoom_jbegin_)
      ENDIF

      IF (PRESENT(zoom_ni_)) THEN
        CALL cxios_get_zoom_domain_zoom_ni(zoom_domain_hdl%daddr, zoom_ni_)
      ENDIF

      IF (PRESENT(zoom_nj_)) THEN
        CALL cxios_get_zoom_domain_zoom_nj(zoom_domain_hdl%daddr, zoom_nj_)
      ENDIF

  END SUBROUTINE xios(get_zoom_domain_attr_hdl_)

  SUBROUTINE xios(is_defined_zoom_domain_attr)  &
    ( zoom_domain_id, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain))  :: zoom_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_domain_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ibegin
      LOGICAL(KIND=C_BOOL) :: zoom_ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_jbegin
      LOGICAL(KIND=C_BOOL) :: zoom_jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ni
      LOGICAL(KIND=C_BOOL) :: zoom_ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_nj
      LOGICAL(KIND=C_BOOL) :: zoom_nj_tmp

      CALL xios(get_zoom_domain_handle)(zoom_domain_id,zoom_domain_hdl)
      CALL xios(is_defined_zoom_domain_attr_hdl_)   &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(is_defined_zoom_domain_attr)

  SUBROUTINE xios(is_defined_zoom_domain_attr_hdl)  &
    ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ibegin
      LOGICAL(KIND=C_BOOL) :: zoom_ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_jbegin
      LOGICAL(KIND=C_BOOL) :: zoom_jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ni
      LOGICAL(KIND=C_BOOL) :: zoom_ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_nj
      LOGICAL(KIND=C_BOOL) :: zoom_nj_tmp

      CALL xios(is_defined_zoom_domain_attr_hdl_)  &
      ( zoom_domain_hdl, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj )

  END SUBROUTINE xios(is_defined_zoom_domain_attr_hdl)

  SUBROUTINE xios(is_defined_zoom_domain_attr_hdl_)   &
    ( zoom_domain_hdl, zoom_ibegin_, zoom_jbegin_, zoom_ni_, zoom_nj_ )

    IMPLICIT NONE
      TYPE(txios(zoom_domain)) , INTENT(IN) :: zoom_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ibegin_
      LOGICAL(KIND=C_BOOL) :: zoom_ibegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_jbegin_
      LOGICAL(KIND=C_BOOL) :: zoom_jbegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_ni_
      LOGICAL(KIND=C_BOOL) :: zoom_ni__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_nj_
      LOGICAL(KIND=C_BOOL) :: zoom_nj__tmp

      IF (PRESENT(zoom_ibegin_)) THEN
        zoom_ibegin__tmp = cxios_is_defined_zoom_domain_zoom_ibegin(zoom_domain_hdl%daddr)
        zoom_ibegin_ = zoom_ibegin__tmp
      ENDIF

      IF (PRESENT(zoom_jbegin_)) THEN
        zoom_jbegin__tmp = cxios_is_defined_zoom_domain_zoom_jbegin(zoom_domain_hdl%daddr)
        zoom_jbegin_ = zoom_jbegin__tmp
      ENDIF

      IF (PRESENT(zoom_ni_)) THEN
        zoom_ni__tmp = cxios_is_defined_zoom_domain_zoom_ni(zoom_domain_hdl%daddr)
        zoom_ni_ = zoom_ni__tmp
      ENDIF

      IF (PRESENT(zoom_nj_)) THEN
        zoom_nj__tmp = cxios_is_defined_zoom_domain_zoom_nj(zoom_domain_hdl%daddr)
        zoom_nj_ = zoom_nj__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_zoom_domain_attr_hdl_)

END MODULE izoom_domain_attr
