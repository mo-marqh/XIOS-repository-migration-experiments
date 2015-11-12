! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE zoom_domain_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_zoom_domain_zoom_ibegin(zoom_domain_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ibegin
    END SUBROUTINE cxios_set_zoom_domain_zoom_ibegin

    SUBROUTINE cxios_get_zoom_domain_zoom_ibegin(zoom_domain_hdl, zoom_ibegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ibegin
    END SUBROUTINE cxios_get_zoom_domain_zoom_ibegin

    FUNCTION cxios_is_defined_zoom_domain_zoom_ibegin(zoom_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_domain_zoom_ibegin
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
    END FUNCTION cxios_is_defined_zoom_domain_zoom_ibegin


    SUBROUTINE cxios_set_zoom_domain_zoom_jbegin(zoom_domain_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_jbegin
    END SUBROUTINE cxios_set_zoom_domain_zoom_jbegin

    SUBROUTINE cxios_get_zoom_domain_zoom_jbegin(zoom_domain_hdl, zoom_jbegin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_jbegin
    END SUBROUTINE cxios_get_zoom_domain_zoom_jbegin

    FUNCTION cxios_is_defined_zoom_domain_zoom_jbegin(zoom_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_domain_zoom_jbegin
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
    END FUNCTION cxios_is_defined_zoom_domain_zoom_jbegin


    SUBROUTINE cxios_set_zoom_domain_zoom_ni(zoom_domain_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_ni
    END SUBROUTINE cxios_set_zoom_domain_zoom_ni

    SUBROUTINE cxios_get_zoom_domain_zoom_ni(zoom_domain_hdl, zoom_ni) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_ni
    END SUBROUTINE cxios_get_zoom_domain_zoom_ni

    FUNCTION cxios_is_defined_zoom_domain_zoom_ni(zoom_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_domain_zoom_ni
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
    END FUNCTION cxios_is_defined_zoom_domain_zoom_ni


    SUBROUTINE cxios_set_zoom_domain_zoom_nj(zoom_domain_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_nj
    END SUBROUTINE cxios_set_zoom_domain_zoom_nj

    SUBROUTINE cxios_get_zoom_domain_zoom_nj(zoom_domain_hdl, zoom_nj) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
      INTEGER (KIND=C_INT)             :: zoom_nj
    END SUBROUTINE cxios_get_zoom_domain_zoom_nj

    FUNCTION cxios_is_defined_zoom_domain_zoom_nj(zoom_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_domain_zoom_nj
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_domain_hdl
    END FUNCTION cxios_is_defined_zoom_domain_zoom_nj

  END INTERFACE

END MODULE zoom_domain_interface_attr
