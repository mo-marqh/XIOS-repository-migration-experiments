! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE zoom_axis_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_zoom_axis_zoom_begin(zoom_axis_hdl, zoom_begin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_begin
    END SUBROUTINE cxios_set_zoom_axis_zoom_begin

    SUBROUTINE cxios_get_zoom_axis_zoom_begin(zoom_axis_hdl, zoom_begin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)             :: zoom_begin
    END SUBROUTINE cxios_get_zoom_axis_zoom_begin

    FUNCTION cxios_is_defined_zoom_axis_zoom_begin(zoom_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_axis_zoom_begin
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
    END FUNCTION cxios_is_defined_zoom_axis_zoom_begin


    SUBROUTINE cxios_set_zoom_axis_zoom_end(zoom_axis_hdl, zoom_end) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_end
    END SUBROUTINE cxios_set_zoom_axis_zoom_end

    SUBROUTINE cxios_get_zoom_axis_zoom_end(zoom_axis_hdl, zoom_end) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)             :: zoom_end
    END SUBROUTINE cxios_get_zoom_axis_zoom_end

    FUNCTION cxios_is_defined_zoom_axis_zoom_end(zoom_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_axis_zoom_end
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
    END FUNCTION cxios_is_defined_zoom_axis_zoom_end


    SUBROUTINE cxios_set_zoom_axis_zoom_size(zoom_axis_hdl, zoom_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_size
    END SUBROUTINE cxios_set_zoom_axis_zoom_size

    SUBROUTINE cxios_get_zoom_axis_zoom_size(zoom_axis_hdl, zoom_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
      INTEGER (KIND=C_INT)             :: zoom_size
    END SUBROUTINE cxios_get_zoom_axis_zoom_size

    FUNCTION cxios_is_defined_zoom_axis_zoom_size(zoom_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_zoom_axis_zoom_size
      INTEGER (kind = C_INTPTR_T), VALUE :: zoom_axis_hdl
    END FUNCTION cxios_is_defined_zoom_axis_zoom_size

  END INTERFACE

END MODULE zoom_axis_interface_attr
