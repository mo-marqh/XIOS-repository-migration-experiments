! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE redistribute_axis_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_redistribute_axis_index(redistribute_axis_hdl, index, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_axis_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: index
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_redistribute_axis_index

    SUBROUTINE cxios_get_redistribute_axis_index(redistribute_axis_hdl, index, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_axis_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: index
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_redistribute_axis_index

    FUNCTION cxios_is_defined_redistribute_axis_index(redistribute_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_axis_index
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_axis_hdl
    END FUNCTION cxios_is_defined_redistribute_axis_index


    SUBROUTINE cxios_set_redistribute_axis_mask(redistribute_axis_hdl, mask, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_axis_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_redistribute_axis_mask

    SUBROUTINE cxios_get_redistribute_axis_mask(redistribute_axis_hdl, mask, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_axis_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_redistribute_axis_mask

    FUNCTION cxios_is_defined_redistribute_axis_mask(redistribute_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_axis_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_axis_hdl
    END FUNCTION cxios_is_defined_redistribute_axis_mask


    SUBROUTINE cxios_set_redistribute_axis_type(redistribute_axis_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_axis_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_set_redistribute_axis_type

    SUBROUTINE cxios_get_redistribute_axis_type(redistribute_axis_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_axis_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_get_redistribute_axis_type

    FUNCTION cxios_is_defined_redistribute_axis_type(redistribute_axis_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_axis_type
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_axis_hdl
    END FUNCTION cxios_is_defined_redistribute_axis_type

  END INTERFACE

END MODULE redistribute_axis_interface_attr
