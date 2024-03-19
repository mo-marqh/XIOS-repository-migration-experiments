! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE redistribute_scalar_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_redistribute_scalar_index(redistribute_scalar_hdl, index, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_scalar_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: index
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_redistribute_scalar_index

    SUBROUTINE cxios_get_redistribute_scalar_index(redistribute_scalar_hdl, index, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_scalar_hdl
      INTEGER (KIND=C_INT)     , DIMENSION(*) :: index
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_redistribute_scalar_index

    FUNCTION cxios_is_defined_redistribute_scalar_index(redistribute_scalar_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_scalar_index
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_scalar_hdl
    END FUNCTION cxios_is_defined_redistribute_scalar_index


    SUBROUTINE cxios_set_redistribute_scalar_mask(redistribute_scalar_hdl, mask, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_scalar_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_redistribute_scalar_mask

    SUBROUTINE cxios_get_redistribute_scalar_mask(redistribute_scalar_hdl, mask, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: redistribute_scalar_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_redistribute_scalar_mask

    FUNCTION cxios_is_defined_redistribute_scalar_mask(redistribute_scalar_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_scalar_mask
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_scalar_hdl
    END FUNCTION cxios_is_defined_redistribute_scalar_mask


    SUBROUTINE cxios_set_redistribute_scalar_type(redistribute_scalar_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_scalar_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_set_redistribute_scalar_type

    SUBROUTINE cxios_get_redistribute_scalar_type(redistribute_scalar_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_scalar_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_get_redistribute_scalar_type

    FUNCTION cxios_is_defined_redistribute_scalar_type(redistribute_scalar_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_redistribute_scalar_type
      INTEGER (kind = C_INTPTR_T), VALUE :: redistribute_scalar_hdl
    END FUNCTION cxios_is_defined_redistribute_scalar_type

  END INTERFACE

END MODULE redistribute_scalar_interface_attr
