! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE grid_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_grid_description(grid_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_grid_description

    SUBROUTINE cxios_get_grid_description(grid_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_grid_description

    FUNCTION cxios_is_defined_grid_description(grid_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_description
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_description


    SUBROUTINE cxios_set_grid_mask_1d(grid_hdl, mask_1d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_1d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_grid_mask_1d

    SUBROUTINE cxios_get_grid_mask_1d(grid_hdl, mask_1d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_1d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_grid_mask_1d

    FUNCTION cxios_is_defined_grid_mask_1d(grid_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask_1d
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask_1d


    SUBROUTINE cxios_set_grid_mask_2d(grid_hdl, mask_2d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_2d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_grid_mask_2d

    SUBROUTINE cxios_get_grid_mask_2d(grid_hdl, mask_2d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_2d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_grid_mask_2d

    FUNCTION cxios_is_defined_grid_mask_2d(grid_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask_2d
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask_2d


    SUBROUTINE cxios_set_grid_mask_3d(grid_hdl, mask_3d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_3d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_set_grid_mask_3d

    SUBROUTINE cxios_get_grid_mask_3d(grid_hdl, mask_3d, extent) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask_3d
      INTEGER (kind = C_INT), DIMENSION(*)     :: extent
    END SUBROUTINE cxios_get_grid_mask_3d

    FUNCTION cxios_is_defined_grid_mask_3d(grid_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask_3d
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask_3d


    SUBROUTINE cxios_set_grid_name(grid_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_grid_name

    SUBROUTINE cxios_get_grid_name(grid_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_grid_name

    FUNCTION cxios_is_defined_grid_name(grid_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_name
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_name

  END INTERFACE

END MODULE grid_interface_attr
