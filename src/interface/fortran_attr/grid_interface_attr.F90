! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE grid_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_grid_axis_domain_order(grid_hdl, axis_domain_order, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: axis_domain_order
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_grid_axis_domain_order
    
    SUBROUTINE cxios_get_grid_axis_domain_order(grid_hdl, axis_domain_order, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: axis_domain_order
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_grid_axis_domain_order
    
    FUNCTION cxios_is_defined_grid_axis_domain_order(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_axis_domain_order
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_axis_domain_order
    
    
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
    
    FUNCTION cxios_is_defined_grid_description(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_description
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_description
    
    
    SUBROUTINE cxios_set_grid_mask1(grid_hdl, mask1, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask1
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_grid_mask1
    
    SUBROUTINE cxios_get_grid_mask1(grid_hdl, mask1, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask1
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_grid_mask1
    
    FUNCTION cxios_is_defined_grid_mask1(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask1
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask1
    
    
    SUBROUTINE cxios_set_grid_mask2(grid_hdl, mask2, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask2
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_set_grid_mask2
    
    SUBROUTINE cxios_get_grid_mask2(grid_hdl, mask2, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask2
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_get_grid_mask2
    
    FUNCTION cxios_is_defined_grid_mask2(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask2
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask2
    
    
    SUBROUTINE cxios_set_grid_mask3(grid_hdl, mask3, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask3
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_set_grid_mask3
    
    SUBROUTINE cxios_get_grid_mask3(grid_hdl, mask3, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: grid_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask3
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_get_grid_mask3
    
    FUNCTION cxios_is_defined_grid_mask3(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_mask3
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_mask3
    
    
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
    
    FUNCTION cxios_is_defined_grid_name(grid_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_grid_name
      INTEGER (kind = C_INTPTR_T), VALUE :: grid_hdl
    END FUNCTION cxios_is_defined_grid_name
    
    
    END INTERFACE
  
END MODULE grid_interface_attr
