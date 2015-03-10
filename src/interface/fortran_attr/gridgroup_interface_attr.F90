! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE gridgroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_gridgroup_axisDomainOrder(gridgroup_hdl, axisDomainOrder, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: axisDomainOrder
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_gridgroup_axisDomainOrder
    
    SUBROUTINE cxios_get_gridgroup_axisDomainOrder(gridgroup_hdl, axisDomainOrder, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: axisDomainOrder
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_gridgroup_axisDomainOrder
    
    FUNCTION cxios_is_defined_gridgroup_axisDomainOrder(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_axisDomainOrder
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_axisDomainOrder
    
    
    SUBROUTINE cxios_set_gridgroup_description(gridgroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_gridgroup_description
    
    SUBROUTINE cxios_get_gridgroup_description(gridgroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_gridgroup_description
    
    FUNCTION cxios_is_defined_gridgroup_description(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_description
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_description
    
    
    SUBROUTINE cxios_set_gridgroup_group_ref(gridgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_gridgroup_group_ref
    
    SUBROUTINE cxios_get_gridgroup_group_ref(gridgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_gridgroup_group_ref
    
    FUNCTION cxios_is_defined_gridgroup_group_ref(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_group_ref
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_group_ref
    
    
    SUBROUTINE cxios_set_gridgroup_mask1(gridgroup_hdl, mask1, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask1
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_gridgroup_mask1
    
    SUBROUTINE cxios_get_gridgroup_mask1(gridgroup_hdl, mask1, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask1
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_gridgroup_mask1
    
    FUNCTION cxios_is_defined_gridgroup_mask1(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_mask1
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_mask1
    
    
    SUBROUTINE cxios_set_gridgroup_mask2(gridgroup_hdl, mask2, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask2
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_set_gridgroup_mask2
    
    SUBROUTINE cxios_get_gridgroup_mask2(gridgroup_hdl, mask2, extent1, extent2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask2
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
    END SUBROUTINE cxios_get_gridgroup_mask2
    
    FUNCTION cxios_is_defined_gridgroup_mask2(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_mask2
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_mask2
    
    
    SUBROUTINE cxios_set_gridgroup_mask3(gridgroup_hdl, mask3, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask3
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_set_gridgroup_mask3
    
    SUBROUTINE cxios_get_gridgroup_mask3(gridgroup_hdl, mask3, extent1, extent2, extent3) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: gridgroup_hdl
      LOGICAL (KIND=C_BOOL)     , DIMENSION(*) :: mask3
      INTEGER (kind = C_INT), VALUE  :: extent1
      INTEGER (kind = C_INT), VALUE  :: extent2
      INTEGER (kind = C_INT), VALUE  :: extent3
    END SUBROUTINE cxios_get_gridgroup_mask3
    
    FUNCTION cxios_is_defined_gridgroup_mask3(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_mask3
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_mask3
    
    
    SUBROUTINE cxios_set_gridgroup_name(gridgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_gridgroup_name
    
    SUBROUTINE cxios_get_gridgroup_name(gridgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_gridgroup_name
    
    FUNCTION cxios_is_defined_gridgroup_name(gridgroup_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_gridgroup_name
      INTEGER (kind = C_INTPTR_T), VALUE :: gridgroup_hdl
    END FUNCTION cxios_is_defined_gridgroup_name
    
    
    END INTERFACE
  
END MODULE gridgroup_interface_attr
