! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE igrid_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE igrid
  USE grid_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_grid_attr)  &
    ( grid_id, axis_ref, description, domain_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(set_grid_attr_hdl_)   &
      ( grid_hdl, axis_ref, description, domain_ref, name )
    
  END SUBROUTINE xios(set_grid_attr)
  
  SUBROUTINE xios(set_grid_attr_hdl)  &
    ( grid_hdl, axis_ref, description, domain_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(set_grid_attr_hdl_)  &
      ( grid_hdl, axis_ref, description, domain_ref, name )
    
  END SUBROUTINE xios(set_grid_attr_hdl)
  
  SUBROUTINE xios(set_grid_attr_hdl_)   &
    ( grid_hdl, axis_ref_, description_, domain_ref_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_grid_axis_ref(grid_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_grid_domain_ref(grid_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_grid_attr_hdl_)
  
  SUBROUTINE xios(get_grid_attr)  &
    ( grid_id, axis_ref, description, domain_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(get_grid_attr_hdl_)   &
      ( grid_hdl, axis_ref, description, domain_ref, name )
    
  END SUBROUTINE xios(get_grid_attr)
  
  SUBROUTINE xios(get_grid_attr_hdl)  &
    ( grid_hdl, axis_ref, description, domain_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_attr_hdl_)  &
      ( grid_hdl, axis_ref, description, domain_ref, name )
    
  END SUBROUTINE xios(get_grid_attr_hdl)
  
  SUBROUTINE xios(get_grid_attr_hdl_)   &
    ( grid_hdl, axis_ref_, description_, domain_ref_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_grid_axis_ref(grid_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_grid_domain_ref(grid_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_grid_attr_hdl_)
  
END MODULE igrid_attr
