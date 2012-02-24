! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE igridgroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE igrid
  USE gridgroup_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_gridgroup_attr)  &
    ( gridgroup_id, axis_ref, description, domain_ref, group_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(set_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
  END SUBROUTINE xios(set_gridgroup_attr)
  
  SUBROUTINE xios(set_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(set_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
  END SUBROUTINE xios(set_gridgroup_attr_hdl)
  
  SUBROUTINE xios(set_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axis_ref_, description_, domain_ref_, group_ref_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_set_gridgroup_axis_ref(gridgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_gridgroup_domain_ref(gridgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_gridgroup_attr_hdl_)
  
  SUBROUTINE xios(get_gridgroup_attr)  &
    ( gridgroup_id, axis_ref, description, domain_ref, group_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(get_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
  END SUBROUTINE xios(get_gridgroup_attr)
  
  SUBROUTINE xios(get_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axis_ref, description, domain_ref, group_ref, name )
    
  END SUBROUTINE xios(get_gridgroup_attr_hdl)
  
  SUBROUTINE xios(get_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axis_ref_, description_, domain_ref_, group_ref_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: axis_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      
      IF (PRESENT(axis_ref_)) THEN
        CALL cxios_get_gridgroup_axis_ref(gridgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_gridgroup_domain_ref(gridgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_gridgroup_attr_hdl_)
  
END MODULE igridgroup_attr
