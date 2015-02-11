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
    ( gridgroup_id, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(set_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(set_gridgroup_attr)
  
  SUBROUTINE xios(set_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(set_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(set_gridgroup_attr_hdl)
  
  SUBROUTINE xios(set_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axisDomainOrder_, description_, group_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      
      IF (PRESENT(axisDomainOrder_)) THEN
        ALLOCATE(axisDomainOrder__tmp(size(axisDomainOrder_,1)))
        axisDomainOrder__tmp=axisDomainOrder_
        CALL cxios_set_gridgroup_axisDomainOrder(gridgroup_hdl%daddr, axisDomainOrder__tmp,size(axisDomainOrder_,1))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        mask__tmp=mask_
        CALL cxios_set_gridgroup_mask(gridgroup_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_gridgroup_attr_hdl_)
  
  SUBROUTINE xios(get_gridgroup_attr)  &
    ( gridgroup_id, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(get_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(get_gridgroup_attr)
  
  SUBROUTINE xios(get_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(get_gridgroup_attr_hdl)
  
  SUBROUTINE xios(get_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axisDomainOrder_, description_, group_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      
      IF (PRESENT(axisDomainOrder_)) THEN
        ALLOCATE(axisDomainOrder__tmp(size(axisDomainOrder_,1)))
        CALL cxios_get_gridgroup_axisDomainOrder(gridgroup_hdl%daddr, axisDomainOrder__tmp,size(axisDomainOrder_,1))
        axisDomainOrder_=axisDomainOrder__tmp_
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        CALL cxios_get_gridgroup_mask(gridgroup_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_gridgroup_attr_hdl_)
  
  SUBROUTINE xios(is_defined_gridgroup_attr)  &
    ( gridgroup_id, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(is_defined_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(is_defined_gridgroup_attr)
  
  SUBROUTINE xios(is_defined_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(is_defined_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axisDomainOrder, description, group_ref, mask, name )
    
  END SUBROUTINE xios(is_defined_gridgroup_attr_hdl)
  
  SUBROUTINE xios(is_defined_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axisDomainOrder_, description_, group_ref_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder_
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description_
      LOGICAL(KIND=C_BOOL) :: description__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      
      IF (PRESENT(axisDomainOrder_)) THEN
        axisDomainOrder__tmp=cxios_is_defined_gridgroup_axisDomainOrder(gridgroup_hdl%daddr)
        axisDomainOrder_=axisDomainOrder__tmp
      ENDIF
      
      IF (PRESENT(description_)) THEN
        description__tmp=cxios_is_defined_gridgroup_description(gridgroup_hdl%daddr)
        description_=description__tmp
      ENDIF
      
      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp=cxios_is_defined_gridgroup_group_ref(gridgroup_hdl%daddr)
        group_ref_=group_ref__tmp
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        mask__tmp=cxios_is_defined_gridgroup_mask(gridgroup_hdl%daddr)
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        name__tmp=cxios_is_defined_gridgroup_name(gridgroup_hdl%daddr)
        name_=name__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_gridgroup_attr_hdl_)
  
END MODULE igridgroup_attr
