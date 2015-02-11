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
    ( grid_id, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(set_grid_attr_hdl_)   &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(set_grid_attr)
  
  SUBROUTINE xios(set_grid_attr_hdl)  &
    ( grid_hdl, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      
      CALL xios(set_grid_attr_hdl_)  &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(set_grid_attr_hdl)
  
  SUBROUTINE xios(set_grid_attr_hdl_)   &
    ( grid_hdl, axisDomainOrder_, description_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axisDomainOrder_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      
      IF (PRESENT(axisDomainOrder_)) THEN
        ALLOCATE(axisDomainOrder__tmp(size(axisDomainOrder_,1)))
        axisDomainOrder__tmp=axisDomainOrder_
        CALL cxios_set_grid_axisDomainOrder(grid_hdl%daddr, axisDomainOrder__tmp,size(axisDomainOrder_,1))
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        mask__tmp=mask_
        CALL cxios_set_grid_mask(grid_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_grid_attr_hdl_)
  
  SUBROUTINE xios(get_grid_attr)  &
    ( grid_id, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(get_grid_attr_hdl_)   &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(get_grid_attr)
  
  SUBROUTINE xios(get_grid_attr_hdl)  &
    ( grid_hdl, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      
      CALL xios(get_grid_attr_hdl_)  &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(get_grid_attr_hdl)
  
  SUBROUTINE xios(get_grid_attr_hdl_)   &
    ( grid_hdl, axisDomainOrder_, description_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axisDomainOrder_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axisDomainOrder__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      
      IF (PRESENT(axisDomainOrder_)) THEN
        ALLOCATE(axisDomainOrder__tmp(size(axisDomainOrder_,1)))
        CALL cxios_get_grid_axisDomainOrder(grid_hdl%daddr, axisDomainOrder__tmp,size(axisDomainOrder_,1))
        axisDomainOrder_=axisDomainOrder__tmp_
      ENDIF
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_grid_description(grid_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2),size(mask_,3)))
        CALL cxios_get_grid_mask(grid_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2),size(mask_,3))
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_grid_name(grid_hdl%daddr, name_, len(name_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_grid_attr_hdl_)
  
  SUBROUTINE xios(is_defined_grid_attr)  &
    ( grid_id, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid))  :: grid_hdl
      CHARACTER(LEN=*), INTENT(IN) ::grid_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(get_grid_handle)(grid_id,grid_hdl)
      CALL xios(is_defined_grid_attr_hdl_)   &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(is_defined_grid_attr)
  
  SUBROUTINE xios(is_defined_grid_attr_hdl)  &
    ( grid_hdl, axisDomainOrder, description, mask, name )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      
      CALL xios(is_defined_grid_attr_hdl_)  &
      ( grid_hdl, axisDomainOrder, description, mask, name )
    
  END SUBROUTINE xios(is_defined_grid_attr_hdl)
  
  SUBROUTINE xios(is_defined_grid_attr_hdl_)   &
    ( grid_hdl, axisDomainOrder_, description_, mask_, name_ )
    
    IMPLICIT NONE
      TYPE(txios(grid)) , INTENT(IN) :: grid_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axisDomainOrder_
      LOGICAL(KIND=C_BOOL) :: axisDomainOrder__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description_
      LOGICAL(KIND=C_BOOL) :: description__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      
      IF (PRESENT(axisDomainOrder_)) THEN
        axisDomainOrder__tmp=cxios_is_defined_grid_axisDomainOrder(grid_hdl%daddr)
        axisDomainOrder_=axisDomainOrder__tmp
      ENDIF
      
      IF (PRESENT(description_)) THEN
        description__tmp=cxios_is_defined_grid_description(grid_hdl%daddr)
        description_=description__tmp
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        mask__tmp=cxios_is_defined_grid_mask(grid_hdl%daddr)
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        name__tmp=cxios_is_defined_grid_name(grid_hdl%daddr)
        name_=name__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_grid_attr_hdl_)
  
END MODULE igrid_attr
