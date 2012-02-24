! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE idomain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE idomain
  USE domain_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_domain_attr)  &
    ( domain_id, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
    , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
    , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
    , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
    IMPLICIT NONE
      TYPE(txios(domain))  :: domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::domain_id
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: iend
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: jend
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: latvalue(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: lonvalue(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: ni
      INTEGER  , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER  , OPTIONAL, INTENT(IN) :: nj
      INTEGER  , OPTIONAL, INTENT(IN) :: nj_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj_loc
      
      CALL xios(get_domain_handle)(domain_id,domain_hdl)
      CALL xios(set_domain_attr_hdl_)   &
      ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
      , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
      , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
      , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
  END SUBROUTINE xios(set_domain_attr)
  
  SUBROUTINE xios(set_domain_attr_hdl)  &
    ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
    , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
    , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
    , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
    IMPLICIT NONE
      TYPE(txios(domain)) , INTENT(IN) :: domain_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: iend
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: jend
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: latvalue(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: lonvalue(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: ni
      INTEGER  , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER  , OPTIONAL, INTENT(IN) :: nj
      INTEGER  , OPTIONAL, INTENT(IN) :: nj_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni_loc
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj_loc
      
      CALL xios(set_domain_attr_hdl_)  &
      ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
      , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
      , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
      , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
  END SUBROUTINE xios(set_domain_attr_hdl)
  
  SUBROUTINE xios(set_domain_attr_hdl_)   &
    ( domain_hdl, data_dim_, data_i_index_, data_ibegin_, data_j_index_, data_jbegin_, data_n_index_  &
    , data_ni_, data_nj_, domain_group_ref_, ibegin_, iend_, jbegin_, jend_, latvalue_, long_name_  &
    , lonvalue_, mask_, name_, ni_, ni_glo_, nj_, nj_glo_, standard_name_, zoom_ibegin_, zoom_ibegin_loc_  &
    , zoom_jbegin_, zoom_jbegin_loc_, zoom_ni_, zoom_ni_loc_, zoom_nj_, zoom_nj_loc_ )
    
    IMPLICIT NONE
      TYPE(txios(domain)) , INTENT(IN) :: domain_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref_
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: iend_
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: jend_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: latvalue_(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: lonvalue_(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask_(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      INTEGER  , OPTIONAL, INTENT(IN) :: ni_
      INTEGER  , OPTIONAL, INTENT(IN) :: ni_glo_
      INTEGER  , OPTIONAL, INTENT(IN) :: nj_
      INTEGER  , OPTIONAL, INTENT(IN) :: nj_glo_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ibegin_loc_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_jbegin_loc_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_ni_loc_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_nj_loc_
      
      IF (PRESENT(data_dim_)) THEN
        CALL cxios_set_domain_data_dim(domain_hdl%daddr, data_dim_)
      ENDIF
      
      IF (PRESENT(data_i_index_)) THEN
        CALL cxios_set_domain_data_i_index(domain_hdl%daddr, data_i_index_,size(data_i_index_,1))
      ENDIF
      
      IF (PRESENT(data_ibegin_)) THEN
        CALL cxios_set_domain_data_ibegin(domain_hdl%daddr, data_ibegin_)
      ENDIF
      
      IF (PRESENT(data_j_index_)) THEN
        CALL cxios_set_domain_data_j_index(domain_hdl%daddr, data_j_index_,size(data_j_index_,1))
      ENDIF
      
      IF (PRESENT(data_jbegin_)) THEN
        CALL cxios_set_domain_data_jbegin(domain_hdl%daddr, data_jbegin_)
      ENDIF
      
      IF (PRESENT(data_n_index_)) THEN
        CALL cxios_set_domain_data_n_index(domain_hdl%daddr, data_n_index_)
      ENDIF
      
      IF (PRESENT(data_ni_)) THEN
        CALL cxios_set_domain_data_ni(domain_hdl%daddr, data_ni_)
      ENDIF
      
      IF (PRESENT(data_nj_)) THEN
        CALL cxios_set_domain_data_nj(domain_hdl%daddr, data_nj_)
      ENDIF
      
      IF (PRESENT(domain_group_ref_)) THEN
        CALL cxios_set_domain_domain_group_ref(domain_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      ENDIF
      
      IF (PRESENT(ibegin_)) THEN
        CALL cxios_set_domain_ibegin(domain_hdl%daddr, ibegin_)
      ENDIF
      
      IF (PRESENT(iend_)) THEN
        CALL cxios_set_domain_iend(domain_hdl%daddr, iend_)
      ENDIF
      
      IF (PRESENT(jbegin_)) THEN
        CALL cxios_set_domain_jbegin(domain_hdl%daddr, jbegin_)
      ENDIF
      
      IF (PRESENT(jend_)) THEN
        CALL cxios_set_domain_jend(domain_hdl%daddr, jend_)
      ENDIF
      
      IF (PRESENT(latvalue_)) THEN
        CALL cxios_set_domain_latvalue(domain_hdl%daddr, latvalue_,size(latvalue_,1))
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_domain_long_name(domain_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(lonvalue_)) THEN
        CALL cxios_set_domain_lonvalue(domain_hdl%daddr, lonvalue_,size(lonvalue_,1))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2)))
        mask__tmp=mask_
        CALL cxios_set_domain_mask(domain_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_domain_name(domain_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(ni_)) THEN
        CALL cxios_set_domain_ni(domain_hdl%daddr, ni_)
      ENDIF
      
      IF (PRESENT(ni_glo_)) THEN
        CALL cxios_set_domain_ni_glo(domain_hdl%daddr, ni_glo_)
      ENDIF
      
      IF (PRESENT(nj_)) THEN
        CALL cxios_set_domain_nj(domain_hdl%daddr, nj_)
      ENDIF
      
      IF (PRESENT(nj_glo_)) THEN
        CALL cxios_set_domain_nj_glo(domain_hdl%daddr, nj_glo_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_domain_standard_name(domain_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(zoom_ibegin_)) THEN
        CALL cxios_set_domain_zoom_ibegin(domain_hdl%daddr, zoom_ibegin_)
      ENDIF
      
      IF (PRESENT(zoom_ibegin_loc_)) THEN
        CALL cxios_set_domain_zoom_ibegin_loc(domain_hdl%daddr, zoom_ibegin_loc_)
      ENDIF
      
      IF (PRESENT(zoom_jbegin_)) THEN
        CALL cxios_set_domain_zoom_jbegin(domain_hdl%daddr, zoom_jbegin_)
      ENDIF
      
      IF (PRESENT(zoom_jbegin_loc_)) THEN
        CALL cxios_set_domain_zoom_jbegin_loc(domain_hdl%daddr, zoom_jbegin_loc_)
      ENDIF
      
      IF (PRESENT(zoom_ni_)) THEN
        CALL cxios_set_domain_zoom_ni(domain_hdl%daddr, zoom_ni_)
      ENDIF
      
      IF (PRESENT(zoom_ni_loc_)) THEN
        CALL cxios_set_domain_zoom_ni_loc(domain_hdl%daddr, zoom_ni_loc_)
      ENDIF
      
      IF (PRESENT(zoom_nj_)) THEN
        CALL cxios_set_domain_zoom_nj(domain_hdl%daddr, zoom_nj_)
      ENDIF
      
      IF (PRESENT(zoom_nj_loc_)) THEN
        CALL cxios_set_domain_zoom_nj_loc(domain_hdl%daddr, zoom_nj_loc_)
      ENDIF
      
      
    
  END SUBROUTINE xios(set_domain_attr_hdl_)
  
  SUBROUTINE xios(get_domain_attr)  &
    ( domain_id, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
    , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
    , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
    , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
    IMPLICIT NONE
      TYPE(txios(domain))  :: domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::domain_id
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: iend
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: jend
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: latvalue(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: lonvalue(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni_glo
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj_loc
      
      CALL xios(get_domain_handle)(domain_id,domain_hdl)
      CALL xios(get_domain_attr_hdl_)   &
      ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
      , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
      , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
      , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
  END SUBROUTINE xios(get_domain_attr)
  
  SUBROUTINE xios(get_domain_attr_hdl)  &
    ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
    , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
    , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
    , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
    IMPLICIT NONE
      TYPE(txios(domain)) , INTENT(IN) :: domain_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: iend
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: jend
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: latvalue(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: lonvalue(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask_tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni_glo
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj_glo
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni_loc
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj_loc
      
      CALL xios(get_domain_attr_hdl_)  &
      ( domain_hdl, data_dim, data_i_index, data_ibegin, data_j_index, data_jbegin, data_n_index, data_ni  &
      , data_nj, domain_group_ref, ibegin, iend, jbegin, jend, latvalue, long_name, lonvalue, mask  &
      , name, ni, ni_glo, nj, nj_glo, standard_name, zoom_ibegin, zoom_ibegin_loc, zoom_jbegin, zoom_jbegin_loc  &
      , zoom_ni, zoom_ni_loc, zoom_nj, zoom_nj_loc )
    
  END SUBROUTINE xios(get_domain_attr_hdl)
  
  SUBROUTINE xios(get_domain_attr_hdl_)   &
    ( domain_hdl, data_dim_, data_i_index_, data_ibegin_, data_j_index_, data_jbegin_, data_n_index_  &
    , data_ni_, data_nj_, domain_group_ref_, ibegin_, iend_, jbegin_, jend_, latvalue_, long_name_  &
    , lonvalue_, mask_, name_, ni_, ni_glo_, nj_, nj_glo_, standard_name_, zoom_ibegin_, zoom_ibegin_loc_  &
    , zoom_jbegin_, zoom_jbegin_loc_, zoom_ni_, zoom_ni_loc_, zoom_nj_, zoom_nj_loc_ )
    
    IMPLICIT NONE
      TYPE(txios(domain)) , INTENT(IN) :: domain_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref_
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: iend_
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: jend_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: latvalue_(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: lonvalue_(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask_(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask__tmp(:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni_
      INTEGER  , OPTIONAL, INTENT(OUT) :: ni_glo_
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj_
      INTEGER  , OPTIONAL, INTENT(OUT) :: nj_glo_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ibegin_loc_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_jbegin_loc_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_ni_loc_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_nj_loc_
      
      IF (PRESENT(data_dim_)) THEN
        CALL cxios_get_domain_data_dim(domain_hdl%daddr, data_dim_)
      ENDIF
      
      IF (PRESENT(data_i_index_)) THEN
        CALL cxios_get_domain_data_i_index(domain_hdl%daddr, data_i_index_,size(data_i_index_,1))
      ENDIF
      
      IF (PRESENT(data_ibegin_)) THEN
        CALL cxios_get_domain_data_ibegin(domain_hdl%daddr, data_ibegin_)
      ENDIF
      
      IF (PRESENT(data_j_index_)) THEN
        CALL cxios_get_domain_data_j_index(domain_hdl%daddr, data_j_index_,size(data_j_index_,1))
      ENDIF
      
      IF (PRESENT(data_jbegin_)) THEN
        CALL cxios_get_domain_data_jbegin(domain_hdl%daddr, data_jbegin_)
      ENDIF
      
      IF (PRESENT(data_n_index_)) THEN
        CALL cxios_get_domain_data_n_index(domain_hdl%daddr, data_n_index_)
      ENDIF
      
      IF (PRESENT(data_ni_)) THEN
        CALL cxios_get_domain_data_ni(domain_hdl%daddr, data_ni_)
      ENDIF
      
      IF (PRESENT(data_nj_)) THEN
        CALL cxios_get_domain_data_nj(domain_hdl%daddr, data_nj_)
      ENDIF
      
      IF (PRESENT(domain_group_ref_)) THEN
        CALL cxios_get_domain_domain_group_ref(domain_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      ENDIF
      
      IF (PRESENT(ibegin_)) THEN
        CALL cxios_get_domain_ibegin(domain_hdl%daddr, ibegin_)
      ENDIF
      
      IF (PRESENT(iend_)) THEN
        CALL cxios_get_domain_iend(domain_hdl%daddr, iend_)
      ENDIF
      
      IF (PRESENT(jbegin_)) THEN
        CALL cxios_get_domain_jbegin(domain_hdl%daddr, jbegin_)
      ENDIF
      
      IF (PRESENT(jend_)) THEN
        CALL cxios_get_domain_jend(domain_hdl%daddr, jend_)
      ENDIF
      
      IF (PRESENT(latvalue_)) THEN
        CALL cxios_get_domain_latvalue(domain_hdl%daddr, latvalue_,size(latvalue_,1))
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_domain_long_name(domain_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(lonvalue_)) THEN
        CALL cxios_get_domain_lonvalue(domain_hdl%daddr, lonvalue_,size(lonvalue_,1))
      ENDIF
      
      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1),size(mask_,2)))
        CALL cxios_get_domain_mask(domain_hdl%daddr, mask__tmp,size(mask_,1),size(mask_,2))
        mask_=mask__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_domain_name(domain_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(ni_)) THEN
        CALL cxios_get_domain_ni(domain_hdl%daddr, ni_)
      ENDIF
      
      IF (PRESENT(ni_glo_)) THEN
        CALL cxios_get_domain_ni_glo(domain_hdl%daddr, ni_glo_)
      ENDIF
      
      IF (PRESENT(nj_)) THEN
        CALL cxios_get_domain_nj(domain_hdl%daddr, nj_)
      ENDIF
      
      IF (PRESENT(nj_glo_)) THEN
        CALL cxios_get_domain_nj_glo(domain_hdl%daddr, nj_glo_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_domain_standard_name(domain_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(zoom_ibegin_)) THEN
        CALL cxios_get_domain_zoom_ibegin(domain_hdl%daddr, zoom_ibegin_)
      ENDIF
      
      IF (PRESENT(zoom_ibegin_loc_)) THEN
        CALL cxios_get_domain_zoom_ibegin_loc(domain_hdl%daddr, zoom_ibegin_loc_)
      ENDIF
      
      IF (PRESENT(zoom_jbegin_)) THEN
        CALL cxios_get_domain_zoom_jbegin(domain_hdl%daddr, zoom_jbegin_)
      ENDIF
      
      IF (PRESENT(zoom_jbegin_loc_)) THEN
        CALL cxios_get_domain_zoom_jbegin_loc(domain_hdl%daddr, zoom_jbegin_loc_)
      ENDIF
      
      IF (PRESENT(zoom_ni_)) THEN
        CALL cxios_get_domain_zoom_ni(domain_hdl%daddr, zoom_ni_)
      ENDIF
      
      IF (PRESENT(zoom_ni_loc_)) THEN
        CALL cxios_get_domain_zoom_ni_loc(domain_hdl%daddr, zoom_ni_loc_)
      ENDIF
      
      IF (PRESENT(zoom_nj_)) THEN
        CALL cxios_get_domain_zoom_nj(domain_hdl%daddr, zoom_nj_)
      ENDIF
      
      IF (PRESENT(zoom_nj_loc_)) THEN
        CALL cxios_get_domain_zoom_nj_loc(domain_hdl%daddr, zoom_nj_loc_)
      ENDIF
      
      
    
  END SUBROUTINE xios(get_domain_attr_hdl_)
  
END MODULE idomain_attr
