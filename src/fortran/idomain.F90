#include "xios_fortran_prefix.hpp"

MODULE IDOMAIN
   USE, INTRINSIC :: ISO_C_BINDING
   USE DOMAIN_INTERFACE
   USE DOMAINGROUP_INTERFACE
   
   TYPE txios(domain)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(domain)
   
   TYPE txios(domaingroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(domaingroup)
   
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.


   SUBROUTINE xios(set_domaingroup_attr)                                                                   &
   ( domaingroup_id, name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,       &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,                     &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,                 &
     lonvalue, latvalue)
      IMPLICIT NONE
      TYPE(txios(domaingroup))                              :: domaingroup_hdl
      CHARACTER(len = *)                       , INTENT(IN) :: domaingroup_id
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER                        , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER                        , OPTIONAL, INTENT(IN) :: nj_glo
      INTEGER                        , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: iend
      INTEGER                        , OPTIONAL, INTENT(IN) :: ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: jend
      INTEGER                        , OPTIONAL, INTENT(IN) :: nj
      LOGICAL          , dimension(*), OPTIONAL, INTENT(IN) :: mask(:,:)
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_nj
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER       , dimension(*)   , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER       , dimension(*)   , OPTIONAL, INTENT(IN) :: data_j_index(:)
      REAL(kind = 8), dimension(*)   , OPTIONAL, INTENT(IN) :: lonvalue(:)
      REAL(kind = 8), dimension(*)   , OPTIONAL, INTENT(IN) :: latvalue(:)
 
      CALL xios(get_domaingroup_handle)(domaingroup_id,domaingroup_hdl)
      CALL xios(set_domaingroup_attr_hdl_)                                                                 &
   ( domaingroup_hdl, name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,      &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,                     &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,                 &
     lonvalue, latvalue)

   END SUBROUTINE xios(set_domaingroup_attr)

   SUBROUTINE xios(set_domaingroup_attr_hdl)                                                               &   
   ( domaingroup_hdl,name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,       &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,                     &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index,                               &
     data_j_index, lonvalue, latvalue)
      IMPLICIT NONE
      TYPE(txios(domaingroup))             , INTENT(IN) :: domaingroup_hdl
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_glo
      INTEGER                          , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: iend
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: jend
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj
      LOGICAL          , dimension(*)  , OPTIONAL, INTENT(IN) :: mask(:,:)
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_nj
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_j_index(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: lonvalue(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: latvalue(:)   

      CALL xios(set_domaingroup_attr_hdl_)                                                               &
   ( domaingroup_hdl,name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,     &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,                   &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,               &
     lonvalue, latvalue)

    END SUBROUTINE xios(set_domaingroup_attr_hdl)

   SUBROUTINE xios(set_domaingroup_attr_hdl_)                                                                       &
   ( domaingroup_hdl, name_, standard_name_, long_name_, domain_group_ref_, ni_glo_, nj_glo_, ibegin_, iend_,       &
     ni_, jbegin_, jend_, nj_, mask_, data_dim_, data_ni_, data_nj_, data_ibegin_, data_jbegin_,                   &
     zoom_ni_, zoom_nj_, zoom_ibegin_, zoom_jbegin_, data_n_index_, data_i_index_, data_j_index_,                  &
     lonvalue_, latvalue_)
      IMPLICIT NONE
      TYPE(txios(domaingroup))             , INTENT(IN) :: domaingroup_hdl
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: domain_group_ref_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_glo_
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_glo_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: iend_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: jend_
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_
      LOGICAL          , dimension(*)  , OPTIONAL, INTENT(IN) :: mask_(:,:)
      LOGICAL(kind = 1), dimension(:,:), ALLOCATABLE          :: mask__! (size(mask_,1),size(mask,2))
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_dim_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_nj_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_nj_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_n_index_
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_i_index_(:)
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_j_index_(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: lonvalue_(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: latvalue_(:)
      
      IF (PRESENT(name_))             THEN
         CALL cxios_set_domaingroup_name(domaingroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))    THEN
         CALL cxios_set_domaingroup_standard_name(domaingroup_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))        THEN
         CALL cxios_set_domaingroup_long_name(domaingroup_hdl%daddr, long_name_, len(long_name_))
       END IF
      IF (PRESENT(domain_group_ref_)) THEN
         CALL cxios_set_domaingroup_domain_group_ref(domaingroup_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      END IF
      IF (PRESENT(ni_glo_))           THEN
         CALL cxios_set_domaingroup_ni_glo(domaingroup_hdl%daddr, ni_glo_)
      END IF
      IF (PRESENT(nj_glo_))           THEN
         CALL cxios_set_domaingroup_nj_glo(domaingroup_hdl%daddr, nj_glo_)
      END IF
      IF (PRESENT(ibegin_))           THEN
         CALL cxios_set_domaingroup_ibegin(domaingroup_hdl%daddr, ibegin_)
       END IF
      IF (PRESENT(iend_))             THEN
         CALL cxios_set_domaingroup_iend(domaingroup_hdl%daddr, iend_)
      END IF
      IF (PRESENT(ni_))               THEN
         CALL cxios_set_domaingroup_ni(domaingroup_hdl%daddr, ni_)
      END IF
      IF (PRESENT(jbegin_))           THEN
         CALL cxios_set_domaingroup_jbegin(domaingroup_hdl%daddr, jbegin_)
      END IF
      IF (PRESENT(jend_))             THEN
         CALL cxios_set_domaingroup_jend(domaingroup_hdl%daddr, jend_)
      END IF
      IF (PRESENT(nj_))               THEN
         CALL cxios_set_domaingroup_nj(domaingroup_hdl%daddr, nj_)
      END IF
      IF (PRESENT(mask_))             THEN
         ALLOCATE(mask__(size(mask_, 1), size(mask_, 2)))
         mask__(:,:) = mask_(:,:)
         CALL cxios_set_domaingroup_mask(domaingroup_hdl%daddr, mask__, size(mask_, 1), size(mask_, 2))         
         DEALLOCATE(mask__)
      END IF
      IF (PRESENT(data_dim_))         THEN
         CALL cxios_set_domaingroup_data_dim(domaingroup_hdl%daddr, data_dim_)
      END IF
      IF (PRESENT(data_ni_))          THEN
         CALL cxios_set_domaingroup_data_ni(domaingroup_hdl%daddr, data_ni_)
      END IF 
      IF (PRESENT(data_nj_))          THEN
         CALL cxios_set_domaingroup_data_nj(domaingroup_hdl%daddr, data_nj_)
      END IF
      IF (PRESENT(data_ibegin_))      THEN
         CALL cxios_set_domaingroup_data_ibegin(domaingroup_hdl%daddr, data_ibegin_)
      END IF
      IF (PRESENT(data_jbegin_))      THEN
         CALL cxios_set_domaingroup_data_jbegin(domaingroup_hdl%daddr, data_jbegin_)
      END IF
      IF (PRESENT(zoom_ni_))          THEN
         CALL cxios_set_domaingroup_zoom_ni(domaingroup_hdl%daddr, zoom_ni_)
      END IF
      IF (PRESENT(zoom_nj_))          THEN
       CALL cxios_set_domaingroup_zoom_nj(domaingroup_hdl%daddr, zoom_nj_)
      END IF
      IF (PRESENT(zoom_ibegin_))      THEN
         CALL cxios_set_domaingroup_zoom_ibegin(domaingroup_hdl%daddr, zoom_ibegin_)
      END IF
      IF (PRESENT(zoom_jbegin_))      THEN
         CALL cxios_set_domaingroup_zoom_jbegin(domaingroup_hdl%daddr, zoom_jbegin_)
      END IF

      IF (PRESENT(data_n_index_))     THEN
         CALL cxios_set_domaingroup_data_n_index(domaingroup_hdl%daddr, data_n_index_)
      END IF
      IF (PRESENT(data_i_index_))     THEN
         CALL cxios_set_domaingroup_data_i_index(domaingroup_hdl%daddr, data_i_index_, size(data_i_index_, 1))
      END IF
      IF (PRESENT(data_j_index_))     THEN
         CALL cxios_set_domaingroup_data_j_index(domaingroup_hdl%daddr, data_j_index_, size(data_j_index_, 1))
      END IF
      IF (PRESENT(lonvalue_))         THEN
         CALL cxios_set_domaingroup_lonvalue(domaingroup_hdl%daddr, lonvalue_, size(lonvalue_, 1))
      END IF
      IF (PRESENT(latvalue_))         THEN
         CALL cxios_set_domaingroup_latvalue(domaingroup_hdl%daddr, latvalue_, size(latvalue_, 1))
      END IF

   END SUBROUTINE xios(set_domaingroup_attr_hdl_)
   
   
   SUBROUTINE xios(set_domain_attr)                                                                &
   ( domain_id, name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,    &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,             &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,         &
     lonvalue, latvalue)
      IMPLICIT NONE
     
      TYPE(txios(domain))                                   :: domain_hdl
      CHARACTER(len = *)                       , INTENT(IN) :: domain_id
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)             , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER                        , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER                        , OPTIONAL, INTENT(IN) :: nj_glo
      INTEGER                        , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: iend
      INTEGER                        , OPTIONAL, INTENT(IN) :: ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: jend
      INTEGER                        , OPTIONAL, INTENT(IN) :: nj
      LOGICAL          , dimension(*), OPTIONAL, INTENT(IN) :: mask(:,:)
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_nj
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER                        , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER       , dimension(*)   , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER       , dimension(*)   , OPTIONAL, INTENT(IN) :: data_j_index(:)
      REAL(kind = 8), dimension(*)   , OPTIONAL, INTENT(IN) :: lonvalue(:)
      REAL(kind = 8), dimension(*)   , OPTIONAL, INTENT(IN) :: latvalue(:)
 
      CALL xios(get_domain_handle)(domain_id,domain_hdl)
      CALL xios(set_domain_attr_hdl_)                                                               &
   ( domain_hdl, name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,    &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,              &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,          &
     lonvalue, latvalue)

   END SUBROUTINE xios(set_domain_attr)


   SUBROUTINE xios(set_domain_attr_hdl)                                                            &
   ( domain_hdl,name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,    &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,             &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index,                       &
     data_j_index, lonvalue, latvalue)
      IMPLICIT NONE
     
      TYPE(txios(domain))                       , INTENT(IN) :: domain_hdl
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: domain_group_ref
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_glo
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_glo
      INTEGER                          , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: iend
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: jend
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj
      LOGICAL          , dimension(*)  , OPTIONAL, INTENT(IN) :: mask(:,:)
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_nj
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ni
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_nj
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ibegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_jbegin
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_j_index(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: lonvalue(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: latvalue(:)
      
      CALL xios(set_domain_attr_hdl_)                                                             &
   ( domain_hdl,name, standard_name, long_name, domain_group_ref, ni_glo, nj_glo, ibegin, iend,   &
     ni, jbegin, jend, nj, mask, data_dim, data_ni, data_nj, data_ibegin, data_jbegin,            &
     zoom_ni, zoom_nj, zoom_ibegin, zoom_jbegin, data_n_index, data_i_index, data_j_index,        &
     lonvalue, latvalue)
     
   END SUBROUTINE xios(set_domain_attr_hdl)
       
   SUBROUTINE xios(set_domain_attr_hdl_)                                                                 &
   ( domain_hdl,name_, standard_name_, long_name_, domain_group_ref_, ni_glo_, nj_glo_, ibegin_, iend_,  &
     ni_, jbegin_, jend_, nj_, mask_, data_dim_, data_ni_, data_nj_, data_ibegin_, data_jbegin_,         &
     zoom_ni_, zoom_nj_, zoom_ibegin_, zoom_jbegin_, data_n_index_, data_i_index_, data_j_index_,        &
     lonvalue_, latvalue_)

      IMPLICIT NONE
      TYPE(txios(domain))                       , INTENT(IN) :: domain_hdl
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)               , OPTIONAL, INTENT(IN) :: domain_group_ref_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_glo_
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_glo_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: iend_
      INTEGER                          , OPTIONAL, INTENT(IN) :: ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: jend_
      INTEGER                          , OPTIONAL, INTENT(IN) :: nj_
      LOGICAL          , dimension(*)  , OPTIONAL, INTENT(IN) :: mask_(:,:)
      LOGICAL(kind = 1), dimension(:,:), ALLOCATABLE          :: mask__
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_dim_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_nj_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ni_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_nj_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_ibegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: zoom_jbegin_
      INTEGER                          , OPTIONAL, INTENT(IN) :: data_n_index_
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_i_index_(:)
      INTEGER       , dimension(*)     , OPTIONAL, INTENT(IN) :: data_j_index_(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: lonvalue_(:)
      REAL(kind = 8), dimension(*)     , OPTIONAL, INTENT(IN) :: latvalue_(:)
      IF (PRESENT(name_))             THEN
         CALL cxios_set_domain_name(domain_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))    THEN
         CALL cxios_set_domain_standard_name(domain_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))        THEN
         CALL cxios_set_domain_long_name(domain_hdl%daddr, long_name_, len(long_name_))
       END IF
      IF (PRESENT(domain_group_ref_)) THEN
         CALL cxios_set_domain_domain_group_ref(domain_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      END IF
      IF (PRESENT(ni_glo_))           THEN
         CALL cxios_set_domain_ni_glo(domain_hdl%daddr, ni_glo_)
      END IF
      IF (PRESENT(nj_glo_))           THEN
         CALL cxios_set_domain_nj_glo(domain_hdl%daddr, nj_glo_)
      END IF
      IF (PRESENT(ibegin_))           THEN
         CALL cxios_set_domain_ibegin(domain_hdl%daddr, ibegin_)
       END IF
      IF (PRESENT(iend_))             THEN
         CALL cxios_set_domain_iend(domain_hdl%daddr, iend_)
      END IF
      IF (PRESENT(ni_))               THEN
         CALL cxios_set_domain_ni(domain_hdl%daddr, ni_)
      END IF
      IF (PRESENT(jbegin_))           THEN
         CALL cxios_set_domain_jbegin(domain_hdl%daddr, jbegin_)
      END IF
      IF (PRESENT(jend_))             THEN
         CALL cxios_set_domain_jend(domain_hdl%daddr, jend_)
      END IF
      IF (PRESENT(nj_))               THEN
         CALL cxios_set_domain_nj(domain_hdl%daddr, nj_)
      END IF
      IF (PRESENT(mask_))             THEN
         ALLOCATE(mask__(size(mask_, 1), size(mask_, 2)))
         mask__(:,:) = mask_(:,:)
         CALL cxios_set_domain_mask(domain_hdl%daddr, mask__, size(mask_, 1), size(mask_, 2))         
         DEALLOCATE(mask__)
      END IF
      IF (PRESENT(data_dim_))         THEN
         CALL cxios_set_domain_data_dim(domain_hdl%daddr, data_dim_)
      END IF
      IF (PRESENT(data_ni_))          THEN
         CALL cxios_set_domain_data_ni(domain_hdl%daddr, data_ni_)
      END IF 
      IF (PRESENT(data_nj_))          THEN
         CALL cxios_set_domain_data_nj(domain_hdl%daddr, data_nj_)
      END IF
      IF (PRESENT(data_ibegin_))      THEN
         CALL cxios_set_domain_data_ibegin(domain_hdl%daddr, data_ibegin_)
      END IF
      IF (PRESENT(data_jbegin_))      THEN
         CALL cxios_set_domain_data_jbegin(domain_hdl%daddr, data_jbegin_)
      END IF
      IF (PRESENT(zoom_ni_))          THEN
         CALL cxios_set_domain_zoom_ni(domain_hdl%daddr, zoom_ni_)
      END IF
      IF (PRESENT(zoom_nj_))          THEN
       CALL cxios_set_domain_zoom_nj(domain_hdl%daddr, zoom_nj_)
      END IF
      IF (PRESENT(zoom_ibegin_))      THEN
         CALL cxios_set_domain_zoom_ibegin(domain_hdl%daddr, zoom_ibegin_)
      END IF
      IF (PRESENT(zoom_jbegin_))      THEN
         CALL cxios_set_domain_zoom_jbegin(domain_hdl%daddr, zoom_jbegin_)
      END IF
      IF (PRESENT(data_n_index_))     THEN
         CALL cxios_set_domain_data_n_index(domain_hdl%daddr, data_n_index_)
      END IF
      IF (PRESENT(data_i_index_))     THEN
         CALL cxios_set_domain_data_i_index(domain_hdl%daddr, data_i_index_, size(data_i_index_, 1))
      END IF
      IF (PRESENT(data_j_index_))     THEN
         CALL cxios_set_domain_data_j_index(domain_hdl%daddr, data_j_index_, size(data_j_index_, 1))
      END IF
      IF (PRESENT(lonvalue_))         THEN
         CALL cxios_set_domain_lonvalue(domain_hdl%daddr, lonvalue_, size(lonvalue_, 1))
      END IF
      IF (PRESENT(latvalue_))         THEN
         CALL cxios_set_domain_latvalue(domain_hdl%daddr, latvalue_, size(latvalue_, 1))
      END IF

   END SUBROUTINE xios(set_domain_attr_hdl_)


   
   SUBROUTINE xios(get_domain_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt      
      TYPE(txios(domain)), INTENT(OUT):: ret
 
      CALL cxios_domain_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_domain_handle)
   
   SUBROUTINE xios(get_domaingroup_handle)(idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *)      , INTENT(IN) :: idt      
      TYPE(txios(domaingroup)), INTENT(OUT):: ret

      CALL cxios_domaingroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_domaingroup_handle)

   LOGICAL FUNCTION xios(is_valid_domain)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_domain_valid_id(val, idt, len(idt));
      xios(is_valid_domain) = val
   END FUNCTION  xios(is_valid_domain)

   LOGICAL FUNCTION xios(is_valid_domaingroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_domaingroup_valid_id(val, idt, len(idt));
      xios(is_valid_domaingroup) = val
   END FUNCTION  xios(is_valid_domaingroup)
   
END MODULE IDOMAIN
