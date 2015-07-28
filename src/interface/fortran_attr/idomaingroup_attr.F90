! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE idomaingroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE idomain
  USE domaingroup_interface_attr

CONTAINS

  SUBROUTINE xios(set_domaingroup_attr)  &
    ( domaingroup_id, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup))  :: domaingroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::domaingroup_id
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: area(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lat(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lon(:,:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin
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
      INTEGER  , OPTIONAL, INTENT(IN) :: nvertex
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(get_domaingroup_handle)(domaingroup_id,domaingroup_hdl)
      CALL xios(set_domaingroup_attr_hdl_)   &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(set_domaingroup_attr)

  SUBROUTINE xios(set_domaingroup_attr_hdl)  &
    ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: area(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lat(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lon(:,:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      INTEGER  , OPTIONAL, INTENT(IN) :: i_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin
      INTEGER  , OPTIONAL, INTENT(IN) :: j_index(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin
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
      INTEGER  , OPTIONAL, INTENT(IN) :: nvertex
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(set_domaingroup_attr_hdl_)  &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(set_domaingroup_attr_hdl)

  SUBROUTINE xios(set_domaingroup_attr_hdl_)   &
    ( domaingroup_hdl, area_, bounds_lat_, bounds_lon_, data_dim_, data_i_index_, data_ibegin_, data_j_index_  &
    , data_jbegin_, data_n_index_, data_ni_, data_nj_, domain_group_ref_, domain_ref_, group_ref_  &
    , i_index_, ibegin_, j_index_, jbegin_, latvalue_, long_name_, lonvalue_, mask_, name_, ni_  &
    , ni_glo_, nj_, nj_glo_, nvertex_, standard_name_, type_ )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: area_(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lat_(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: bounds_lon_(:,:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_dim_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_i_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_j_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: data_jbegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_n_index_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_ni_
      INTEGER  , OPTIONAL, INTENT(IN) :: data_nj_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      INTEGER  , OPTIONAL, INTENT(IN) :: i_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: ibegin_
      INTEGER  , OPTIONAL, INTENT(IN) :: j_index_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: jbegin_
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
      INTEGER  , OPTIONAL, INTENT(IN) :: nvertex_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(area_)) THEN
        CALL cxios_set_domaingroup_area(domaingroup_hdl%daddr, area_, size(area_,1), size(area_,2))
      ENDIF

      IF (PRESENT(bounds_lat_)) THEN
        CALL cxios_set_domaingroup_bounds_lat(domaingroup_hdl%daddr, bounds_lat_, size(bounds_lat_,1), size(bounds_lat_,2))
      ENDIF

      IF (PRESENT(bounds_lon_)) THEN
        CALL cxios_set_domaingroup_bounds_lon(domaingroup_hdl%daddr, bounds_lon_, size(bounds_lon_,1), size(bounds_lon_,2))
      ENDIF

      IF (PRESENT(data_dim_)) THEN
        CALL cxios_set_domaingroup_data_dim(domaingroup_hdl%daddr, data_dim_)
      ENDIF

      IF (PRESENT(data_i_index_)) THEN
        CALL cxios_set_domaingroup_data_i_index(domaingroup_hdl%daddr, data_i_index_, size(data_i_index_,1))
      ENDIF

      IF (PRESENT(data_ibegin_)) THEN
        CALL cxios_set_domaingroup_data_ibegin(domaingroup_hdl%daddr, data_ibegin_)
      ENDIF

      IF (PRESENT(data_j_index_)) THEN
        CALL cxios_set_domaingroup_data_j_index(domaingroup_hdl%daddr, data_j_index_, size(data_j_index_,1))
      ENDIF

      IF (PRESENT(data_jbegin_)) THEN
        CALL cxios_set_domaingroup_data_jbegin(domaingroup_hdl%daddr, data_jbegin_)
      ENDIF

      IF (PRESENT(data_n_index_)) THEN
        CALL cxios_set_domaingroup_data_n_index(domaingroup_hdl%daddr, data_n_index_)
      ENDIF

      IF (PRESENT(data_ni_)) THEN
        CALL cxios_set_domaingroup_data_ni(domaingroup_hdl%daddr, data_ni_)
      ENDIF

      IF (PRESENT(data_nj_)) THEN
        CALL cxios_set_domaingroup_data_nj(domaingroup_hdl%daddr, data_nj_)
      ENDIF

      IF (PRESENT(domain_group_ref_)) THEN
        CALL cxios_set_domaingroup_domain_group_ref(domaingroup_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_set_domaingroup_domain_ref(domaingroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_domaingroup_group_ref(domaingroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(i_index_)) THEN
        CALL cxios_set_domaingroup_i_index(domaingroup_hdl%daddr, i_index_, size(i_index_,1))
      ENDIF

      IF (PRESENT(ibegin_)) THEN
        CALL cxios_set_domaingroup_ibegin(domaingroup_hdl%daddr, ibegin_)
      ENDIF

      IF (PRESENT(j_index_)) THEN
        CALL cxios_set_domaingroup_j_index(domaingroup_hdl%daddr, j_index_, size(j_index_,1))
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        CALL cxios_set_domaingroup_jbegin(domaingroup_hdl%daddr, jbegin_)
      ENDIF

      IF (PRESENT(latvalue_)) THEN
        CALL cxios_set_domaingroup_latvalue(domaingroup_hdl%daddr, latvalue_, size(latvalue_,1))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_domaingroup_long_name(domaingroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(lonvalue_)) THEN
        CALL cxios_set_domaingroup_lonvalue(domaingroup_hdl%daddr, lonvalue_, size(lonvalue_,1))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1), size(mask_,2)))
        mask__tmp = mask_
        CALL cxios_set_domaingroup_mask(domaingroup_hdl%daddr, mask__tmp, size(mask_,1), size(mask_,2))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_set_domaingroup_name(domaingroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(ni_)) THEN
        CALL cxios_set_domaingroup_ni(domaingroup_hdl%daddr, ni_)
      ENDIF

      IF (PRESENT(ni_glo_)) THEN
        CALL cxios_set_domaingroup_ni_glo(domaingroup_hdl%daddr, ni_glo_)
      ENDIF

      IF (PRESENT(nj_)) THEN
        CALL cxios_set_domaingroup_nj(domaingroup_hdl%daddr, nj_)
      ENDIF

      IF (PRESENT(nj_glo_)) THEN
        CALL cxios_set_domaingroup_nj_glo(domaingroup_hdl%daddr, nj_glo_)
      ENDIF

      IF (PRESENT(nvertex_)) THEN
        CALL cxios_set_domaingroup_nvertex(domaingroup_hdl%daddr, nvertex_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_domaingroup_standard_name(domaingroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_domaingroup_type(domaingroup_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(set_domaingroup_attr_hdl_)

  SUBROUTINE xios(get_domaingroup_attr)  &
    ( domaingroup_id, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup))  :: domaingroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::domaingroup_id
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: area(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lat(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lon(:,:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin
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
      INTEGER  , OPTIONAL, INTENT(OUT) :: nvertex
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_domaingroup_handle)(domaingroup_id,domaingroup_hdl)
      CALL xios(get_domaingroup_attr_hdl_)   &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(get_domaingroup_attr)

  SUBROUTINE xios(get_domaingroup_attr_hdl)  &
    ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: area(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lat(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lon(:,:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      INTEGER  , OPTIONAL, INTENT(OUT) :: i_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin
      INTEGER  , OPTIONAL, INTENT(OUT) :: j_index(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin
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
      INTEGER  , OPTIONAL, INTENT(OUT) :: nvertex
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_domaingroup_attr_hdl_)  &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(get_domaingroup_attr_hdl)

  SUBROUTINE xios(get_domaingroup_attr_hdl_)   &
    ( domaingroup_hdl, area_, bounds_lat_, bounds_lon_, data_dim_, data_i_index_, data_ibegin_, data_j_index_  &
    , data_jbegin_, data_n_index_, data_ni_, data_nj_, domain_group_ref_, domain_ref_, group_ref_  &
    , i_index_, ibegin_, j_index_, jbegin_, latvalue_, long_name_, lonvalue_, mask_, name_, ni_  &
    , ni_glo_, nj_, nj_glo_, nvertex_, standard_name_, type_ )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: area_(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lat_(:,:)
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: bounds_lon_(:,:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_dim_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_i_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_j_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_jbegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_n_index_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_ni_
      INTEGER  , OPTIONAL, INTENT(OUT) :: data_nj_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: domain_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      INTEGER  , OPTIONAL, INTENT(OUT) :: i_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: ibegin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: j_index_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: jbegin_
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
      INTEGER  , OPTIONAL, INTENT(OUT) :: nvertex_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(area_)) THEN
        CALL cxios_get_domaingroup_area(domaingroup_hdl%daddr, area_, size(area_,1), size(area_,2))
      ENDIF

      IF (PRESENT(bounds_lat_)) THEN
        CALL cxios_get_domaingroup_bounds_lat(domaingroup_hdl%daddr, bounds_lat_, size(bounds_lat_,1), size(bounds_lat_,2))
      ENDIF

      IF (PRESENT(bounds_lon_)) THEN
        CALL cxios_get_domaingroup_bounds_lon(domaingroup_hdl%daddr, bounds_lon_, size(bounds_lon_,1), size(bounds_lon_,2))
      ENDIF

      IF (PRESENT(data_dim_)) THEN
        CALL cxios_get_domaingroup_data_dim(domaingroup_hdl%daddr, data_dim_)
      ENDIF

      IF (PRESENT(data_i_index_)) THEN
        CALL cxios_get_domaingroup_data_i_index(domaingroup_hdl%daddr, data_i_index_, size(data_i_index_,1))
      ENDIF

      IF (PRESENT(data_ibegin_)) THEN
        CALL cxios_get_domaingroup_data_ibegin(domaingroup_hdl%daddr, data_ibegin_)
      ENDIF

      IF (PRESENT(data_j_index_)) THEN
        CALL cxios_get_domaingroup_data_j_index(domaingroup_hdl%daddr, data_j_index_, size(data_j_index_,1))
      ENDIF

      IF (PRESENT(data_jbegin_)) THEN
        CALL cxios_get_domaingroup_data_jbegin(domaingroup_hdl%daddr, data_jbegin_)
      ENDIF

      IF (PRESENT(data_n_index_)) THEN
        CALL cxios_get_domaingroup_data_n_index(domaingroup_hdl%daddr, data_n_index_)
      ENDIF

      IF (PRESENT(data_ni_)) THEN
        CALL cxios_get_domaingroup_data_ni(domaingroup_hdl%daddr, data_ni_)
      ENDIF

      IF (PRESENT(data_nj_)) THEN
        CALL cxios_get_domaingroup_data_nj(domaingroup_hdl%daddr, data_nj_)
      ENDIF

      IF (PRESENT(domain_group_ref_)) THEN
        CALL cxios_get_domaingroup_domain_group_ref(domaingroup_hdl%daddr, domain_group_ref_, len(domain_group_ref_))
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        CALL cxios_get_domaingroup_domain_ref(domaingroup_hdl%daddr, domain_ref_, len(domain_ref_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_domaingroup_group_ref(domaingroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(i_index_)) THEN
        CALL cxios_get_domaingroup_i_index(domaingroup_hdl%daddr, i_index_, size(i_index_,1))
      ENDIF

      IF (PRESENT(ibegin_)) THEN
        CALL cxios_get_domaingroup_ibegin(domaingroup_hdl%daddr, ibegin_)
      ENDIF

      IF (PRESENT(j_index_)) THEN
        CALL cxios_get_domaingroup_j_index(domaingroup_hdl%daddr, j_index_, size(j_index_,1))
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        CALL cxios_get_domaingroup_jbegin(domaingroup_hdl%daddr, jbegin_)
      ENDIF

      IF (PRESENT(latvalue_)) THEN
        CALL cxios_get_domaingroup_latvalue(domaingroup_hdl%daddr, latvalue_, size(latvalue_,1))
      ENDIF

      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_domaingroup_long_name(domaingroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF

      IF (PRESENT(lonvalue_)) THEN
        CALL cxios_get_domaingroup_lonvalue(domaingroup_hdl%daddr, lonvalue_, size(lonvalue_,1))
      ENDIF

      IF (PRESENT(mask_)) THEN
        ALLOCATE(mask__tmp(size(mask_,1), size(mask_,2)))
        CALL cxios_get_domaingroup_mask(domaingroup_hdl%daddr, mask__tmp, size(mask_,1), size(mask_,2))
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_get_domaingroup_name(domaingroup_hdl%daddr, name_, len(name_))
      ENDIF

      IF (PRESENT(ni_)) THEN
        CALL cxios_get_domaingroup_ni(domaingroup_hdl%daddr, ni_)
      ENDIF

      IF (PRESENT(ni_glo_)) THEN
        CALL cxios_get_domaingroup_ni_glo(domaingroup_hdl%daddr, ni_glo_)
      ENDIF

      IF (PRESENT(nj_)) THEN
        CALL cxios_get_domaingroup_nj(domaingroup_hdl%daddr, nj_)
      ENDIF

      IF (PRESENT(nj_glo_)) THEN
        CALL cxios_get_domaingroup_nj_glo(domaingroup_hdl%daddr, nj_glo_)
      ENDIF

      IF (PRESENT(nvertex_)) THEN
        CALL cxios_get_domaingroup_nvertex(domaingroup_hdl%daddr, nvertex_)
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_domaingroup_standard_name(domaingroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_domaingroup_type(domaingroup_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(get_domaingroup_attr_hdl_)

  SUBROUTINE xios(is_defined_domaingroup_attr)  &
    ( domaingroup_id, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup))  :: domaingroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::domaingroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: area
      LOGICAL(KIND=C_BOOL) :: area_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lat
      LOGICAL(KIND=C_BOOL) :: bounds_lat_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lon
      LOGICAL(KIND=C_BOOL) :: bounds_lon_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_dim
      LOGICAL(KIND=C_BOOL) :: data_dim_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_i_index
      LOGICAL(KIND=C_BOOL) :: data_i_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ibegin
      LOGICAL(KIND=C_BOOL) :: data_ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_j_index
      LOGICAL(KIND=C_BOOL) :: data_j_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_jbegin
      LOGICAL(KIND=C_BOOL) :: data_jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n_index
      LOGICAL(KIND=C_BOOL) :: data_n_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ni
      LOGICAL(KIND=C_BOOL) :: data_ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_nj
      LOGICAL(KIND=C_BOOL) :: data_nj_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_group_ref
      LOGICAL(KIND=C_BOOL) :: domain_group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: i_index
      LOGICAL(KIND=C_BOOL) :: i_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin
      LOGICAL(KIND=C_BOOL) :: ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: j_index
      LOGICAL(KIND=C_BOOL) :: j_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin
      LOGICAL(KIND=C_BOOL) :: jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: latvalue
      LOGICAL(KIND=C_BOOL) :: latvalue_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: lonvalue
      LOGICAL(KIND=C_BOOL) :: lonvalue_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni
      LOGICAL(KIND=C_BOOL) :: ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni_glo
      LOGICAL(KIND=C_BOOL) :: ni_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj
      LOGICAL(KIND=C_BOOL) :: nj_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj_glo
      LOGICAL(KIND=C_BOOL) :: nj_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nvertex
      LOGICAL(KIND=C_BOOL) :: nvertex_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(get_domaingroup_handle)(domaingroup_id,domaingroup_hdl)
      CALL xios(is_defined_domaingroup_attr_hdl_)   &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(is_defined_domaingroup_attr)

  SUBROUTINE xios(is_defined_domaingroup_attr_hdl)  &
    ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
    , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
    , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
    , nvertex, standard_name, type )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: area
      LOGICAL(KIND=C_BOOL) :: area_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lat
      LOGICAL(KIND=C_BOOL) :: bounds_lat_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lon
      LOGICAL(KIND=C_BOOL) :: bounds_lon_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_dim
      LOGICAL(KIND=C_BOOL) :: data_dim_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_i_index
      LOGICAL(KIND=C_BOOL) :: data_i_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ibegin
      LOGICAL(KIND=C_BOOL) :: data_ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_j_index
      LOGICAL(KIND=C_BOOL) :: data_j_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_jbegin
      LOGICAL(KIND=C_BOOL) :: data_jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n_index
      LOGICAL(KIND=C_BOOL) :: data_n_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ni
      LOGICAL(KIND=C_BOOL) :: data_ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_nj
      LOGICAL(KIND=C_BOOL) :: data_nj_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_group_ref
      LOGICAL(KIND=C_BOOL) :: domain_group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref
      LOGICAL(KIND=C_BOOL) :: domain_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: i_index
      LOGICAL(KIND=C_BOOL) :: i_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin
      LOGICAL(KIND=C_BOOL) :: ibegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: j_index
      LOGICAL(KIND=C_BOOL) :: j_index_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin
      LOGICAL(KIND=C_BOOL) :: jbegin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: latvalue
      LOGICAL(KIND=C_BOOL) :: latvalue_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name
      LOGICAL(KIND=C_BOOL) :: long_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: lonvalue
      LOGICAL(KIND=C_BOOL) :: lonvalue_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask
      LOGICAL(KIND=C_BOOL) :: mask_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni
      LOGICAL(KIND=C_BOOL) :: ni_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni_glo
      LOGICAL(KIND=C_BOOL) :: ni_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj
      LOGICAL(KIND=C_BOOL) :: nj_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj_glo
      LOGICAL(KIND=C_BOOL) :: nj_glo_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nvertex
      LOGICAL(KIND=C_BOOL) :: nvertex_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name
      LOGICAL(KIND=C_BOOL) :: standard_name_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(is_defined_domaingroup_attr_hdl_)  &
      ( domaingroup_hdl, area, bounds_lat, bounds_lon, data_dim, data_i_index, data_ibegin, data_j_index  &
      , data_jbegin, data_n_index, data_ni, data_nj, domain_group_ref, domain_ref, group_ref, i_index  &
      , ibegin, j_index, jbegin, latvalue, long_name, lonvalue, mask, name, ni, ni_glo, nj, nj_glo  &
      , nvertex, standard_name, type )

  END SUBROUTINE xios(is_defined_domaingroup_attr_hdl)

  SUBROUTINE xios(is_defined_domaingroup_attr_hdl_)   &
    ( domaingroup_hdl, area_, bounds_lat_, bounds_lon_, data_dim_, data_i_index_, data_ibegin_, data_j_index_  &
    , data_jbegin_, data_n_index_, data_ni_, data_nj_, domain_group_ref_, domain_ref_, group_ref_  &
    , i_index_, ibegin_, j_index_, jbegin_, latvalue_, long_name_, lonvalue_, mask_, name_, ni_  &
    , ni_glo_, nj_, nj_glo_, nvertex_, standard_name_, type_ )

    IMPLICIT NONE
      TYPE(txios(domaingroup)) , INTENT(IN) :: domaingroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: area_
      LOGICAL(KIND=C_BOOL) :: area__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lat_
      LOGICAL(KIND=C_BOOL) :: bounds_lat__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: bounds_lon_
      LOGICAL(KIND=C_BOOL) :: bounds_lon__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_dim_
      LOGICAL(KIND=C_BOOL) :: data_dim__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_i_index_
      LOGICAL(KIND=C_BOOL) :: data_i_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ibegin_
      LOGICAL(KIND=C_BOOL) :: data_ibegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_j_index_
      LOGICAL(KIND=C_BOOL) :: data_j_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_jbegin_
      LOGICAL(KIND=C_BOOL) :: data_jbegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_n_index_
      LOGICAL(KIND=C_BOOL) :: data_n_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_ni_
      LOGICAL(KIND=C_BOOL) :: data_ni__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: data_nj_
      LOGICAL(KIND=C_BOOL) :: data_nj__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_group_ref_
      LOGICAL(KIND=C_BOOL) :: domain_group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: domain_ref_
      LOGICAL(KIND=C_BOOL) :: domain_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: i_index_
      LOGICAL(KIND=C_BOOL) :: i_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ibegin_
      LOGICAL(KIND=C_BOOL) :: ibegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: j_index_
      LOGICAL(KIND=C_BOOL) :: j_index__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: jbegin_
      LOGICAL(KIND=C_BOOL) :: jbegin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: latvalue_
      LOGICAL(KIND=C_BOOL) :: latvalue__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: long_name_
      LOGICAL(KIND=C_BOOL) :: long_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: lonvalue_
      LOGICAL(KIND=C_BOOL) :: lonvalue__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask_
      LOGICAL(KIND=C_BOOL) :: mask__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni_
      LOGICAL(KIND=C_BOOL) :: ni__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: ni_glo_
      LOGICAL(KIND=C_BOOL) :: ni_glo__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj_
      LOGICAL(KIND=C_BOOL) :: nj__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nj_glo_
      LOGICAL(KIND=C_BOOL) :: nj_glo__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: nvertex_
      LOGICAL(KIND=C_BOOL) :: nvertex__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: standard_name_
      LOGICAL(KIND=C_BOOL) :: standard_name__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(area_)) THEN
        area__tmp = cxios_is_defined_domaingroup_area(domaingroup_hdl%daddr)
        area_ = area__tmp
      ENDIF

      IF (PRESENT(bounds_lat_)) THEN
        bounds_lat__tmp = cxios_is_defined_domaingroup_bounds_lat(domaingroup_hdl%daddr)
        bounds_lat_ = bounds_lat__tmp
      ENDIF

      IF (PRESENT(bounds_lon_)) THEN
        bounds_lon__tmp = cxios_is_defined_domaingroup_bounds_lon(domaingroup_hdl%daddr)
        bounds_lon_ = bounds_lon__tmp
      ENDIF

      IF (PRESENT(data_dim_)) THEN
        data_dim__tmp = cxios_is_defined_domaingroup_data_dim(domaingroup_hdl%daddr)
        data_dim_ = data_dim__tmp
      ENDIF

      IF (PRESENT(data_i_index_)) THEN
        data_i_index__tmp = cxios_is_defined_domaingroup_data_i_index(domaingroup_hdl%daddr)
        data_i_index_ = data_i_index__tmp
      ENDIF

      IF (PRESENT(data_ibegin_)) THEN
        data_ibegin__tmp = cxios_is_defined_domaingroup_data_ibegin(domaingroup_hdl%daddr)
        data_ibegin_ = data_ibegin__tmp
      ENDIF

      IF (PRESENT(data_j_index_)) THEN
        data_j_index__tmp = cxios_is_defined_domaingroup_data_j_index(domaingroup_hdl%daddr)
        data_j_index_ = data_j_index__tmp
      ENDIF

      IF (PRESENT(data_jbegin_)) THEN
        data_jbegin__tmp = cxios_is_defined_domaingroup_data_jbegin(domaingroup_hdl%daddr)
        data_jbegin_ = data_jbegin__tmp
      ENDIF

      IF (PRESENT(data_n_index_)) THEN
        data_n_index__tmp = cxios_is_defined_domaingroup_data_n_index(domaingroup_hdl%daddr)
        data_n_index_ = data_n_index__tmp
      ENDIF

      IF (PRESENT(data_ni_)) THEN
        data_ni__tmp = cxios_is_defined_domaingroup_data_ni(domaingroup_hdl%daddr)
        data_ni_ = data_ni__tmp
      ENDIF

      IF (PRESENT(data_nj_)) THEN
        data_nj__tmp = cxios_is_defined_domaingroup_data_nj(domaingroup_hdl%daddr)
        data_nj_ = data_nj__tmp
      ENDIF

      IF (PRESENT(domain_group_ref_)) THEN
        domain_group_ref__tmp = cxios_is_defined_domaingroup_domain_group_ref(domaingroup_hdl%daddr)
        domain_group_ref_ = domain_group_ref__tmp
      ENDIF

      IF (PRESENT(domain_ref_)) THEN
        domain_ref__tmp = cxios_is_defined_domaingroup_domain_ref(domaingroup_hdl%daddr)
        domain_ref_ = domain_ref__tmp
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp = cxios_is_defined_domaingroup_group_ref(domaingroup_hdl%daddr)
        group_ref_ = group_ref__tmp
      ENDIF

      IF (PRESENT(i_index_)) THEN
        i_index__tmp = cxios_is_defined_domaingroup_i_index(domaingroup_hdl%daddr)
        i_index_ = i_index__tmp
      ENDIF

      IF (PRESENT(ibegin_)) THEN
        ibegin__tmp = cxios_is_defined_domaingroup_ibegin(domaingroup_hdl%daddr)
        ibegin_ = ibegin__tmp
      ENDIF

      IF (PRESENT(j_index_)) THEN
        j_index__tmp = cxios_is_defined_domaingroup_j_index(domaingroup_hdl%daddr)
        j_index_ = j_index__tmp
      ENDIF

      IF (PRESENT(jbegin_)) THEN
        jbegin__tmp = cxios_is_defined_domaingroup_jbegin(domaingroup_hdl%daddr)
        jbegin_ = jbegin__tmp
      ENDIF

      IF (PRESENT(latvalue_)) THEN
        latvalue__tmp = cxios_is_defined_domaingroup_latvalue(domaingroup_hdl%daddr)
        latvalue_ = latvalue__tmp
      ENDIF

      IF (PRESENT(long_name_)) THEN
        long_name__tmp = cxios_is_defined_domaingroup_long_name(domaingroup_hdl%daddr)
        long_name_ = long_name__tmp
      ENDIF

      IF (PRESENT(lonvalue_)) THEN
        lonvalue__tmp = cxios_is_defined_domaingroup_lonvalue(domaingroup_hdl%daddr)
        lonvalue_ = lonvalue__tmp
      ENDIF

      IF (PRESENT(mask_)) THEN
        mask__tmp = cxios_is_defined_domaingroup_mask(domaingroup_hdl%daddr)
        mask_ = mask__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_domaingroup_name(domaingroup_hdl%daddr)
        name_ = name__tmp
      ENDIF

      IF (PRESENT(ni_)) THEN
        ni__tmp = cxios_is_defined_domaingroup_ni(domaingroup_hdl%daddr)
        ni_ = ni__tmp
      ENDIF

      IF (PRESENT(ni_glo_)) THEN
        ni_glo__tmp = cxios_is_defined_domaingroup_ni_glo(domaingroup_hdl%daddr)
        ni_glo_ = ni_glo__tmp
      ENDIF

      IF (PRESENT(nj_)) THEN
        nj__tmp = cxios_is_defined_domaingroup_nj(domaingroup_hdl%daddr)
        nj_ = nj__tmp
      ENDIF

      IF (PRESENT(nj_glo_)) THEN
        nj_glo__tmp = cxios_is_defined_domaingroup_nj_glo(domaingroup_hdl%daddr)
        nj_glo_ = nj_glo__tmp
      ENDIF

      IF (PRESENT(nvertex_)) THEN
        nvertex__tmp = cxios_is_defined_domaingroup_nvertex(domaingroup_hdl%daddr)
        nvertex_ = nvertex__tmp
      ENDIF

      IF (PRESENT(standard_name_)) THEN
        standard_name__tmp = cxios_is_defined_domaingroup_standard_name(domaingroup_hdl%daddr)
        standard_name_ = standard_name__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_domaingroup_type(domaingroup_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_domaingroup_attr_hdl_)

END MODULE idomaingroup_attr
