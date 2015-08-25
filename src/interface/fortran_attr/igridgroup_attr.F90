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
    ( gridgroup_id, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL  , OPTIONAL, INTENT(IN) :: axis_domain_order(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask1(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1_tmp(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask2(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2_tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask3(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name

      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(set_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(set_gridgroup_attr)

  SUBROUTINE xios(set_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axis_domain_order(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask1(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1_tmp(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask2(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2_tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask3(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name

      CALL xios(set_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(set_gridgroup_attr_hdl)

  SUBROUTINE xios(set_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axis_domain_order_, description_, group_ref_, mask1_, mask2_, mask3_, name_  &
     )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: axis_domain_order_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask1_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1__tmp(:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask2_(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2__tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(IN) :: mask3_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_

      IF (PRESENT(axis_domain_order_)) THEN
        ALLOCATE(axis_domain_order__tmp(SIZE(axis_domain_order_,1)))
        axis_domain_order__tmp = axis_domain_order_
        CALL cxios_set_gridgroup_axis_domain_order(gridgroup_hdl%daddr, axis_domain_order__tmp, SHAPE(axis_domain_order_))
      ENDIF

      IF (PRESENT(description_)) THEN
        CALL cxios_set_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(mask1_)) THEN
        ALLOCATE(mask1__tmp(SIZE(mask1_,1)))
        mask1__tmp = mask1_
        CALL cxios_set_gridgroup_mask1(gridgroup_hdl%daddr, mask1__tmp, SHAPE(mask1_))
      ENDIF

      IF (PRESENT(mask2_)) THEN
        ALLOCATE(mask2__tmp(SIZE(mask2_,1), SIZE(mask2_,2)))
        mask2__tmp = mask2_
        CALL cxios_set_gridgroup_mask2(gridgroup_hdl%daddr, mask2__tmp, SHAPE(mask2_))
      ENDIF

      IF (PRESENT(mask3_)) THEN
        ALLOCATE(mask3__tmp(SIZE(mask3_,1), SIZE(mask3_,2), SIZE(mask3_,3)))
        mask3__tmp = mask3_
        CALL cxios_set_gridgroup_mask3(gridgroup_hdl%daddr, mask3__tmp, SHAPE(mask3_))
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_set_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF

  END SUBROUTINE xios(set_gridgroup_attr_hdl_)

  SUBROUTINE xios(get_gridgroup_attr)  &
    ( gridgroup_id, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axis_domain_order(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask1(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1_tmp(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask2(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2_tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask3(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name

      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(get_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(get_gridgroup_attr)

  SUBROUTINE xios(get_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axis_domain_order(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order_tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask1(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1_tmp(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask2(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2_tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask3(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3_tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name

      CALL xios(get_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(get_gridgroup_attr_hdl)

  SUBROUTINE xios(get_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axis_domain_order_, description_, group_ref_, mask1_, mask2_, mask3_, name_  &
     )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: axis_domain_order_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: axis_domain_order__tmp(:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask1_(:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask1__tmp(:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask2_(:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask2__tmp(:,:)
      LOGICAL  , OPTIONAL, INTENT(OUT) :: mask3_(:,:,:)
      LOGICAL (KIND=C_BOOL) , ALLOCATABLE :: mask3__tmp(:,:,:)
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_

      IF (PRESENT(axis_domain_order_)) THEN
        ALLOCATE(axis_domain_order__tmp(SIZE(axis_domain_order_,1)))
        CALL cxios_get_gridgroup_axis_domain_order(gridgroup_hdl%daddr, axis_domain_order__tmp, SHAPE(axis_domain_order_))
        axis_domain_order_ = axis_domain_order__tmp
      ENDIF

      IF (PRESENT(description_)) THEN
        CALL cxios_get_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_gridgroup_group_ref(gridgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF

      IF (PRESENT(mask1_)) THEN
        ALLOCATE(mask1__tmp(SIZE(mask1_,1)))
        CALL cxios_get_gridgroup_mask1(gridgroup_hdl%daddr, mask1__tmp, SHAPE(mask1_))
        mask1_ = mask1__tmp
      ENDIF

      IF (PRESENT(mask2_)) THEN
        ALLOCATE(mask2__tmp(SIZE(mask2_,1), SIZE(mask2_,2)))
        CALL cxios_get_gridgroup_mask2(gridgroup_hdl%daddr, mask2__tmp, SHAPE(mask2_))
        mask2_ = mask2__tmp
      ENDIF

      IF (PRESENT(mask3_)) THEN
        ALLOCATE(mask3__tmp(SIZE(mask3_,1), SIZE(mask3_,2), SIZE(mask3_,3)))
        CALL cxios_get_gridgroup_mask3(gridgroup_hdl%daddr, mask3__tmp, SHAPE(mask3_))
        mask3_ = mask3__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        CALL cxios_get_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      ENDIF

  END SUBROUTINE xios(get_gridgroup_attr_hdl_)

  SUBROUTINE xios(is_defined_gridgroup_attr)  &
    ( gridgroup_id, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup))  :: gridgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::gridgroup_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_domain_order
      LOGICAL(KIND=C_BOOL) :: axis_domain_order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask1
      LOGICAL(KIND=C_BOOL) :: mask1_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask2
      LOGICAL(KIND=C_BOOL) :: mask2_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask3
      LOGICAL(KIND=C_BOOL) :: mask3_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp

      CALL xios(get_gridgroup_handle)(gridgroup_id,gridgroup_hdl)
      CALL xios(is_defined_gridgroup_attr_hdl_)   &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(is_defined_gridgroup_attr)

  SUBROUTINE xios(is_defined_gridgroup_attr_hdl)  &
    ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_domain_order
      LOGICAL(KIND=C_BOOL) :: axis_domain_order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description
      LOGICAL(KIND=C_BOOL) :: description_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref
      LOGICAL(KIND=C_BOOL) :: group_ref_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask1
      LOGICAL(KIND=C_BOOL) :: mask1_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask2
      LOGICAL(KIND=C_BOOL) :: mask2_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask3
      LOGICAL(KIND=C_BOOL) :: mask3_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name
      LOGICAL(KIND=C_BOOL) :: name_tmp

      CALL xios(is_defined_gridgroup_attr_hdl_)  &
      ( gridgroup_hdl, axis_domain_order, description, group_ref, mask1, mask2, mask3, name )

  END SUBROUTINE xios(is_defined_gridgroup_attr_hdl)

  SUBROUTINE xios(is_defined_gridgroup_attr_hdl_)   &
    ( gridgroup_hdl, axis_domain_order_, description_, group_ref_, mask1_, mask2_, mask3_, name_  &
     )

    IMPLICIT NONE
      TYPE(txios(gridgroup)) , INTENT(IN) :: gridgroup_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: axis_domain_order_
      LOGICAL(KIND=C_BOOL) :: axis_domain_order__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: description_
      LOGICAL(KIND=C_BOOL) :: description__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: group_ref_
      LOGICAL(KIND=C_BOOL) :: group_ref__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask1_
      LOGICAL(KIND=C_BOOL) :: mask1__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask2_
      LOGICAL(KIND=C_BOOL) :: mask2__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: mask3_
      LOGICAL(KIND=C_BOOL) :: mask3__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: name_
      LOGICAL(KIND=C_BOOL) :: name__tmp

      IF (PRESENT(axis_domain_order_)) THEN
        axis_domain_order__tmp = cxios_is_defined_gridgroup_axis_domain_order(gridgroup_hdl%daddr)
        axis_domain_order_ = axis_domain_order__tmp
      ENDIF

      IF (PRESENT(description_)) THEN
        description__tmp = cxios_is_defined_gridgroup_description(gridgroup_hdl%daddr)
        description_ = description__tmp
      ENDIF

      IF (PRESENT(group_ref_)) THEN
        group_ref__tmp = cxios_is_defined_gridgroup_group_ref(gridgroup_hdl%daddr)
        group_ref_ = group_ref__tmp
      ENDIF

      IF (PRESENT(mask1_)) THEN
        mask1__tmp = cxios_is_defined_gridgroup_mask1(gridgroup_hdl%daddr)
        mask1_ = mask1__tmp
      ENDIF

      IF (PRESENT(mask2_)) THEN
        mask2__tmp = cxios_is_defined_gridgroup_mask2(gridgroup_hdl%daddr)
        mask2_ = mask2__tmp
      ENDIF

      IF (PRESENT(mask3_)) THEN
        mask3__tmp = cxios_is_defined_gridgroup_mask3(gridgroup_hdl%daddr)
        mask3_ = mask3__tmp
      ENDIF

      IF (PRESENT(name_)) THEN
        name__tmp = cxios_is_defined_gridgroup_name(gridgroup_hdl%daddr)
        name_ = name__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_gridgroup_attr_hdl_)

END MODULE igridgroup_attr
