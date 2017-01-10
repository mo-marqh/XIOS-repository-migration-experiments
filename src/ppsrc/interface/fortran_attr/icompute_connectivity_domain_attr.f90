! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE icompute_connectivity_domain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE icompute_connectivity_domain
  USE compute_connectivity_domain_interface_attr

CONTAINS

  SUBROUTINE xios_set_compute_connectivity_domain_attr &
    ( compute_connectivity_domain_id, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) :: compute_connectivity_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::compute_connectivity_domain_id
      INTEGER , OPTIONAL, INTENT(IN) :: local_neighbor(:,:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor(:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor_max
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_get_compute_connectivity_domain_handle &
      (compute_connectivity_domain_id,compute_connectivity_domain_hdl)
      CALL xios_set_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_set_compute_connectivity_domain_attr

  SUBROUTINE xios_set_compute_connectivity_domain_attr_hdl &
    ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: local_neighbor(:,:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor(:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor_max
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios_set_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_set_compute_connectivity_domain_attr_hdl

  SUBROUTINE xios_set_compute_connectivity_domain_attr_hdl_ &
    ( compute_connectivity_domain_hdl, local_neighbor_, n_neighbor_, n_neighbor_max_, type_ )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      INTEGER , OPTIONAL, INTENT(IN) :: local_neighbor_(:,:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor_(:)
      INTEGER , OPTIONAL, INTENT(IN) :: n_neighbor_max_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(local_neighbor_)) THEN
        CALL cxios_set_compute_connectivity_domain_local_neighbor &
      (compute_connectivity_domain_hdl%daddr, local_neighbor_, SHAPE(local_neighbor_))
      ENDIF

      IF (PRESENT(n_neighbor_)) THEN
        CALL cxios_set_compute_connectivity_domain_n_neighbor &
      (compute_connectivity_domain_hdl%daddr, n_neighbor_, SHAPE(n_neighbor_))
      ENDIF

      IF (PRESENT(n_neighbor_max_)) THEN
        CALL cxios_set_compute_connectivity_domain_n_neighbor_max &
      (compute_connectivity_domain_hdl%daddr, n_neighbor_max_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_compute_connectivity_domain_type &
      (compute_connectivity_domain_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_set_compute_connectivity_domain_attr_hdl_

  SUBROUTINE xios_get_compute_connectivity_domain_attr &
    ( compute_connectivity_domain_id, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) :: compute_connectivity_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::compute_connectivity_domain_id
      INTEGER , OPTIONAL, INTENT(OUT) :: local_neighbor(:,:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor(:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor_max
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_compute_connectivity_domain_handle &
      (compute_connectivity_domain_id,compute_connectivity_domain_hdl)
      CALL xios_get_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_get_compute_connectivity_domain_attr

  SUBROUTINE xios_get_compute_connectivity_domain_attr_hdl &
    ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: local_neighbor(:,:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor(:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor_max
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios_get_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_get_compute_connectivity_domain_attr_hdl

  SUBROUTINE xios_get_compute_connectivity_domain_attr_hdl_ &
    ( compute_connectivity_domain_hdl, local_neighbor_, n_neighbor_, n_neighbor_max_, type_ )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      INTEGER , OPTIONAL, INTENT(OUT) :: local_neighbor_(:,:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor_(:)
      INTEGER , OPTIONAL, INTENT(OUT) :: n_neighbor_max_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(local_neighbor_)) THEN
        CALL cxios_get_compute_connectivity_domain_local_neighbor &
      (compute_connectivity_domain_hdl%daddr, local_neighbor_, SHAPE(local_neighbor_))
      ENDIF

      IF (PRESENT(n_neighbor_)) THEN
        CALL cxios_get_compute_connectivity_domain_n_neighbor &
      (compute_connectivity_domain_hdl%daddr, n_neighbor_, SHAPE(n_neighbor_))
      ENDIF

      IF (PRESENT(n_neighbor_max_)) THEN
        CALL cxios_get_compute_connectivity_domain_n_neighbor_max &
      (compute_connectivity_domain_hdl%daddr, n_neighbor_max_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_compute_connectivity_domain_type &
      (compute_connectivity_domain_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios_get_compute_connectivity_domain_attr_hdl_

  SUBROUTINE xios_is_defined_compute_connectivity_domain_attr &
    ( compute_connectivity_domain_id, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) :: compute_connectivity_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::compute_connectivity_domain_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: local_neighbor
      LOGICAL(KIND=C_BOOL) :: local_neighbor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor
      LOGICAL(KIND=C_BOOL) :: n_neighbor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor_max
      LOGICAL(KIND=C_BOOL) :: n_neighbor_max_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_get_compute_connectivity_domain_handle &
      (compute_connectivity_domain_id,compute_connectivity_domain_hdl)
      CALL xios_is_defined_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_is_defined_compute_connectivity_domain_attr

  SUBROUTINE xios_is_defined_compute_connectivity_domain_attr_hdl &
    ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: local_neighbor
      LOGICAL(KIND=C_BOOL) :: local_neighbor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor
      LOGICAL(KIND=C_BOOL) :: n_neighbor_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor_max
      LOGICAL(KIND=C_BOOL) :: n_neighbor_max_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios_is_defined_compute_connectivity_domain_attr_hdl_ &
      ( compute_connectivity_domain_hdl, local_neighbor, n_neighbor, n_neighbor_max, type )

  END SUBROUTINE xios_is_defined_compute_connectivity_domain_attr_hdl

  SUBROUTINE xios_is_defined_compute_connectivity_domain_attr_hdl_ &
    ( compute_connectivity_domain_hdl, local_neighbor_, n_neighbor_, n_neighbor_max_, type_ )

    IMPLICIT NONE
      TYPE(xios_compute_connectivity_domain) , INTENT(IN) :: compute_connectivity_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: local_neighbor_
      LOGICAL(KIND=C_BOOL) :: local_neighbor__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor_
      LOGICAL(KIND=C_BOOL) :: n_neighbor__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: n_neighbor_max_
      LOGICAL(KIND=C_BOOL) :: n_neighbor_max__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(local_neighbor_)) THEN
        local_neighbor__tmp = cxios_is_defined_compute_connectivity_domain_local_neighbor &
      (compute_connectivity_domain_hdl%daddr)
        local_neighbor_ = local_neighbor__tmp
      ENDIF

      IF (PRESENT(n_neighbor_)) THEN
        n_neighbor__tmp = cxios_is_defined_compute_connectivity_domain_n_neighbor &
      (compute_connectivity_domain_hdl%daddr)
        n_neighbor_ = n_neighbor__tmp
      ENDIF

      IF (PRESENT(n_neighbor_max_)) THEN
        n_neighbor_max__tmp = cxios_is_defined_compute_connectivity_domain_n_neighbor_max &
      (compute_connectivity_domain_hdl%daddr)
        n_neighbor_max_ = n_neighbor_max__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_compute_connectivity_domain_type &
      (compute_connectivity_domain_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios_is_defined_compute_connectivity_domain_attr_hdl_

END MODULE icompute_connectivity_domain_attr
