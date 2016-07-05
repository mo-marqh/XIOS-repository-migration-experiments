! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iinterpolate_domain_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iinterpolate_domain
  USE interpolate_domain_interface_attr

CONTAINS

  SUBROUTINE xios(set_interpolate_domain_attr)  &
    ( interpolate_domain_id, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain))  :: interpolate_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_domain_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: file
      INTEGER  , OPTIONAL, INTENT(IN) :: order
      LOGICAL  , OPTIONAL, INTENT(IN) :: renormalize
      LOGICAL (KIND=C_BOOL) :: renormalize_tmp

      CALL xios(get_interpolate_domain_handle)(interpolate_domain_id,interpolate_domain_hdl)
      CALL xios(set_interpolate_domain_attr_hdl_)   &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(set_interpolate_domain_attr)

  SUBROUTINE xios(set_interpolate_domain_attr_hdl)  &
    ( interpolate_domain_hdl, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: file
      INTEGER  , OPTIONAL, INTENT(IN) :: order
      LOGICAL  , OPTIONAL, INTENT(IN) :: renormalize
      LOGICAL (KIND=C_BOOL) :: renormalize_tmp

      CALL xios(set_interpolate_domain_attr_hdl_)  &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(set_interpolate_domain_attr_hdl)

  SUBROUTINE xios(set_interpolate_domain_attr_hdl_)   &
    ( interpolate_domain_hdl, file_, order_, renormalize_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: file_
      INTEGER  , OPTIONAL, INTENT(IN) :: order_
      LOGICAL  , OPTIONAL, INTENT(IN) :: renormalize_
      LOGICAL (KIND=C_BOOL) :: renormalize__tmp

      IF (PRESENT(file_)) THEN
        CALL cxios_set_interpolate_domain_file(interpolate_domain_hdl%daddr, file_, len(file_))
      ENDIF

      IF (PRESENT(order_)) THEN
        CALL cxios_set_interpolate_domain_order(interpolate_domain_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(renormalize_)) THEN
        renormalize__tmp = renormalize_
        CALL cxios_set_interpolate_domain_renormalize(interpolate_domain_hdl%daddr, renormalize__tmp)
      ENDIF

  END SUBROUTINE xios(set_interpolate_domain_attr_hdl_)

  SUBROUTINE xios(get_interpolate_domain_attr)  &
    ( interpolate_domain_id, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain))  :: interpolate_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_domain_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: file
      INTEGER  , OPTIONAL, INTENT(OUT) :: order
      LOGICAL  , OPTIONAL, INTENT(OUT) :: renormalize
      LOGICAL (KIND=C_BOOL) :: renormalize_tmp

      CALL xios(get_interpolate_domain_handle)(interpolate_domain_id,interpolate_domain_hdl)
      CALL xios(get_interpolate_domain_attr_hdl_)   &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(get_interpolate_domain_attr)

  SUBROUTINE xios(get_interpolate_domain_attr_hdl)  &
    ( interpolate_domain_hdl, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: file
      INTEGER  , OPTIONAL, INTENT(OUT) :: order
      LOGICAL  , OPTIONAL, INTENT(OUT) :: renormalize
      LOGICAL (KIND=C_BOOL) :: renormalize_tmp

      CALL xios(get_interpolate_domain_attr_hdl_)  &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(get_interpolate_domain_attr_hdl)

  SUBROUTINE xios(get_interpolate_domain_attr_hdl_)   &
    ( interpolate_domain_hdl, file_, order_, renormalize_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: file_
      INTEGER  , OPTIONAL, INTENT(OUT) :: order_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: renormalize_
      LOGICAL (KIND=C_BOOL) :: renormalize__tmp

      IF (PRESENT(file_)) THEN
        CALL cxios_get_interpolate_domain_file(interpolate_domain_hdl%daddr, file_, len(file_))
      ENDIF

      IF (PRESENT(order_)) THEN
        CALL cxios_get_interpolate_domain_order(interpolate_domain_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(renormalize_)) THEN
        CALL cxios_get_interpolate_domain_renormalize(interpolate_domain_hdl%daddr, renormalize__tmp)
        renormalize_ = renormalize__tmp
      ENDIF

  END SUBROUTINE xios(get_interpolate_domain_attr_hdl_)

  SUBROUTINE xios(is_defined_interpolate_domain_attr)  &
    ( interpolate_domain_id, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain))  :: interpolate_domain_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_domain_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: file
      LOGICAL(KIND=C_BOOL) :: file_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: renormalize
      LOGICAL(KIND=C_BOOL) :: renormalize_tmp

      CALL xios(get_interpolate_domain_handle)(interpolate_domain_id,interpolate_domain_hdl)
      CALL xios(is_defined_interpolate_domain_attr_hdl_)   &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(is_defined_interpolate_domain_attr)

  SUBROUTINE xios(is_defined_interpolate_domain_attr_hdl)  &
    ( interpolate_domain_hdl, file, order, renormalize )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: file
      LOGICAL(KIND=C_BOOL) :: file_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: renormalize
      LOGICAL(KIND=C_BOOL) :: renormalize_tmp

      CALL xios(is_defined_interpolate_domain_attr_hdl_)  &
      ( interpolate_domain_hdl, file, order, renormalize )

  END SUBROUTINE xios(is_defined_interpolate_domain_attr_hdl)

  SUBROUTINE xios(is_defined_interpolate_domain_attr_hdl_)   &
    ( interpolate_domain_hdl, file_, order_, renormalize_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_domain)) , INTENT(IN) :: interpolate_domain_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: file_
      LOGICAL(KIND=C_BOOL) :: file__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order_
      LOGICAL(KIND=C_BOOL) :: order__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: renormalize_
      LOGICAL(KIND=C_BOOL) :: renormalize__tmp

      IF (PRESENT(file_)) THEN
        file__tmp = cxios_is_defined_interpolate_domain_file(interpolate_domain_hdl%daddr)
        file_ = file__tmp
      ENDIF

      IF (PRESENT(order_)) THEN
        order__tmp = cxios_is_defined_interpolate_domain_order(interpolate_domain_hdl%daddr)
        order_ = order__tmp
      ENDIF

      IF (PRESENT(renormalize_)) THEN
        renormalize__tmp = cxios_is_defined_interpolate_domain_renormalize(interpolate_domain_hdl%daddr)
        renormalize_ = renormalize__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_interpolate_domain_attr_hdl_)

END MODULE iinterpolate_domain_attr
