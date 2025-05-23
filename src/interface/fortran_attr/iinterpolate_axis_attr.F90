! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iinterpolate_axis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iinterpolate_axis
  USE interpolate_axis_interface_attr
  USE LOGICAL_BOOL_CONVERSION

CONTAINS

  SUBROUTINE xios(set_interpolate_axis_attr)  &
    ( interpolate_axis_id, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis))  :: interpolate_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_dst
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_src
      LOGICAL  , OPTIONAL, INTENT(IN) :: extrapolate
      LOGICAL (KIND=C_BOOL) :: extrapolate_tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(get_interpolate_axis_handle) &
      (interpolate_axis_id,interpolate_axis_hdl)
      CALL xios(set_interpolate_axis_attr_hdl_)   &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(set_interpolate_axis_attr)

  SUBROUTINE xios(set_interpolate_axis_attr_hdl)  &
    ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_dst
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_src
      LOGICAL  , OPTIONAL, INTENT(IN) :: extrapolate
      LOGICAL (KIND=C_BOOL) :: extrapolate_tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type

      CALL xios(set_interpolate_axis_attr_hdl_)  &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(set_interpolate_axis_attr_hdl)

  SUBROUTINE xios(set_interpolate_axis_attr_hdl_)   &
    ( interpolate_axis_hdl, coordinate_, coordinate_dst_, coordinate_src_, extrapolate_, order_  &
    , type_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_dst_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: coordinate_src_
      LOGICAL  , OPTIONAL, INTENT(IN) :: extrapolate_
      LOGICAL (KIND=C_BOOL) :: extrapolate__tmp
      INTEGER  , OPTIONAL, INTENT(IN) :: order_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_

      IF (PRESENT(coordinate_)) THEN
        CALL cxios_set_interpolate_axis_coordinate &
      (interpolate_axis_hdl%daddr, coordinate_, len(coordinate_))
      ENDIF

      IF (PRESENT(coordinate_dst_)) THEN
        CALL cxios_set_interpolate_axis_coordinate_dst &
      (interpolate_axis_hdl%daddr, coordinate_dst_, len(coordinate_dst_))
      ENDIF

      IF (PRESENT(coordinate_src_)) THEN
        CALL cxios_set_interpolate_axis_coordinate_src &
      (interpolate_axis_hdl%daddr, coordinate_src_, len(coordinate_src_))
      ENDIF

      IF (PRESENT(extrapolate_)) THEN
        extrapolate__tmp = extrapolate_
        CALL xios_logical_to_bool_0d(extrapolate__tmp)
        CALL cxios_set_interpolate_axis_extrapolate &
      (interpolate_axis_hdl%daddr, extrapolate__tmp)
      ENDIF

      IF (PRESENT(order_)) THEN
        CALL cxios_set_interpolate_axis_order &
      (interpolate_axis_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_set_interpolate_axis_type &
      (interpolate_axis_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(set_interpolate_axis_attr_hdl_)

  SUBROUTINE xios(get_interpolate_axis_attr)  &
    ( interpolate_axis_id, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis))  :: interpolate_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_dst
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_src
      LOGICAL  , OPTIONAL, INTENT(OUT) :: extrapolate
      LOGICAL (KIND=C_BOOL) :: extrapolate_tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_interpolate_axis_handle) &
      (interpolate_axis_id,interpolate_axis_hdl)
      CALL xios(get_interpolate_axis_attr_hdl_)   &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(get_interpolate_axis_attr)

  SUBROUTINE xios(get_interpolate_axis_attr_hdl)  &
    ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_dst
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_src
      LOGICAL  , OPTIONAL, INTENT(OUT) :: extrapolate
      LOGICAL (KIND=C_BOOL) :: extrapolate_tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: order
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type

      CALL xios(get_interpolate_axis_attr_hdl_)  &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(get_interpolate_axis_attr_hdl)

  SUBROUTINE xios(get_interpolate_axis_attr_hdl_)   &
    ( interpolate_axis_hdl, coordinate_, coordinate_dst_, coordinate_src_, extrapolate_, order_  &
    , type_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_dst_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: coordinate_src_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: extrapolate_
      LOGICAL (KIND=C_BOOL) :: extrapolate__tmp
      INTEGER  , OPTIONAL, INTENT(OUT) :: order_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_

      IF (PRESENT(coordinate_)) THEN
        CALL cxios_get_interpolate_axis_coordinate &
      (interpolate_axis_hdl%daddr, coordinate_, len(coordinate_))
      ENDIF

      IF (PRESENT(coordinate_dst_)) THEN
        CALL cxios_get_interpolate_axis_coordinate_dst &
      (interpolate_axis_hdl%daddr, coordinate_dst_, len(coordinate_dst_))
      ENDIF

      IF (PRESENT(coordinate_src_)) THEN
        CALL cxios_get_interpolate_axis_coordinate_src &
      (interpolate_axis_hdl%daddr, coordinate_src_, len(coordinate_src_))
      ENDIF

      IF (PRESENT(extrapolate_)) THEN
        CALL cxios_get_interpolate_axis_extrapolate &
      (interpolate_axis_hdl%daddr, extrapolate__tmp)
        CALL xios_bool_to_logical_0d(extrapolate__tmp)
        extrapolate_ = extrapolate__tmp
      ENDIF

      IF (PRESENT(order_)) THEN
        CALL cxios_get_interpolate_axis_order &
      (interpolate_axis_hdl%daddr, order_)
      ENDIF

      IF (PRESENT(type_)) THEN
        CALL cxios_get_interpolate_axis_type &
      (interpolate_axis_hdl%daddr, type_, len(type_))
      ENDIF

  END SUBROUTINE xios(get_interpolate_axis_attr_hdl_)

  SUBROUTINE xios(is_defined_interpolate_axis_attr)  &
    ( interpolate_axis_id, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis))  :: interpolate_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::interpolate_axis_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate
      LOGICAL(KIND=C_BOOL) :: coordinate_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_dst
      LOGICAL(KIND=C_BOOL) :: coordinate_dst_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_src
      LOGICAL(KIND=C_BOOL) :: coordinate_src_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: extrapolate
      LOGICAL(KIND=C_BOOL) :: extrapolate_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(get_interpolate_axis_handle) &
      (interpolate_axis_id,interpolate_axis_hdl)
      CALL xios(is_defined_interpolate_axis_attr_hdl_)   &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(is_defined_interpolate_axis_attr)

  SUBROUTINE xios(is_defined_interpolate_axis_attr_hdl)  &
    ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
     )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate
      LOGICAL(KIND=C_BOOL) :: coordinate_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_dst
      LOGICAL(KIND=C_BOOL) :: coordinate_dst_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_src
      LOGICAL(KIND=C_BOOL) :: coordinate_src_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: extrapolate
      LOGICAL(KIND=C_BOOL) :: extrapolate_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order
      LOGICAL(KIND=C_BOOL) :: order_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp

      CALL xios(is_defined_interpolate_axis_attr_hdl_)  &
      ( interpolate_axis_hdl, coordinate, coordinate_dst, coordinate_src, extrapolate, order, type  &
       )

  END SUBROUTINE xios(is_defined_interpolate_axis_attr_hdl)

  SUBROUTINE xios(is_defined_interpolate_axis_attr_hdl_)   &
    ( interpolate_axis_hdl, coordinate_, coordinate_dst_, coordinate_src_, extrapolate_, order_  &
    , type_ )

    IMPLICIT NONE
      TYPE(txios(interpolate_axis)) , INTENT(IN) :: interpolate_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_
      LOGICAL(KIND=C_BOOL) :: coordinate__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_dst_
      LOGICAL(KIND=C_BOOL) :: coordinate_dst__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: coordinate_src_
      LOGICAL(KIND=C_BOOL) :: coordinate_src__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: extrapolate_
      LOGICAL(KIND=C_BOOL) :: extrapolate__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: order_
      LOGICAL(KIND=C_BOOL) :: order__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp

      IF (PRESENT(coordinate_)) THEN
        coordinate__tmp = cxios_is_defined_interpolate_axis_coordinate &
      (interpolate_axis_hdl%daddr)
        coordinate_ = coordinate__tmp
      ENDIF

      IF (PRESENT(coordinate_dst_)) THEN
        coordinate_dst__tmp = cxios_is_defined_interpolate_axis_coordinate_dst &
      (interpolate_axis_hdl%daddr)
        coordinate_dst_ = coordinate_dst__tmp
      ENDIF

      IF (PRESENT(coordinate_src_)) THEN
        coordinate_src__tmp = cxios_is_defined_interpolate_axis_coordinate_src &
      (interpolate_axis_hdl%daddr)
        coordinate_src_ = coordinate_src__tmp
      ENDIF

      IF (PRESENT(extrapolate_)) THEN
        extrapolate__tmp = cxios_is_defined_interpolate_axis_extrapolate &
      (interpolate_axis_hdl%daddr)
        extrapolate_ = extrapolate__tmp
      ENDIF

      IF (PRESENT(order_)) THEN
        order__tmp = cxios_is_defined_interpolate_axis_order &
      (interpolate_axis_hdl%daddr)
        order_ = order__tmp
      ENDIF

      IF (PRESENT(type_)) THEN
        type__tmp = cxios_is_defined_interpolate_axis_type &
      (interpolate_axis_hdl%daddr)
        type_ = type__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_interpolate_axis_attr_hdl_)

END MODULE iinterpolate_axis_attr
