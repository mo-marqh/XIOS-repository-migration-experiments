! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE izoom_axis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE izoom_axis
  USE zoom_axis_interface_attr

CONTAINS

  SUBROUTINE xios(set_zoom_axis_attr)  &
    ( zoom_axis_id, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis))  :: zoom_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_axis_id
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size

      CALL xios(get_zoom_axis_handle)(zoom_axis_id,zoom_axis_hdl)
      CALL xios(set_zoom_axis_attr_hdl_)   &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(set_zoom_axis_attr)

  SUBROUTINE xios(set_zoom_axis_attr_hdl)  &
    ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size

      CALL xios(set_zoom_axis_attr_hdl_)  &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(set_zoom_axis_attr_hdl)

  SUBROUTINE xios(set_zoom_axis_attr_hdl_)   &
    ( zoom_axis_hdl, zoom_begin_, zoom_end_, zoom_size_ )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size_

      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_set_zoom_axis_zoom_begin(zoom_axis_hdl%daddr, zoom_begin_)
      ENDIF

      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_set_zoom_axis_zoom_end(zoom_axis_hdl%daddr, zoom_end_)
      ENDIF

      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_set_zoom_axis_zoom_size(zoom_axis_hdl%daddr, zoom_size_)
      ENDIF

  END SUBROUTINE xios(set_zoom_axis_attr_hdl_)

  SUBROUTINE xios(get_zoom_axis_attr)  &
    ( zoom_axis_id, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis))  :: zoom_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_axis_id
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size

      CALL xios(get_zoom_axis_handle)(zoom_axis_id,zoom_axis_hdl)
      CALL xios(get_zoom_axis_attr_hdl_)   &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(get_zoom_axis_attr)

  SUBROUTINE xios(get_zoom_axis_attr_hdl)  &
    ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size

      CALL xios(get_zoom_axis_attr_hdl_)  &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(get_zoom_axis_attr_hdl)

  SUBROUTINE xios(get_zoom_axis_attr_hdl_)   &
    ( zoom_axis_hdl, zoom_begin_, zoom_end_, zoom_size_ )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size_

      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_get_zoom_axis_zoom_begin(zoom_axis_hdl%daddr, zoom_begin_)
      ENDIF

      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_get_zoom_axis_zoom_end(zoom_axis_hdl%daddr, zoom_end_)
      ENDIF

      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_get_zoom_axis_zoom_size(zoom_axis_hdl%daddr, zoom_size_)
      ENDIF

  END SUBROUTINE xios(get_zoom_axis_attr_hdl_)

  SUBROUTINE xios(is_defined_zoom_axis_attr)  &
    ( zoom_axis_id, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis))  :: zoom_axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::zoom_axis_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_begin
      LOGICAL(KIND=C_BOOL) :: zoom_begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_end
      LOGICAL(KIND=C_BOOL) :: zoom_end_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_size
      LOGICAL(KIND=C_BOOL) :: zoom_size_tmp

      CALL xios(get_zoom_axis_handle)(zoom_axis_id,zoom_axis_hdl)
      CALL xios(is_defined_zoom_axis_attr_hdl_)   &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(is_defined_zoom_axis_attr)

  SUBROUTINE xios(is_defined_zoom_axis_attr_hdl)  &
    ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_begin
      LOGICAL(KIND=C_BOOL) :: zoom_begin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_end
      LOGICAL(KIND=C_BOOL) :: zoom_end_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_size
      LOGICAL(KIND=C_BOOL) :: zoom_size_tmp

      CALL xios(is_defined_zoom_axis_attr_hdl_)  &
      ( zoom_axis_hdl, zoom_begin, zoom_end, zoom_size )

  END SUBROUTINE xios(is_defined_zoom_axis_attr_hdl)

  SUBROUTINE xios(is_defined_zoom_axis_attr_hdl_)   &
    ( zoom_axis_hdl, zoom_begin_, zoom_end_, zoom_size_ )

    IMPLICIT NONE
      TYPE(txios(zoom_axis)) , INTENT(IN) :: zoom_axis_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_begin_
      LOGICAL(KIND=C_BOOL) :: zoom_begin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_end_
      LOGICAL(KIND=C_BOOL) :: zoom_end__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: zoom_size_
      LOGICAL(KIND=C_BOOL) :: zoom_size__tmp

      IF (PRESENT(zoom_begin_)) THEN
        zoom_begin__tmp = cxios_is_defined_zoom_axis_zoom_begin(zoom_axis_hdl%daddr)
        zoom_begin_ = zoom_begin__tmp
      ENDIF

      IF (PRESENT(zoom_end_)) THEN
        zoom_end__tmp = cxios_is_defined_zoom_axis_zoom_end(zoom_axis_hdl%daddr)
        zoom_end_ = zoom_end__tmp
      ENDIF

      IF (PRESENT(zoom_size_)) THEN
        zoom_size__tmp = cxios_is_defined_zoom_axis_zoom_size(zoom_axis_hdl%daddr)
        zoom_size_ = zoom_size__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_zoom_axis_attr_hdl_)

END MODULE izoom_axis_attr
