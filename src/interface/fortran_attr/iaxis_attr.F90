! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iaxis_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iaxis
  USE axis_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_axis_attr)  &
    ( axis_id, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
     )
    
    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size
      
      CALL xios(get_axis_handle)(axis_id,axis_hdl)
      CALL xios(set_axis_attr_hdl_)   &
      ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
       )
    
  END SUBROUTINE xios(set_axis_attr)
  
  SUBROUTINE xios(set_axis_attr_hdl)  &
    ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
     )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size
      
      CALL xios(set_axis_attr_hdl_)  &
      ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
       )
    
  END SUBROUTINE xios(set_axis_attr_hdl)
  
  SUBROUTINE xios(set_axis_attr_hdl_)   &
    ( axis_hdl, long_name_, name_, size_, standard_name_, unit_, value_, zoom_begin_, zoom_end_  &
    , zoom_size_ )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      INTEGER  , OPTIONAL, INTENT(IN) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size_
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_axis_long_name(axis_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_axis_name(axis_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(size_)) THEN
        CALL cxios_set_axis_size(axis_hdl%daddr, size_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_axis_standard_name(axis_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_set_axis_unit(axis_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      IF (PRESENT(value_)) THEN
        CALL cxios_set_axis_value(axis_hdl%daddr, value_,size(value_,1))
      ENDIF
      
      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_set_axis_zoom_begin(axis_hdl%daddr, zoom_begin_)
      ENDIF
      
      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_set_axis_zoom_end(axis_hdl%daddr, zoom_end_)
      ENDIF
      
      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_set_axis_zoom_size(axis_hdl%daddr, zoom_size_)
      ENDIF
      
      
    
  END SUBROUTINE xios(set_axis_attr_hdl_)
  
  SUBROUTINE xios(get_axis_attr)  &
    ( axis_id, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
     )
    
    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size
      
      CALL xios(get_axis_handle)(axis_id,axis_hdl)
      CALL xios(get_axis_attr_hdl_)   &
      ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
       )
    
  END SUBROUTINE xios(get_axis_attr)
  
  SUBROUTINE xios(get_axis_attr_hdl)  &
    ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
     )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size
      
      CALL xios(get_axis_attr_hdl_)  &
      ( axis_hdl, long_name, name, size, standard_name, unit, value, zoom_begin, zoom_end, zoom_size  &
       )
    
  END SUBROUTINE xios(get_axis_attr_hdl)
  
  SUBROUTINE xios(get_axis_attr_hdl_)   &
    ( axis_hdl, long_name_, name_, size_, standard_name_, unit_, value_, zoom_begin_, zoom_end_  &
    , zoom_size_ )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      INTEGER  , OPTIONAL, INTENT(OUT) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size_
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_axis_long_name(axis_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_axis_name(axis_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(size_)) THEN
        CALL cxios_get_axis_size(axis_hdl%daddr, size_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_axis_standard_name(axis_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_get_axis_unit(axis_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      IF (PRESENT(value_)) THEN
        CALL cxios_get_axis_value(axis_hdl%daddr, value_,size(value_,1))
      ENDIF
      
      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_get_axis_zoom_begin(axis_hdl%daddr, zoom_begin_)
      ENDIF
      
      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_get_axis_zoom_end(axis_hdl%daddr, zoom_end_)
      ENDIF
      
      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_get_axis_zoom_size(axis_hdl%daddr, zoom_size_)
      ENDIF
      
      
    
  END SUBROUTINE xios(get_axis_attr_hdl_)
  
END MODULE iaxis_attr
