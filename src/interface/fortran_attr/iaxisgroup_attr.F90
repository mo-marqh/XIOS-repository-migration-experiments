! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE iaxisgroup_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE iaxis
  USE axisgroup_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_axisgroup_attr)  &
    ( axisgroup_id, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
    , zoom_end, zoom_size )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup))  :: axisgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axisgroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size
      
      CALL xios(get_axisgroup_handle)(axisgroup_id,axisgroup_hdl)
      CALL xios(set_axisgroup_attr_hdl_)   &
      ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
      , zoom_end, zoom_size )
    
  END SUBROUTINE xios(set_axisgroup_attr)
  
  SUBROUTINE xios(set_axisgroup_attr_hdl)  &
    ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
    , zoom_end, zoom_size )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup)) , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size
      
      CALL xios(set_axisgroup_attr_hdl_)  &
      ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
      , zoom_end, zoom_size )
    
  END SUBROUTINE xios(set_axisgroup_attr_hdl)
  
  SUBROUTINE xios(set_axisgroup_attr_hdl_)   &
    ( axisgroup_hdl, group_ref_, long_name_, name_, positive_, size_, standard_name_, unit_, value_  &
    , zoom_begin_, zoom_end_, zoom_size_ )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup)) , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: positive_
      INTEGER  , OPTIONAL, INTENT(IN) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value_(:)
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(IN) :: zoom_size_
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_set_axisgroup_group_ref(axisgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_set_axisgroup_long_name(axisgroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_axisgroup_name(axisgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(positive_)) THEN
        CALL cxios_set_axisgroup_positive(axisgroup_hdl%daddr, positive_, len(positive_))
      ENDIF
      
      IF (PRESENT(size_)) THEN
        CALL cxios_set_axisgroup_size(axisgroup_hdl%daddr, size_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_set_axisgroup_standard_name(axisgroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_set_axisgroup_unit(axisgroup_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      IF (PRESENT(value_)) THEN
        CALL cxios_set_axisgroup_value(axisgroup_hdl%daddr, value_,size(value_,1))
      ENDIF
      
      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_set_axisgroup_zoom_begin(axisgroup_hdl%daddr, zoom_begin_)
      ENDIF
      
      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_set_axisgroup_zoom_end(axisgroup_hdl%daddr, zoom_end_)
      ENDIF
      
      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_set_axisgroup_zoom_size(axisgroup_hdl%daddr, zoom_size_)
      ENDIF
      
      
    
  END SUBROUTINE xios(set_axisgroup_attr_hdl_)
  
  SUBROUTINE xios(get_axisgroup_attr)  &
    ( axisgroup_id, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
    , zoom_end, zoom_size )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup))  :: axisgroup_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axisgroup_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size
      
      CALL xios(get_axisgroup_handle)(axisgroup_id,axisgroup_hdl)
      CALL xios(get_axisgroup_attr_hdl_)   &
      ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
      , zoom_end, zoom_size )
    
  END SUBROUTINE xios(get_axisgroup_attr)
  
  SUBROUTINE xios(get_axisgroup_attr_hdl)  &
    ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
    , zoom_end, zoom_size )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup)) , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size
      
      CALL xios(get_axisgroup_attr_hdl_)  &
      ( axisgroup_hdl, group_ref, long_name, name, positive, size, standard_name, unit, value, zoom_begin  &
      , zoom_end, zoom_size )
    
  END SUBROUTINE xios(get_axisgroup_attr_hdl)
  
  SUBROUTINE xios(get_axisgroup_attr_hdl_)   &
    ( axisgroup_hdl, group_ref_, long_name_, name_, positive_, size_, standard_name_, unit_, value_  &
    , zoom_begin_, zoom_end_, zoom_size_ )
    
    IMPLICIT NONE
      TYPE(txios(axisgroup)) , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: group_ref_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: positive_
      INTEGER  , OPTIONAL, INTENT(OUT) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value_(:)
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_begin_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_end_
      INTEGER  , OPTIONAL, INTENT(OUT) :: zoom_size_
      
      IF (PRESENT(group_ref_)) THEN
        CALL cxios_get_axisgroup_group_ref(axisgroup_hdl%daddr, group_ref_, len(group_ref_))
      ENDIF
      
      IF (PRESENT(long_name_)) THEN
        CALL cxios_get_axisgroup_long_name(axisgroup_hdl%daddr, long_name_, len(long_name_))
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_axisgroup_name(axisgroup_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(positive_)) THEN
        CALL cxios_get_axisgroup_positive(axisgroup_hdl%daddr, positive_, len(positive_))
      ENDIF
      
      IF (PRESENT(size_)) THEN
        CALL cxios_get_axisgroup_size(axisgroup_hdl%daddr, size_)
      ENDIF
      
      IF (PRESENT(standard_name_)) THEN
        CALL cxios_get_axisgroup_standard_name(axisgroup_hdl%daddr, standard_name_, len(standard_name_))
      ENDIF
      
      IF (PRESENT(unit_)) THEN
        CALL cxios_get_axisgroup_unit(axisgroup_hdl%daddr, unit_, len(unit_))
      ENDIF
      
      IF (PRESENT(value_)) THEN
        CALL cxios_get_axisgroup_value(axisgroup_hdl%daddr, value_,size(value_,1))
      ENDIF
      
      IF (PRESENT(zoom_begin_)) THEN
        CALL cxios_get_axisgroup_zoom_begin(axisgroup_hdl%daddr, zoom_begin_)
      ENDIF
      
      IF (PRESENT(zoom_end_)) THEN
        CALL cxios_get_axisgroup_zoom_end(axisgroup_hdl%daddr, zoom_end_)
      ENDIF
      
      IF (PRESENT(zoom_size_)) THEN
        CALL cxios_get_axisgroup_zoom_size(axisgroup_hdl%daddr, zoom_size_)
      ENDIF
      
      
    
  END SUBROUTINE xios(get_axisgroup_attr_hdl_)
  
END MODULE iaxisgroup_attr
