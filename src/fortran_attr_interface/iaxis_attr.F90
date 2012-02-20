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
    ( axis_id, long_name, name, size, standard_name, unit, value )
    
    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      
      CALL xios(get_axis_handle)(axis_id,axis_hdl)
      CALL xios(set_axis_attr_hdl_)   &
      ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
  END SUBROUTINE xios(set_axis_attr)
  
  SUBROUTINE xios(set_axis_attr_hdl)  &
    ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      INTEGER  , OPTIONAL, INTENT(IN) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value(:)
      
      CALL xios(set_axis_attr_hdl_)  &
      ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
  END SUBROUTINE xios(set_axis_attr_hdl)
  
  SUBROUTINE xios(set_axis_attr_hdl_)   &
    ( axis_hdl, long_name_, name_, size_, standard_name_, unit_, value_ )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      INTEGER  , OPTIONAL, INTENT(IN) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(IN) :: value_(:)
      
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
      
      
    
  END SUBROUTINE xios(set_axis_attr_hdl_)
  
  SUBROUTINE xios(get_axis_attr)  &
    ( axis_id, long_name, name, size, standard_name, unit, value )
    
    IMPLICIT NONE
      TYPE(txios(axis))  :: axis_hdl
      CHARACTER(LEN=*), INTENT(IN) ::axis_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      
      CALL xios(get_axis_handle)(axis_id,axis_hdl)
      CALL xios(get_axis_attr_hdl_)   &
      ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
  END SUBROUTINE xios(get_axis_attr)
  
  SUBROUTINE xios(get_axis_attr_hdl)  &
    ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      INTEGER  , OPTIONAL, INTENT(OUT) :: size
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value(:)
      
      CALL xios(get_axis_attr_hdl_)  &
      ( axis_hdl, long_name, name, size, standard_name, unit, value )
    
  END SUBROUTINE xios(get_axis_attr_hdl)
  
  SUBROUTINE xios(get_axis_attr_hdl_)   &
    ( axis_hdl, long_name_, name_, size_, standard_name_, unit_, value_ )
    
    IMPLICIT NONE
      TYPE(txios(axis)) , INTENT(IN) :: axis_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: long_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      INTEGER  , OPTIONAL, INTENT(OUT) :: size_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: standard_name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: unit_
      REAL (KIND=8) , OPTIONAL, INTENT(OUT) :: value_(:)
      
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
      
      
    
  END SUBROUTINE xios(get_axis_attr_hdl_)
  
END MODULE iaxis_attr
