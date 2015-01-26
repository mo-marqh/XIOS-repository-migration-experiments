! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE icalendar_wrapper_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE icalendar_wrapper
  USE calendar_wrapper_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_calendar_wrapper_attr)  &
    ( calendar_wrapper_id, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper))  :: calendar_wrapper_hdl
      CHARACTER(LEN=*), INTENT(IN) ::calendar_wrapper_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: timestep
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(get_calendar_wrapper_handle)(calendar_wrapper_id,calendar_wrapper_hdl)
      CALL xios(set_calendar_wrapper_attr_hdl_)   &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(set_calendar_wrapper_attr)
  
  SUBROUTINE xios(set_calendar_wrapper_attr_hdl)  &
    ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: timestep
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(set_calendar_wrapper_attr_hdl_)  &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(set_calendar_wrapper_attr_hdl)
  
  SUBROUTINE xios(set_calendar_wrapper_attr_hdl_)   &
    ( calendar_wrapper_hdl, start_date_, time_origin_, timestep_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: time_origin_
      TYPE(txios(duration))  , OPTIONAL, INTENT(IN) :: timestep_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_set_calendar_wrapper_start_date(calendar_wrapper_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        CALL cxios_set_calendar_wrapper_time_origin(calendar_wrapper_hdl%daddr, time_origin_, len(time_origin_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_set_calendar_wrapper_timestep(calendar_wrapper_hdl%daddr, timestep_)
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_set_calendar_wrapper_type(calendar_wrapper_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_calendar_wrapper_attr_hdl_)
  
  SUBROUTINE xios(get_calendar_wrapper_attr)  &
    ( calendar_wrapper_id, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper))  :: calendar_wrapper_hdl
      CHARACTER(LEN=*), INTENT(IN) ::calendar_wrapper_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: timestep
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_calendar_wrapper_handle)(calendar_wrapper_id,calendar_wrapper_hdl)
      CALL xios(get_calendar_wrapper_attr_hdl_)   &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(get_calendar_wrapper_attr)
  
  SUBROUTINE xios(get_calendar_wrapper_attr_hdl)  &
    ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: timestep
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_calendar_wrapper_attr_hdl_)  &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(get_calendar_wrapper_attr_hdl)
  
  SUBROUTINE xios(get_calendar_wrapper_attr_hdl_)   &
    ( calendar_wrapper_hdl, start_date_, time_origin_, timestep_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: time_origin_
      TYPE(txios(duration))  , OPTIONAL, INTENT(OUT) :: timestep_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_get_calendar_wrapper_start_date(calendar_wrapper_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        CALL cxios_get_calendar_wrapper_time_origin(calendar_wrapper_hdl%daddr, time_origin_, len(time_origin_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_get_calendar_wrapper_timestep(calendar_wrapper_hdl%daddr, timestep_)
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_get_calendar_wrapper_type(calendar_wrapper_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_calendar_wrapper_attr_hdl_)
  
  SUBROUTINE xios(is_defined_calendar_wrapper_attr)  &
    ( calendar_wrapper_id, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper))  :: calendar_wrapper_hdl
      CHARACTER(LEN=*), INTENT(IN) ::calendar_wrapper_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date
      LOGICAL(KIND=C_BOOL) :: start_date_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin
      LOGICAL(KIND=C_BOOL) :: time_origin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep
      LOGICAL(KIND=C_BOOL) :: timestep_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp
      
      CALL xios(get_calendar_wrapper_handle)(calendar_wrapper_id,calendar_wrapper_hdl)
      CALL xios(is_defined_calendar_wrapper_attr_hdl_)   &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(is_defined_calendar_wrapper_attr)
  
  SUBROUTINE xios(is_defined_calendar_wrapper_attr_hdl)  &
    ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date
      LOGICAL(KIND=C_BOOL) :: start_date_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin
      LOGICAL(KIND=C_BOOL) :: time_origin_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep
      LOGICAL(KIND=C_BOOL) :: timestep_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type
      LOGICAL(KIND=C_BOOL) :: type_tmp
      
      CALL xios(is_defined_calendar_wrapper_attr_hdl_)  &
      ( calendar_wrapper_hdl, start_date, time_origin, timestep, type )
    
  END SUBROUTINE xios(is_defined_calendar_wrapper_attr_hdl)
  
  SUBROUTINE xios(is_defined_calendar_wrapper_attr_hdl_)   &
    ( calendar_wrapper_hdl, start_date_, time_origin_, timestep_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(calendar_wrapper)) , INTENT(IN) :: calendar_wrapper_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: start_date_
      LOGICAL(KIND=C_BOOL) :: start_date__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: time_origin_
      LOGICAL(KIND=C_BOOL) :: time_origin__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: timestep_
      LOGICAL(KIND=C_BOOL) :: timestep__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: type_
      LOGICAL(KIND=C_BOOL) :: type__tmp
      
      IF (PRESENT(start_date_)) THEN
        start_date__tmp=cxios_is_defined_calendar_wrapper_start_date(calendar_wrapper_hdl%daddr)
        start_date_=start_date__tmp
      ENDIF
      
      IF (PRESENT(time_origin_)) THEN
        time_origin__tmp=cxios_is_defined_calendar_wrapper_time_origin(calendar_wrapper_hdl%daddr)
        time_origin_=time_origin__tmp
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        timestep__tmp=cxios_is_defined_calendar_wrapper_timestep(calendar_wrapper_hdl%daddr)
        timestep_=timestep__tmp
      ENDIF
      
      IF (PRESENT(type_)) THEN
        type__tmp=cxios_is_defined_calendar_wrapper_type(calendar_wrapper_hdl%daddr)
        type_=type__tmp
      ENDIF
      
      
    
  END SUBROUTINE xios(is_defined_calendar_wrapper_attr_hdl_)
  
END MODULE icalendar_wrapper_attr
