! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE calendar_wrapper_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_calendar_wrapper_start_date(calendar_wrapper_hdl, start_date, start_date_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: start_date
      INTEGER  (kind = C_INT)     , VALUE        :: start_date_size
    END SUBROUTINE cxios_set_calendar_wrapper_start_date
    
    SUBROUTINE cxios_get_calendar_wrapper_start_date(calendar_wrapper_hdl, start_date, start_date_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: start_date
      INTEGER  (kind = C_INT)     , VALUE        :: start_date_size
    END SUBROUTINE cxios_get_calendar_wrapper_start_date
    
    FUNCTION cxios_is_defined_calendar_wrapper_start_date(calendar_wrapper_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_calendar_wrapper_start_date
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
    END FUNCTION cxios_is_defined_calendar_wrapper_start_date
    
    
    SUBROUTINE cxios_set_calendar_wrapper_time_origin(calendar_wrapper_hdl, time_origin, time_origin_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: time_origin
      INTEGER  (kind = C_INT)     , VALUE        :: time_origin_size
    END SUBROUTINE cxios_set_calendar_wrapper_time_origin
    
    SUBROUTINE cxios_get_calendar_wrapper_time_origin(calendar_wrapper_hdl, time_origin, time_origin_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: time_origin
      INTEGER  (kind = C_INT)     , VALUE        :: time_origin_size
    END SUBROUTINE cxios_get_calendar_wrapper_time_origin
    
    FUNCTION cxios_is_defined_calendar_wrapper_time_origin(calendar_wrapper_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_calendar_wrapper_time_origin
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
    END FUNCTION cxios_is_defined_calendar_wrapper_time_origin
    
    
    SUBROUTINE cxios_set_calendar_wrapper_timestep(calendar_wrapper_hdl, timestep) BIND(C)
      USE ISO_C_BINDING
      USE IDURATION
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      TYPE(txios(duration)), VALUE :: timestep
    END SUBROUTINE cxios_set_calendar_wrapper_timestep
    
    SUBROUTINE cxios_get_calendar_wrapper_timestep(calendar_wrapper_hdl, timestep) BIND(C)
      USE ISO_C_BINDING
      USE IDURATION
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      TYPE(txios(duration)) :: timestep
    END SUBROUTINE cxios_get_calendar_wrapper_timestep
    
    FUNCTION cxios_is_defined_calendar_wrapper_timestep(calendar_wrapper_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_calendar_wrapper_timestep
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
    END FUNCTION cxios_is_defined_calendar_wrapper_timestep
    
    
    SUBROUTINE cxios_set_calendar_wrapper_type(calendar_wrapper_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_set_calendar_wrapper_type
    
    SUBROUTINE cxios_get_calendar_wrapper_type(calendar_wrapper_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_get_calendar_wrapper_type
    
    FUNCTION cxios_is_defined_calendar_wrapper_type(calendar_wrapper_hdl ) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_calendar_wrapper_type
      INTEGER (kind = C_INTPTR_T), VALUE :: calendar_wrapper_hdl
    END FUNCTION cxios_is_defined_calendar_wrapper_type
    
    
    END INTERFACE
  
END MODULE calendar_wrapper_interface_attr
