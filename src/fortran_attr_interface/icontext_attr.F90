! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE icontext_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE icontext
  USE context_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_context_attr)  &
    ( context_id, calendar_type, output_dir, start_date, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep
      
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(set_context_attr_hdl_)   &
      ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
  END SUBROUTINE xios(set_context_attr)
  
  SUBROUTINE xios(set_context_attr_hdl)  &
    ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep
      
      CALL xios(set_context_attr_hdl_)  &
      ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
  END SUBROUTINE xios(set_context_attr_hdl)
  
  SUBROUTINE xios(set_context_attr_hdl_)   &
    ( context_hdl, calendar_type_, output_dir_, start_date_, timestep_ )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: timestep_
      
      IF (PRESENT(calendar_type_)) THEN
        CALL cxios_set_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      ENDIF
      
      IF (PRESENT(output_dir_)) THEN
        CALL cxios_set_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_set_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_set_context_timestep(context_hdl%daddr, timestep_, len(timestep_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_context_attr_hdl_)
  
  SUBROUTINE xios(get_context_attr)  &
    ( context_id, calendar_type, output_dir, start_date, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep
      
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(get_context_attr_hdl_)   &
      ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
  END SUBROUTINE xios(get_context_attr)
  
  SUBROUTINE xios(get_context_attr_hdl)  &
    ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep
      
      CALL xios(get_context_attr_hdl_)  &
      ( context_hdl, calendar_type, output_dir, start_date, timestep )
    
  END SUBROUTINE xios(get_context_attr_hdl)
  
  SUBROUTINE xios(get_context_attr_hdl_)   &
    ( context_hdl, calendar_type_, output_dir_, start_date_, timestep_ )
    
    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: calendar_type_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: start_date_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: timestep_
      
      IF (PRESENT(calendar_type_)) THEN
        CALL cxios_get_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      ENDIF
      
      IF (PRESENT(output_dir_)) THEN
        CALL cxios_get_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF
      
      IF (PRESENT(start_date_)) THEN
        CALL cxios_get_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      ENDIF
      
      IF (PRESENT(timestep_)) THEN
        CALL cxios_get_context_timestep(context_hdl%daddr, timestep_, len(timestep_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_context_attr_hdl_)
  
END MODULE icontext_attr
