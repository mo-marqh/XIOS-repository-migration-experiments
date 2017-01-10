

MODULE ICALENDAR_WRAPPER
   USE, INTRINSIC :: ISO_C_BINDING
   USE CALENDAR_WRAPPER_INTERFACE
   USE IDATE
   USE IDURATION

   TYPE xios_calendar_wrapper
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_calendar_wrapper

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_calendar_wrapper_handle(idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_calendar_wrapper), INTENT(OUT) :: ret

      CALL cxios_calendar_wrapper_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios_get_calendar_wrapper_handle

   SUBROUTINE xios_get_default_calendar_wrapper_handle(ret)
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(OUT) :: ret

      CALL cxios_get_current_calendar_wrapper(ret%daddr)
   END SUBROUTINE xios_get_default_calendar_wrapper_handle

   LOGICAL FUNCTION xios_is_valid_calendar_wrapper(idt)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_calendar_wrapper_valid_id(val, idt, len(idt));
      xios_is_valid_calendar_wrapper = val
   END FUNCTION xios_is_valid_calendar_wrapper

   SUBROUTINE xios_create_calendar(hdl)
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl

      CALL cxios_create_calendar(hdl%daddr)
   END SUBROUTINE xios_create_calendar

   SUBROUTINE xios_update_calendar_timestep(hdl)
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl

      CALL cxios_update_calendar_timestep(hdl%daddr)
   END SUBROUTINE xios_update_calendar_timestep

   SUBROUTINE xios_set_start_date_hdl(hdl, start_date)
      USE IDATE
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl
      TYPE(xios_date), INTENT(IN) :: start_date

      CALL cxios_set_calendar_wrapper_date_start_date(hdl%daddr, start_date)
   END SUBROUTINE xios_set_start_date_hdl

   SUBROUTINE xios_get_start_date_hdl(hdl, start_date)
      USE IDATE
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl
      TYPE(xios_date), INTENT(OUT) :: start_date

      CALL cxios_get_calendar_wrapper_date_start_date(hdl%daddr, start_date)
   END SUBROUTINE xios_get_start_date_hdl

   SUBROUTINE xios_set_time_origin_hdl(hdl, time_origin)
      USE IDATE
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl
      TYPE(xios_date), INTENT(IN) :: time_origin

      CALL cxios_set_calendar_wrapper_date_time_origin(hdl%daddr, time_origin)
   END SUBROUTINE xios_set_time_origin_hdl

   SUBROUTINE xios_get_time_origin_hdl(hdl, time_origin)
      USE IDATE
      IMPLICIT NONE
      TYPE(xios_calendar_wrapper), INTENT(IN) :: hdl
      TYPE(xios_date), INTENT(OUT) :: time_origin

      CALL cxios_get_calendar_wrapper_date_time_origin(hdl%daddr, time_origin)
   END SUBROUTINE xios_get_time_origin_hdl

END MODULE ICALENDAR_WRAPPER
