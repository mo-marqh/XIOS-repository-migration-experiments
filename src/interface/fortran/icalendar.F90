#include "xios_fortran_prefix.hpp"
MODULE ICALENDAR
   USE, INTRINSIC :: ISO_C_BINDING
   USE CALENDAR_INTERFACE
   USE IDATE
   USE IDURATION

   ! enum XCalendarType
   INTEGER(kind = C_INT), PARAMETER :: D360 = 0 , ALLLEAP = 1 , NOLEAP = 2 , JULIAN = 3 , GREGORIAN = 4

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(set_calendar)(calendar_type, start_date, time_origin, timestep)
      USE ICONTEXT, ONLY : txios(context), xios(get_current_context)
      USE icontext_attr, ONLY : xios(set_context_attr_hdl)
      USE IDATE, ONLY : txios(date)
      USE IDURATION, ONLY : txios(duration)
      IMPLICIT NONE
      CHARACTER(len = *),    OPTIONAL, INTENT(IN) :: calendar_type
      TYPE(txios(date)),     OPTIONAL, INTENT(IN) :: start_date
      TYPE(txios(date)),     OPTIONAL, INTENT(IN) :: time_origin
      TYPE(txios(duration)), OPTIONAL, INTENT(IN) :: timestep
      TYPE(txios(context)) :: context

      CALL xios(get_current_context)(context)

      IF (PRESENT(calendar_type)) THEN
         CALL xios(set_context_attr_hdl)(context, calendar_type=calendar_type)
      END IF
      IF (PRESENT(start_date)) THEN
         CALL xios(set_context_attr_hdl)(context, start_date=start_date)
      END IF
      IF (PRESENT(time_origin)) THEN
         CALL xios(set_context_attr_hdl)(context, time_origin=time_origin)
      END IF
      IF (PRESENT(time_origin)) THEN
         CALL xios(set_context_attr_hdl)(context, timestep=timestep)
      END IF

      CALL cxios_create_calendar()
   END SUBROUTINE xios(set_calendar)

   SUBROUTINE xios(set_timestep)(timestep)
      USE ICONTEXT, ONLY : txios(context), xios(get_current_context)
      USE icontext_attr, ONLY : xios(set_context_attr_hdl)
      USE IDURATION, ONLY : txios(duration)
      IMPLICIT NONE
      TYPE(txios(duration)), INTENT(IN) :: timestep
      TYPE(txios(context)) :: context

      CALL xios(get_current_context)(context)

      CALL xios(set_context_attr_hdl)(context, timestep=timestep)
   END SUBROUTINE xios(set_timestep)

   SUBROUTINE xios(update_calendar)(step)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: step
      
      IF (step < 0) THEN
         PRINT *, "L'argument 'step' ne peut être négatif"
         STOP
      END IF
      CALL cxios_update_calendar(step)
   END SUBROUTINE xios(update_calendar)

END MODULE ICALENDAR
