#include "xios_fortran_prefix.hpp"
MODULE IDATE
   USE, INTRINSIC :: ISO_C_BINDING

   ! enum XCalendarType
   INTEGER(kind = C_INT), PARAMETER :: D360 = 0 , ALLLEAP = 1 , NOLEAP = 2 , JULIAN = 3 , GREGORIAN = 4

   TYPE, BIND(C) :: txios(date)
      INTEGER(kind = C_INT) :: year, month, day, hour, minute, second
   END TYPE txios(date)

   TYPE, BIND(C) :: txios(duration)
      REAL(kind = C_DOUBLE) :: year=0, month=0, day=0, hour=0, minute=0, second=0, timestep=0
   END TYPE txios(duration)

   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99

      SUBROUTINE cxios_update_calendar(step) BIND(C)
         IMPORT C_INT
         INTEGER (kind = C_INT), VALUE :: step
      END SUBROUTINE cxios_update_calendar

   END INTERFACE
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(update_calendar)(step)
     IMPLICIT NONE
     INTEGER, INTENT(IN):: step
      
      IF (step < 0) THEN
         PRINT *, "L'argument 'step' ne peut être négatif"
         STOP
      END IF
      CALL cxios_update_calendar(step)
   END SUBROUTINE xios(update_calendar)

END MODULE IDATE
