MODULE IDATE
   USE, INTRINSIC :: ISO_C_BINDING

   ! enum XCalendarType
   INTEGER(kind = C_INT), PARAMETER :: D360 = 0 , ALLLEAP = 1 , NOLEAP = 2 , JULIAN = 3 , GREGORIAN = 4

   TYPE XDate
      INTEGER :: year, month, day, hour, minute, second
   END TYPE XDate

   TYPE XDuration
      REAL(kind = 8) :: year, month, day, hour, minute, second
   END TYPE XDuration
   
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
   
      SUBROUTINE xios_set_timestep(ts_year, ts_month, ts_day, ts_hour, ts_minute, ts_second) BIND(C)
         IMPORT C_DOUBLE
         REAL (kind = C_DOUBLE), VALUE :: ts_year, ts_month , ts_day   , &
                                          ts_hour, ts_minute, ts_second
      END SUBROUTINE xios_set_timestep

      SUBROUTINE xios_update_calendar(step) BIND(C)
         IMPORT C_INT
         INTEGER (kind = C_INT), VALUE :: step
      END SUBROUTINE xios_update_calendar
      
   END INTERFACE
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
   SUBROUTINE set_timestep(timestep)
      TYPE(XDuration), INTENT(IN):: timestep
      CALL xios_set_timestep(timestep%year, timestep%month , timestep%day, &
                             timestep%hour, timestep%minute, timestep%second)
   END SUBROUTINE set_timestep
   
   SUBROUTINE update_calendar(step)
      INTEGER, INTENT(IN):: step
      IF (step < 1) THEN
         PRINT *, "L'argument 'step' ne peut être négatif ou nul"
         STOP
      END IF
      CALL xios_update_calendar(step)
   END SUBROUTINE update_calendar
   
END MODULE IDATE
