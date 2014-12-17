MODULE CALENDAR_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99

      SUBROUTINE cxios_create_calendar() BIND(C)
      END SUBROUTINE cxios_create_calendar

      SUBROUTINE cxios_update_calendar(step) BIND(C)
         IMPORT C_INT
         INTEGER (kind = C_INT), VALUE :: step
      END SUBROUTINE cxios_update_calendar

   END INTERFACE
     
END MODULE CALENDAR_INTERFACE
