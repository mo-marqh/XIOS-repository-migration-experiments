
MODULE ICALENDAR
   USE, INTRINSIC :: ISO_C_BINDING
   USE CALENDAR_INTERFACE
   USE ICALENDAR_WRAPPER
   USE IDATE
   USE IDURATION

   ! enum XCalendarType
   INTEGER(kind = C_INT), PARAMETER :: D360 = 0 , ALLLEAP = 1 , NOLEAP = 2 , JULIAN = 3 , GREGORIAN = 4

   INTERFACE xios_set_start_date
      MODULE PROCEDURE xios_set_start_date_date, xios_set_start_date_dur
   END INTERFACE xios_set_start_date

   INTERFACE xios_set_time_origin
      MODULE PROCEDURE xios_set_time_origin_date, xios_set_time_origin_dur
   END INTERFACE xios_set_time_origin

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_define_calendar(type, timestep, start_date, time_origin, &
                                    day_length, month_lengths, year_length, &
                                    leap_year_month, leap_year_drift, leap_year_drift_offset)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE icalendar_wrapper_attr, ONLY : xios_set_calendar_wrapper_attr_hdl
      USE IDURATION, ONLY : xios_duration
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: type
      TYPE(xios_duration), OPTIONAL, INTENT(IN) :: timestep
      TYPE(xios_date), OPTIONAL, INTENT(IN) :: start_date
      TYPE(xios_date), OPTIONAL, INTENT(IN) :: time_origin
      INTEGER, OPTIONAL, INTENT(IN) :: day_length
      INTEGER, OPTIONAL, INTENT(IN) :: month_lengths(:)
      INTEGER, OPTIONAL, INTENT(IN) :: year_length
      REAL (KIND=8), OPTIONAL, INTENT(IN) :: leap_year_drift
      REAL (KIND=8), OPTIONAL, INTENT(IN) :: leap_year_drift_offset
      INTEGER, OPTIONAL, INTENT(IN) :: leap_year_month
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, type=type)
      IF (PRESENT(timestep)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, timestep=timestep)
      END IF
      IF (PRESENT(day_length)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, day_length=day_length)
      END IF
      IF (PRESENT(month_lengths)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, month_lengths=month_lengths)
      END IF
      IF (PRESENT(year_length)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, year_length=year_length)
      END IF
      IF (PRESENT(leap_year_month)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, leap_year_month=leap_year_month)
      END IF
      IF (PRESENT(leap_year_drift)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, leap_year_drift=leap_year_drift)
      END IF
      IF (PRESENT(leap_year_drift_offset)) THEN
         CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, leap_year_drift_offset=leap_year_drift_offset)
      END IF

      CALL xios_create_calendar(calendar_wrapper)

      IF (PRESENT(start_date)) THEN
         CALL xios_set_start_date_hdl(calendar_wrapper, start_date=start_date)
      END IF
      IF (PRESENT(time_origin)) THEN
         CALL xios_set_time_origin_hdl(calendar_wrapper, time_origin=time_origin)
      END IF
   END SUBROUTINE xios_define_calendar

   SUBROUTINE xios_get_calendar_type(calendar_type)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE icalendar_wrapper_attr, ONLY : xios_get_calendar_wrapper_attr_hdl
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(OUT) :: calendar_type
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_get_calendar_wrapper_attr_hdl(calendar_wrapper, type=calendar_type)
   END SUBROUTINE xios_get_calendar_type

   SUBROUTINE xios_set_timestep(timestep)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE icalendar_wrapper_attr, ONLY : xios_set_calendar_wrapper_attr_hdl
      USE IDURATION, ONLY : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: timestep
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_set_calendar_wrapper_attr_hdl(calendar_wrapper, timestep=timestep)

      CALL xios_update_calendar_timestep(calendar_wrapper)
   END SUBROUTINE xios_set_timestep

   SUBROUTINE xios_get_timestep(timestep)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE icalendar_wrapper_attr, ONLY : xios_get_calendar_wrapper_attr_hdl
      USE IDURATION, ONLY : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(OUT) :: timestep
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_get_calendar_wrapper_attr_hdl(calendar_wrapper, timestep=timestep)
   END SUBROUTINE xios_get_timestep

   SUBROUTINE xios_set_start_date_date(start_date)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_date), INTENT(IN) :: start_date
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_set_start_date_hdl(calendar_wrapper, start_date)
   END SUBROUTINE xios_set_start_date_date

   SUBROUTINE xios_set_start_date_dur(start_date)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDURATION, ONLY : xios_duration
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: start_date
      TYPE(xios_calendar_wrapper) :: calendar_wrapper
      TYPE(xios_date) :: start_date_date

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      start_date_date = start_date
      CALL xios_set_start_date_hdl(calendar_wrapper, start_date_date)
   END SUBROUTINE xios_set_start_date_dur

   SUBROUTINE xios_get_start_date(start_date)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_date), INTENT(OUT) :: start_date
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_get_start_date_hdl(calendar_wrapper, start_date)
   END SUBROUTINE xios_get_start_date

   SUBROUTINE xios_set_time_origin_date(time_origin)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_date), INTENT(IN) :: time_origin
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_set_time_origin_hdl(calendar_wrapper, time_origin)
   END SUBROUTINE xios_set_time_origin_date

   SUBROUTINE xios_set_time_origin_dur(time_origin)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDURATION, ONLY : xios_duration
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: time_origin
      TYPE(xios_calendar_wrapper) :: calendar_wrapper
      TYPE(xios_date) :: time_origin_date

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      time_origin_date = time_origin
      CALL xios_set_time_origin_hdl(calendar_wrapper, time_origin_date)
   END SUBROUTINE xios_set_time_origin_dur

   SUBROUTINE xios_get_time_origin(time_origin)
      USE ICALENDAR_WRAPPER, ONLY : xios_calendar_wrapper, xios_get_default_calendar_wrapper_handle
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_date), INTENT(OUT) :: time_origin
      TYPE(xios_calendar_wrapper) :: calendar_wrapper

      CALL xios_get_default_calendar_wrapper_handle(calendar_wrapper)

      CALL xios_get_time_origin_hdl(calendar_wrapper, time_origin)
   END SUBROUTINE xios_get_time_origin

   SUBROUTINE xios_update_calendar(step)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: step

      IF (step < 0) THEN
         PRINT *, "L'argument 'step' ne peut être négatif"
         STOP
      END IF
      CALL cxios_update_calendar(step)
   END SUBROUTINE xios_update_calendar

   SUBROUTINE xios_get_current_date(current_date)
      USE IDATE, ONLY : xios_date
      IMPLICIT NONE
      TYPE(xios_date), INTENT(OUT) :: current_date

      CALL cxios_get_current_date(current_date)
   END SUBROUTINE xios_get_current_date

   FUNCTION xios_get_year_length_in_seconds(year) RESULT(res)
      IMPLICIT NONE
      INTEGER(kind = C_INT), INTENT(IN) :: year
      INTEGER(kind = C_INT) :: res

      res = cxios_get_year_length_in_seconds(year)
   END FUNCTION xios_get_year_length_in_seconds

   FUNCTION xios_get_day_length_in_seconds() RESULT(res)
      IMPLICIT NONE
      INTEGER(kind = C_INT) :: res

      res = cxios_get_day_length_in_seconds()
   END FUNCTION xios_get_day_length_in_seconds

END MODULE ICALENDAR
