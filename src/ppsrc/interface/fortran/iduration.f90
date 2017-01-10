

MODULE IDURATION
   USE, INTRINSIC :: ISO_C_BINDING
   USE DURATION_INTERFACE

   TYPE(xios_duration), PARAMETER :: xios_year = xios_duration(1, 0, 0, 0, 0, 0, 0)
   TYPE(xios_duration), PARAMETER :: xios_month = xios_duration(0, 1, 0, 0, 0, 0, 0)
   TYPE(xios_duration), PARAMETER :: xios_day = xios_duration(0, 0, 1, 0, 0, 0, 0)
   TYPE(xios_duration), PARAMETER :: xios_hour = xios_duration(0, 0, 0, 1, 0, 0, 0)
   TYPE(xios_duration), PARAMETER :: xios_minute = xios_duration(0, 0, 0, 0, 1, 0, 0)
   TYPE(xios_duration), PARAMETER :: xios_second = xios_duration(0, 0, 0, 0, 0, 1, 0)
   TYPE(xios_duration), PARAMETER :: xios_timestep = xios_duration(0, 0, 0, 0, 0, 0, 1)

   INTERFACE OPERATOR(+)
      MODULE PROCEDURE xios_duration_add
   END INTERFACE

   INTERFACE OPERATOR(-)
      MODULE PROCEDURE xios_duration_sub
      MODULE PROCEDURE xios_duration_neg
   END INTERFACE

   INTERFACE OPERATOR(*)
      MODULE PROCEDURE xios_real4_duration_mult
      MODULE PROCEDURE xios_duration_real4_mult
      MODULE PROCEDURE xios_real8_duration_mult
      MODULE PROCEDURE xios_duration_real8_mult
      MODULE PROCEDURE xios_int_duration_mult
      MODULE PROCEDURE xios_duration_int_mult
   END INTERFACE

   INTERFACE xios_duration_mult
      MODULE PROCEDURE xios_real4_duration_mult
      MODULE PROCEDURE xios_duration_real4_mult
      MODULE PROCEDURE xios_real8_duration_mult
      MODULE PROCEDURE xios_duration_real8_mult
      MODULE PROCEDURE xios_int_duration_mult
      MODULE PROCEDURE xios_duration_int_mult
   END INTERFACE

   INTERFACE OPERATOR(==)
      MODULE PROCEDURE xios_duration_eq
   END INTERFACE

   INTERFACE OPERATOR(/=)
      MODULE PROCEDURE xios_duration_neq
   END INTERFACE

   CONTAINS

   ! Conversion function

   SUBROUTINE xios_duration_convert_to_string(dur, str)
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur
      CHARACTER(len = *), INTENT(OUT) :: str

      CALL cxios_duration_convert_to_string(dur, str, len(str))
   END SUBROUTINE xios_duration_convert_to_string

   FUNCTION xios_duration_convert_from_string(str) RESULT(res)
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: str
      TYPE(xios_duration) :: res

      res = cxios_duration_convert_from_string(str, len(str))
   END FUNCTION xios_duration_convert_from_string

   ! Addition

   FUNCTION xios_duration_add(dur1, dur2) RESULT(res)
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur1, dur2
      TYPE(xios_duration) :: res

      res = cxios_duration_add(dur1, dur2)
   END FUNCTION xios_duration_add

   ! Subtraction

   FUNCTION xios_duration_sub(dur1, dur2) RESULT(res)
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur1, dur2
      TYPE(xios_duration) :: res

      res = cxios_duration_sub(dur1, dur2)
   END FUNCTION xios_duration_sub

   ! Multiplication by a scalar

   FUNCTION xios_real4_duration_mult(val, dur) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      REAL(kind = C_FLOAT), INTENT(IN) :: val
      TYPE(xios_duration), INTENT(IN) :: dur
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(REAL(val, C_DOUBLE), dur)
   END FUNCTION xios_real4_duration_mult

   FUNCTION xios_duration_real4_mult(dur, val2) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur
      REAL(kind = C_FLOAT), INTENT(IN) :: val2
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(REAL(val2, C_DOUBLE), dur)
   END FUNCTION xios_duration_real4_mult

   FUNCTION xios_real8_duration_mult(val, dur) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      REAL(kind = C_DOUBLE), INTENT(IN) :: val
      TYPE(xios_duration), INTENT(IN) :: dur
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(val, dur)
   END FUNCTION xios_real8_duration_mult

   FUNCTION xios_duration_real8_mult(dur, val2) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur
      REAL(kind = C_DOUBLE), INTENT(IN) :: val2
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(val2, dur)
   END FUNCTION xios_duration_real8_mult

   FUNCTION xios_int_duration_mult(val, dur) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: val
      TYPE(xios_duration), INTENT(IN) :: dur
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(REAL(val, C_DOUBLE), dur)
   END FUNCTION xios_int_duration_mult

   FUNCTION xios_duration_int_mult(dur, val2) RESULT(res)
      USE ISO_C_BINDING
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur
      INTEGER, INTENT(IN) :: val2
      TYPE(xios_duration) :: res

      res = cxios_duration_mult(REAL(val2, C_DOUBLE), dur)
   END FUNCTION xios_duration_int_mult

   ! Negation

   FUNCTION xios_duration_neg(dur) RESULT(res)
      USE DURATION_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur
      TYPE(xios_duration) :: res

      res = cxios_duration_neg(dur)
   END FUNCTION xios_duration_neg

   FUNCTION xios_duration_eq(dur1, dur2) RESULT(res)
      USE duration_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur1, dur2
      LOGICAL :: res

      res = cxios_duration_eq(dur1, dur2)
   END FUNCTION xios_duration_eq

   FUNCTION xios_duration_neq(dur1, dur2) RESULT(res)
      USE duration_INTERFACE, only : xios_duration
      IMPLICIT NONE
      TYPE(xios_duration), INTENT(IN) :: dur1, dur2
      LOGICAL :: res

      res = cxios_duration_neq(dur1, dur2)
   END FUNCTION xios_duration_neq

END MODULE IDURATION
