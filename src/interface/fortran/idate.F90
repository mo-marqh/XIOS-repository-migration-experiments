#include "xios_fortran_prefix.hpp"
MODULE IDATE
   USE, INTRINSIC :: ISO_C_BINDING
   USE DATE_INTERFACE

   INTERFACE OPERATOR(+)
      MODULE PROCEDURE xios(date_add_duration)
   END INTERFACE

   INTERFACE OPERATOR(-)
      MODULE PROCEDURE xios(date_sub_duration)
      MODULE PROCEDURE xios(date_sub)
   END INTERFACE

   INTERFACE OPERATOR(==)
      MODULE PROCEDURE xios(date_eq)
   END INTERFACE

   INTERFACE OPERATOR(/=)
      MODULE PROCEDURE xios(date_neq)
   END INTERFACE

   INTERFACE OPERATOR(<)
      MODULE PROCEDURE xios(date_lt)
   END INTERFACE

   INTERFACE OPERATOR(<=)
      MODULE PROCEDURE xios(date_le)
   END INTERFACE

   INTERFACE OPERATOR(>)
      MODULE PROCEDURE xios(date_gt)
   END INTERFACE

   INTERFACE OPERATOR(>=)
      MODULE PROCEDURE xios(date_ge)
   END INTERFACE

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   FUNCTION xios(date_convert_to_seconds)(date) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date
      INTEGER(kind = C_LONG_LONG) :: res

      res = cxios_date_convert_to_seconds(date)
   END FUNCTION xios(date_convert_to_seconds)

   ! Addition: date + duration = date

   FUNCTION xios(date_add_duration)(date, dur) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      USE IDURATION, only : txios(duration)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date
      TYPE(txios(duration)), INTENT(IN) :: dur
      TYPE(txios(date)) :: res

      res = cxios_date_add_duration(date, dur)
   END FUNCTION xios(date_add_duration)

   ! Subtraction: date - duration = date

   FUNCTION xios(date_sub_duration)(date, dur) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      USE IDURATION, only : txios(duration)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date
      TYPE(txios(duration)), INTENT(IN) :: dur
      TYPE(txios(date)) :: res

      res = cxios_date_sub_duration(date, dur)
   END FUNCTION xios(date_sub_duration)

   ! Subtraction: date - date = duration

   FUNCTION xios(date_sub)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      USE IDURATION, only : txios(duration)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      TYPE(txios(duration)) :: res

      res = cxios_date_sub(date1, date2)
   END FUNCTION xios(date_sub)

   FUNCTION xios(date_eq)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_eq(date1, date2)
   END FUNCTION xios(date_eq)

   FUNCTION xios(date_neq)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_neq(date1, date2)
   END FUNCTION xios(date_neq)

   FUNCTION xios(date_lt)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_lt(date1, date2)
   END FUNCTION xios(date_lt)

   FUNCTION xios(date_le)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_le(date1, date2)
   END FUNCTION xios(date_le)

   FUNCTION xios(date_gt)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_gt(date1, date2)
   END FUNCTION xios(date_gt)

   FUNCTION xios(date_ge)(date1, date2) RESULT(res)
      USE DATE_INTERFACE, only : txios(date)
      IMPLICIT NONE
      TYPE(txios(date)), INTENT(IN) :: date1, date2
      LOGICAL :: res

      res = cxios_date_ge(date1, date2)
   END FUNCTION xios(date_ge)

END MODULE IDATE
