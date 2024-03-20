#include "xios_fortran_prefix.hpp"

MODULE IREDISTRIBUTE_AXIS
   USE, INTRINSIC :: ISO_C_BINDING
   USE REDISTRIBUTE_AXIS_INTERFACE
   USE LOGICAL_BOOL_CONVERSION

   TYPE txios(redistribute_axis)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(redistribute_axis)

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_redistribute_axis_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(txios(redistribute_axis)) , INTENT(OUT):: ret
      CALL cxios_redistribute_axis_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios(get_redistribute_axis_handle)

   LOGICAL FUNCTION xios(is_valid_redistribute_axis)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_redistribute_axis_valid_id(val, idt, len(idt))
      CALL xios_bool_to_logical_0d(val)
      xios(is_valid_redistribute_axis) = val

   END FUNCTION  xios(is_valid_redistribute_axis)

END MODULE IREDISTRIBUTE_AXIS
