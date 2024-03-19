#include "xios_fortran_prefix.hpp"

MODULE IREDISTRIBUTE_SCALAR
   USE, INTRINSIC :: ISO_C_BINDING
   USE REDISTRIBUTE_SCALAR_INTERFACE

   TYPE txios(redistribute_scalar)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(redistribute_scalar)

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_redistribute_scalar_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(txios(redistribute_scalar)) , INTENT(OUT):: ret
      CALL cxios_redistribute_scalar_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios(get_redistribute_scalar_handle)

   LOGICAL FUNCTION xios(is_valid_redistribute_scalar)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_redistribute_scalar_valid_id(val, idt, len(idt))
      xios(is_valid_redistribute_scalar) = val

   END FUNCTION  xios(is_valid_redistribute_scalar)

END MODULE IREDISTRIBUTE_SCALAR
