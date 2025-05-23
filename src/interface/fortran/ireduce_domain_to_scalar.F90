#include "xios_fortran_prefix.hpp"

MODULE IREDUCE_DOMAIN_TO_SCALAR
   USE, INTRINSIC :: ISO_C_BINDING
   USE REDUCE_DOMAIN_TO_SCALAR_INTERFACE
   USE LOGICAL_BOOL_CONVERSION

   TYPE txios(reduce_domain_to_scalar)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(reduce_domain_to_scalar)

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_reduce_domain_to_scalar_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(txios(reduce_domain_to_scalar)) , INTENT(OUT):: ret
      CALL cxios_reduce_domain_to_scalar_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios(get_reduce_domain_to_scalar_handle)

   LOGICAL FUNCTION xios(is_valid_reduce_domain_to_scalar)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_reduce_domain_to_scalar_valid_id(val, idt, len(idt))
      CALL xios_bool_to_logical_0d(val)
      xios(is_valid_reduce_domain_to_scalar) = val

   END FUNCTION  xios(is_valid_reduce_domain_to_scalar)

END MODULE IREDUCE_DOMAIN_TO_SCALAR
