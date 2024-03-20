#include "xios_fortran_prefix.hpp"

MODULE IREDISTRIBUTE_DOMAIN
   USE, INTRINSIC :: ISO_C_BINDING
   USE REDISTRIBUTE_DOMAIN_INTERFACE
   USE LOGICAL_BOOL_CONVERSION

   TYPE txios(redistribute_domain)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(redistribute_domain)

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_redistribute_domain_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(txios(redistribute_domain)) , INTENT(OUT):: ret
      CALL cxios_redistribute_domain_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios(get_redistribute_domain_handle)

   LOGICAL FUNCTION xios(is_valid_redistribute_domain)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_redistribute_domain_valid_id(val, idt, len(idt))
      CALL xios_bool_to_logical_0d(val)
      xios(is_valid_redistribute_domain) = val

   END FUNCTION  xios(is_valid_redistribute_domain)

END MODULE IREDISTRIBUTE_DOMAIN
