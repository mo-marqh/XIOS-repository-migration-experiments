#include "xios_fortran_prefix.hpp"

MODULE IEXTRACT_DOMAIN
   USE, INTRINSIC :: ISO_C_BINDING
   USE EXTRACT_DOMAIN_INTERFACE
   USE LOGICAL_BOOL_CONVERSION

   TYPE txios(extract_domain)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(extract_domain)

   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios(get_extract_domain_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(txios(extract_domain)) , INTENT(OUT):: ret
      CALL cxios_extract_domain_handle_create(ret%daddr, idt, len(idt))
   END SUBROUTINE xios(get_extract_domain_handle)

   LOGICAL FUNCTION xios(is_valid_extract_domain)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_extract_domain_valid_id(val, idt, len(idt))
      CALL xios_bool_to_logical_0d(val)
      xios(is_valid_extract_domain) = val

   END FUNCTION  xios(is_valid_extract_domain)

END MODULE IEXTRACT_DOMAIN
