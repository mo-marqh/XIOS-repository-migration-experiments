

MODULE IDOMAIN
   USE, INTRINSIC :: ISO_C_BINDING
   USE DOMAIN_INTERFACE
   USE DOMAINGROUP_INTERFACE
! USE IDOMAIN_ATTR
! USE IDOMAINGROUP_ATTR

   TYPE xios_domain
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_domain

   TYPE xios_domaingroup
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_domaingroup


   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_domain_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_domain), INTENT(OUT):: ret

      CALL cxios_domain_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_domain_handle

   SUBROUTINE xios_get_domaingroup_handle(idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_domaingroup), INTENT(OUT):: ret

      CALL cxios_domaingroup_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_domaingroup_handle

   LOGICAL FUNCTION xios_is_valid_domain(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val
      CALL cxios_domain_valid_id(val, idt, len(idt));
      xios_is_valid_domain = val
   END FUNCTION xios_is_valid_domain

   LOGICAL FUNCTION xios_is_valid_domaingroup(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val
      CALL cxios_domaingroup_valid_id(val, idt, len(idt));
      xios_is_valid_domaingroup = val
   END FUNCTION xios_is_valid_domaingroup

END MODULE IDOMAIN
