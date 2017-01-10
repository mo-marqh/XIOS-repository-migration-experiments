

MODULE IGRID
   USE, INTRINSIC :: ISO_C_BINDING
   USE GRID_INTERFACE
   USE GRIDGROUP_INTERFACE
! USE IGRID_ATTR
! USE IGRIDGROUP_ATTR

   TYPE xios_grid
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_grid

   TYPE xios_gridgroup
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE xios_gridgroup


   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_get_grid_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt
      TYPE(xios_grid), INTENT(OUT):: ret

      CALL cxios_grid_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_grid_handle

   SUBROUTINE xios_get_gridgroup_handle(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      TYPE(xios_gridgroup) , INTENT(OUT):: ret

      CALL cxios_gridgroup_handle_create(ret%daddr, idt, len(idt))

   END SUBROUTINE xios_get_gridgroup_handle

   LOGICAL FUNCTION xios_is_valid_grid(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_grid_valid_id(val, idt, len(idt));
      xios_is_valid_grid = val

   END FUNCTION xios_is_valid_grid

   LOGICAL FUNCTION xios_is_valid_gridgroup(idt)
      IMPLICIT NONE
      CHARACTER(len = *) , INTENT(IN) :: idt
      LOGICAL (kind = 1) :: val

      CALL cxios_gridgroup_valid_id(val, idt, len(idt));
      xios_is_valid_gridgroup = val

   END FUNCTION xios_is_valid_gridgroup


END MODULE IGRID
