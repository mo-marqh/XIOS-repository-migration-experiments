MODULE IGRID
   USE, INTRINSIC :: ISO_C_BINDING
   USE GRID_INTERFACE
   USE GRIDGROUP_INTERFACE
   
   TYPE XGridHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XGridHandle
   
   TYPE XGridGroupHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XGridGroupHandle
   
   !----------------------------------------------------------------------------
   INTERFACE set_grid_attributes
      MODULE PROCEDURE set_grid_attributes_id,set_grid_attributes_hdl
   END INTERFACE  
   
   INTERFACE set_grid_group_attributes
      MODULE PROCEDURE set_gridgroup_attributes_id,set_gridgroup_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
   SUBROUTINE set_grid_attributes_id(grid_id, name_, description_, domain_ref_, axis_ref_)
      IMPLICIT NONE
      TYPE(XGridHandle)                             :: grid_hdl
      CHARACTER(len = *)               , INTENT(IN) :: grid_id
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      
      CALL grid_handle_create(grid_hdl, grid_id)
      CALL set_grid_attributes_hdl(grid_hdl, name_, description_, domain_ref_, axis_ref_)

   END SUBROUTINE set_grid_attributes_id

   SUBROUTINE set_grid_attributes_hdl(grid_hdl, name_, description_, domain_ref_, axis_ref_)
      IMPLICIT NONE
      TYPE      (XGridHandle)                       :: grid_hdl
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      
      IF (PRESENT(name_))        THEN
       CALL xios_set_grid_name(grid_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_)) THEN
       CALL xios_set_grid_description(grid_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(domain_ref_))  THEN
       CALL xios_set_grid_domain_ref(grid_hdl%daddr, domain_ref_, len(domain_ref_))
      END IF
      IF (PRESENT(axis_ref_))    THEN
       CALL xios_set_grid_axis_ref(grid_hdl%daddr, axis_ref_, len(axis_ref_))
      END IF
   END SUBROUTINE set_grid_attributes_hdl
   
   SUBROUTINE set_gridgroup_attributes_id(gridgroup_id, name_, description_, domain_ref_, axis_ref_)
      IMPLICIT NONE
      TYPE(XGridGroupHandle)                        :: gridgroup_hdl
      CHARACTER(len = *)               , INTENT(IN) :: gridgroup_id
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      
      CALL gridgroup_handle_create(gridgroup_hdl, gridgroup_id)
      CALL set_gridgroup_attributes_hdl(gridgroup_hdl, name_, description_, domain_ref_, axis_ref_)

   END SUBROUTINE set_gridgroup_attributes_id

   SUBROUTINE set_gridgroup_attributes_hdl(gridgroup_hdl, name_, description_, domain_ref_, axis_ref_)
      IMPLICIT NONE
      TYPE      (XGridGroupHandle)                  :: gridgroup_hdl
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      
      IF (PRESENT(name_))        THEN
       CALL xios_set_gridgroup_name(gridgroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_)) THEN
       CALL xios_set_gridgroup_description(gridgroup_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(domain_ref_))  THEN
       CALL xios_set_gridgroup_domain_ref(gridgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      END IF
      IF (PRESENT(axis_ref_))    THEN
       CALL xios_set_gridgroup_axis_ref(gridgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      END IF
   END SUBROUTINE set_gridgroup_attributes_hdl

   SUBROUTINE grid_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XGridHandle), INTENT(OUT):: ret
      CHARACTER(len = *), INTENT(IN) :: idt      
      CALL xios_grid_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE grid_handle_create
   
   SUBROUTINE gridgroup_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XGridGroupHandle), INTENT(OUT):: ret
      CHARACTER(len = *)     , INTENT(IN) :: idt      
      CALL xios_gridgroup_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE gridgroup_handle_create
   
END MODULE IGRID
