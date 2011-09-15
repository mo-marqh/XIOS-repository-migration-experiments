MODULE IFIELD
   USE, INTRINSIC :: ISO_C_BINDING
   USE FIELD_INTERFACE
   USE FIELDGROUP_INTERFACE
   
   TYPE XFieldHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XFieldHandle
   
   TYPE XFieldGroupHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XFieldGroupHandle
   
   !----------------------------------------------------------------------------
   INTERFACE set_field_attributes
      MODULE PROCEDURE set_field_attributes_id,set_field_attributes_hdl
   END INTERFACE  
   
   INTERFACE set_field_group_attributes
      MODULE PROCEDURE set_fieldgroup_attributes_id,set_fieldgroup_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
    
   SUBROUTINE set_fieldgroup_attributes_id                                                 &
   (fieldgroup_id, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
    prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, fieldgroup_ref_, default_value_)
    
      IMPLICIT NONE
      TYPE(XFieldGroupHandle)                       :: fieldgroup_hdl
      CHARACTER(len = *)               , INTENT(IN) :: fieldgroup_id
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: unit_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: operation_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: freq_op_
      INTEGER                , OPTIONAL, INTENT(IN) :: level_
      INTEGER                , OPTIONAL, INTENT(IN) :: prec_
      LOGICAL                , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: grid_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: fieldgroup_ref_
      REAL(kind=8)           , OPTIONAL, INTENT(IN) :: default_value_
        
      CALL fieldgroup_handle_create(fieldgroup_hdl, fieldgroup_id)
      CALL set_fieldgroup_attributes_hdl                                                          &
         (fieldgroup_hdl, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
          prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, fieldgroup_ref_, default_value_)

   END SUBROUTINE set_fieldgroup_attributes_id

   SUBROUTINE set_fieldgroup_attributes_hdl                                                 &
   (fieldgroup_hdl, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
    prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, fieldgroup_ref_, default_value_)
      IMPLICIT NONE
      TYPE(XFieldgroupHandle)          , INTENT(IN) :: fieldgroup_hdl
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: unit_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: operation_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: freq_op_
      INTEGER                , OPTIONAL, INTENT(IN) :: level_
      INTEGER                , OPTIONAL, INTENT(IN) :: prec_
      LOGICAL(kind = 1)                             :: enabled__
      LOGICAL                , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: grid_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: fieldgroup_ref_
      REAL(kind=8)           , OPTIONAL, INTENT(IN) :: default_value_
      
      enabled__ = enabled_  
      IF (PRESENT(name_))           THEN
         CALL xios_set_fieldgroup_name(fieldgroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))  THEN
         CALL xios_set_fieldgroup_standard_name(fieldgroup_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))      THEN
         CALL xios_set_fieldgroup_long_name(fieldgroup_hdl%daddr, long_name_, len(long_name_))
      END IF
      IF (PRESENT(unit_))           THEN
         CALL xios_set_fieldgroup_unit(fieldgroup_hdl%daddr, unit_, len(unit_))
      END IF
      IF (PRESENT(operation_))      THEN
         CALL xios_set_fieldgroup_operation(fieldgroup_hdl%daddr, operation_, len(operation_))
      END IF
      IF (PRESENT(freq_op_))        THEN
         CALL xios_set_fieldgroup_freq_op(fieldgroup_hdl%daddr, freq_op_, len(freq_op_))
      END IF
      IF (PRESENT(level_))          THEN
         CALL xios_set_fieldgroup_level(fieldgroup_hdl%daddr, level_)
      END IF
      IF (PRESENT(prec_))           THEN
         CALL xios_set_fieldgroup_prec(fieldgroup_hdl%daddr, prec_)
      END IF
      IF (PRESENT(enabled_))        THEN
         CALL xios_set_fieldgroup_enabled(fieldgroup_hdl%daddr, enabled__)
      END IF
      IF (PRESENT(domain_ref_))     THEN
         CALL xios_set_fieldgroup_domain_ref(fieldgroup_hdl%daddr, domain_ref_, len(domain_ref_))
      END IF
      IF (PRESENT(axis_ref_))       THEN
         CALL xios_set_fieldgroup_axis_ref(fieldgroup_hdl%daddr, axis_ref_, len(axis_ref_))
      END IF
      IF (PRESENT(grid_ref_))       THEN
         CALL xios_set_fieldgroup_grid_ref(fieldgroup_hdl%daddr, grid_ref_, len(grid_ref_))
      END IF
      IF (PRESENT(fieldgroup_ref_))      THEN
         CALL xios_set_fieldgroup_fieldgroup_ref(fieldgroup_hdl%daddr, fieldgroup_ref_, len(fieldgroup_ref_))
      END IF
      IF (PRESENT(default_value_))  THEN
         CALL xios_set_fieldgroup_default_value(fieldgroup_hdl%daddr, default_value_)
      END IF

   END SUBROUTINE set_fieldgroup_attributes_hdl
   
   SUBROUTINE set_field_attributes_id                                                 &
   (field_id, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
    prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, field_ref_, default_value_)
    
      IMPLICIT NONE
      TYPE(XFieldHandle)                            :: field_hdl
      CHARACTER(len = *)               , INTENT(IN) :: field_id
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: unit_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: operation_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: freq_op_
      INTEGER                , OPTIONAL, INTENT(IN) :: level_
      INTEGER                , OPTIONAL, INTENT(IN) :: prec_
      LOGICAL                , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: grid_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: field_ref_
      REAL(kind=8)           , OPTIONAL, INTENT(IN) :: default_value_
      
      CALL field_handle_create(field_hdl, field_id)
      CALL set_field_attributes_hdl                                                          &
         (field_hdl, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
          prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, field_ref_, default_value_)

   END SUBROUTINE set_field_attributes_id

   SUBROUTINE set_field_attributes_hdl                                                 &
   (field_hdl, name_, standard_name_, long_name_, unit_, operation_, freq_op_, level_, &
    prec_, enabled_, domain_ref_, axis_ref_, grid_ref_, field_ref_, default_value_)
      IMPLICIT NONE
      TYPE(XFieldHandle)               , INTENT(IN) :: field_hdl
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: unit_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: operation_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: freq_op_
      INTEGER                , OPTIONAL, INTENT(IN) :: level_
      INTEGER                , OPTIONAL, INTENT(IN) :: prec_
      LOGICAL(kind = 1)                             :: enabled__
      LOGICAL                , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: domain_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: axis_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: grid_ref_
      CHARACTER(len = *)     , OPTIONAL, INTENT(IN) :: field_ref_
      REAL(kind=8)           , OPTIONAL, INTENT(IN) :: default_value_
      
      enabled__ = enabled_  
      IF (PRESENT(name_))           THEN
         CALL xios_set_field_name(field_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))  THEN
         CALL xios_set_field_standard_name(field_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))      THEN
         CALL xios_set_field_long_name(field_hdl%daddr, long_name_, len(long_name_))
      END IF
      IF (PRESENT(unit_))           THEN
         CALL xios_set_field_unit(field_hdl%daddr, unit_, len(unit_))
      END IF
      IF (PRESENT(operation_))      THEN
         CALL xios_set_field_operation(field_hdl%daddr, operation_, len(operation_))
      END IF
      IF (PRESENT(freq_op_))        THEN
         CALL xios_set_field_freq_op(field_hdl%daddr, freq_op_, len(freq_op_))
      END IF
      IF (PRESENT(level_))          THEN
         CALL xios_set_field_level(field_hdl%daddr, level_)
      END IF
      IF (PRESENT(prec_))           THEN
         CALL xios_set_field_prec(field_hdl%daddr, prec_)
      END IF
      IF (PRESENT(enabled_))        THEN
         CALL xios_set_field_enabled(field_hdl%daddr, enabled__)
      END IF
      IF (PRESENT(domain_ref_))     THEN
         CALL xios_set_field_domain_ref(field_hdl%daddr, domain_ref_, len(domain_ref_))
      END IF
      IF (PRESENT(axis_ref_))       THEN
         CALL xios_set_field_axis_ref(field_hdl%daddr, axis_ref_, len(axis_ref_))
      END IF
      IF (PRESENT(grid_ref_))       THEN
         CALL xios_set_field_grid_ref(field_hdl%daddr, grid_ref_, len(grid_ref_))
      END IF
      IF (PRESENT(field_ref_))      THEN
         CALL xios_set_field_field_ref(field_hdl%daddr, field_ref_, len(field_ref_))
      END IF
      IF (PRESENT(default_value_))  THEN
         CALL xios_set_field_default_value(field_hdl%daddr, default_value_)
      END IF

   END SUBROUTINE set_field_attributes_hdl

   SUBROUTINE field_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFieldHandle), INTENT(OUT):: ret
      CHARACTER(len = *), INTENT(IN) :: idt      
      CALL xios_field_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE field_handle_create
   
   SUBROUTINE fieldgroup_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFieldGroupHandle), INTENT(OUT):: ret
      CHARACTER(len = *)     , INTENT(IN) :: idt      
      CALL xios_fieldgroup_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE fieldgroup_handle_create
   
END MODULE IFIELD
