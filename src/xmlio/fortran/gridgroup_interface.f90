MODULE GRIDGROUP_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_set_gridgroup_name(gridgroup_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: gridgroup_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: name
         INTEGER (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_gridgroup_name

      SUBROUTINE xios_set_gridgroup_description(gridgroup_hdl, description, description_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: gridgroup_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: description
         INTEGER (kind = C_INT)     , VALUE        :: description_size
      END SUBROUTINE xios_set_gridgroup_description

      SUBROUTINE xios_set_gridgroup_domain_ref(gridgroup_hdl, domain_ref, domain_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: gridgroup_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: domain_ref
         INTEGER (kind = C_INT)     , VALUE        :: domain_ref_size
      END SUBROUTINE xios_set_gridgroup_domain_ref

      SUBROUTINE xios_set_gridgroup_axis_ref(gridgroup_hdl, axis_ref, axis_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: gridgroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
         INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
      END SUBROUTINE xios_set_gridgroup_axis_ref
      
      SUBROUTINE xios_domaingroup_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_domaingroup_handle_create

      SUBROUTINE xios_gridgroup_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_gridgroup_valid_id

   END INTERFACE
     
END MODULE GRIDGROUP_INTERFACE
