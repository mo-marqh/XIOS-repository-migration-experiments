MODULE GRID_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE cxios_set_grid_name(grid_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: name
         INTEGER (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE cxios_set_grid_name

      SUBROUTINE cxios_set_grid_description(grid_hdl, description, description_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: description
         INTEGER (kind = C_INT)     , VALUE        :: description_size
      END SUBROUTINE cxios_set_grid_description

      SUBROUTINE cxios_set_grid_domain_ref(grid_hdl, domain_ref, domain_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: domain_ref
         INTEGER (kind = C_INT)     , VALUE        :: domain_ref_size
      END SUBROUTINE cxios_set_grid_domain_ref

      SUBROUTINE cxios_set_grid_axis_ref(grid_hdl, axis_ref, axis_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
         INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
      END SUBROUTINE cxios_set_grid_axis_ref
      
      SUBROUTINE cxios_grid_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_grid_handle_create

      SUBROUTINE cxios_grid_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_grid_valid_id

   END INTERFACE
     
END MODULE GRID_INTERFACE
