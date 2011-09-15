MODULE GRID_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_set_grid_name(grid_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: name
         INTEGER (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_grid_name

      SUBROUTINE xios_set_grid_description(grid_hdl, description, description_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: description
         INTEGER (kind = C_INT)     , VALUE        :: description_size
      END SUBROUTINE xios_set_grid_description

      SUBROUTINE xios_set_grid_domain_ref(grid_hdl, domain_ref, domain_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: domain_ref
         INTEGER (kind = C_INT)     , VALUE        :: domain_ref_size
      END SUBROUTINE xios_set_grid_domain_ref

      SUBROUTINE xios_set_grid_axis_ref(grid_hdl, axis_ref, axis_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: grid_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
         INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
      END SUBROUTINE xios_set_grid_axis_ref
      
      SUBROUTINE xios_grid_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_grid_handle_create
     
   END INTERFACE
     
END MODULE GRID_INTERFACE
