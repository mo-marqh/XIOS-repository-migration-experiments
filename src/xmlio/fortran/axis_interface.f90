MODULE AXIS_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_set_axis_name(axis_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axis_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
         INTEGER  (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_axis_name

      SUBROUTINE xios_set_axis_standard_name(axis_hdl, standard_name, standard_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axis_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
         INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
      END SUBROUTINE xios_set_axis_standard_name

      SUBROUTINE xios_set_axis_long_name(axis_hdl, long_name, long_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE       :: axis_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
         INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
      END SUBROUTINE xios_set_axis_long_name

      SUBROUTINE xios_set_axis_unit(axis_hdl, unit, unit_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE       :: axis_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
         INTEGER  (kind = C_INT)     , VALUE        :: unit_size
      END SUBROUTINE xios_set_axis_unit

      SUBROUTINE xios_set_axis_size(axis_hdl, size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: axis_hdl
         INTEGER (kind = C_INT)     , VALUE :: size
      END SUBROUTINE xios_set_axis_size

      SUBROUTINE xios_set_axis_zvalue(axis_hdl, zvalue, zvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: axis_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: zvalue
         INTEGER (kind = C_INT)     , VALUE        :: zvalue_extent1
      END SUBROUTINE xios_set_axis_zvalue
      
      SUBROUTINE xios_axis_handle_create(ret, idt, idt_size) BIND(C)
         import C_CHAR, C_INTPTR_T, C_INT
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_axis_handle_create
       
   END INTERFACE
     
END MODULE AXIS_INTERFACE
