MODULE AXISGROUP_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_set_axisgroup_name(axisgroup_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axisgroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
         INTEGER  (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_axisgroup_name

      SUBROUTINE xios_set_axisgroup_standard_name(axisgroup_hdl, standard_name, standard_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axisgroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
         INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
      END SUBROUTINE xios_set_axisgroup_standard_name

      SUBROUTINE xios_set_axisgroup_long_name(axisgroup_hdl, long_name, long_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axisgroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
         INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
      END SUBROUTINE xios_set_axisgroup_long_name

      SUBROUTINE xios_set_axisgroup_unit(axisgroup_hdl, unit, unit_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: axisgroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
         INTEGER  (kind = C_INT)     , VALUE        :: unit_size
      END SUBROUTINE xios_set_axisgroup_unit

      SUBROUTINE xios_set_axisgroup_size(axisgroup_hdl, size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: size
      END SUBROUTINE xios_set_axisgroup_size

      SUBROUTINE xios_set_axisgroup_zvalue(axisgroup_hdl, zvalue, zvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: axisgroup_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: zvalue
         INTEGER (kind = C_INT)     , VALUE        :: zvalue_extent1
      END SUBROUTINE xios_set_axisgroup_zvalue
  
      SUBROUTINE xios_axisgroup_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_axisgroup_handle_create
     
   END INTERFACE
     
END MODULE AXISGROUP_INTERFACE
