MODULE FIELD_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
   
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
   
      SUBROUTINE xios_set_field_name(field_hdl, name ,name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
         INTEGER  (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_field_name

      SUBROUTINE xios_set_field_standard_name(field_hdl, standard_name ,standard_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
         INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
      END SUBROUTINE xios_set_field_standard_name

      SUBROUTINE xios_set_field_long_name(field_hdl, long_name ,long_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)   , DIMENSION(*) :: long_name
         INTEGER (kind = C_INT)     , VALUE        :: long_name_size
      END SUBROUTINE xios_set_field_long_name

      SUBROUTINE xios_set_field_unit(field_hdl, unit ,unit_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
         INTEGER  (kind = C_INT)     , VALUE        :: unit_size
      END SUBROUTINE xios_set_field_unit

      SUBROUTINE xios_set_field_operation(field_hdl, operation ,operation_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: operation
         INTEGER  (kind = C_INT)     , VALUE        :: operation_size
      END SUBROUTINE xios_set_field_operation

      SUBROUTINE xios_set_field_freq_op(field_hdl, freq_op ,freq_op_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: freq_op
         INTEGER  (kind = C_INT)     , VALUE        :: freq_op_size
      END SUBROUTINE xios_set_field_freq_op

      SUBROUTINE xios_set_field_level(field_hdl, level) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
         INTEGER (kind = C_INT)     , VALUE :: level
      END SUBROUTINE xios_set_field_level

      SUBROUTINE xios_set_field_prec(field_hdl, prec) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
         INTEGER (kind = C_INT)     , VALUE :: prec
      END SUBROUTINE xios_set_field_prec

      SUBROUTINE xios_set_field_enabled(field_hdl, enabled) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
         LOGICAL (kind = C_BOOL)    , VALUE :: enabled
         END SUBROUTINE xios_set_field_enabled

      SUBROUTINE xios_set_field_domain_ref(field_hdl, domain_ref ,domain_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_ref
         INTEGER  (kind = C_INT)     , VALUE        :: domain_ref_size
      END SUBROUTINE xios_set_field_domain_ref

      SUBROUTINE xios_set_field_axis_ref(field_hdl, axis_ref ,axis_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: axis_ref
         INTEGER  (kind = C_INT)     , VALUE        :: axis_ref_size
      END SUBROUTINE xios_set_field_axis_ref

      SUBROUTINE xios_set_field_grid_ref(field_hdl, grid_ref ,grid_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: grid_ref
         INTEGER  (kind = C_INT)     , VALUE        :: grid_ref_size
      END SUBROUTINE xios_set_field_grid_ref

      SUBROUTINE xios_set_field_field_ref(field_hdl, field_ref ,field_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: field_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: field_ref
         INTEGER  (kind = C_INT)     , VALUE        :: field_ref_size
      END SUBROUTINE xios_set_field_field_ref

      SUBROUTINE xios_set_field_default_value(field_hdl, default_value) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: field_hdl
         REAL    (kind = C_DOUBLE)  , VALUE :: default_value
      END SUBROUTINE xios_set_field_default_value
   
      SUBROUTINE xios_field_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_field_handle_create

      SUBROUTINE xios_field_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_field_valid_id

   END INTERFACE
   
END MODULE FIELD_INTERFACE
