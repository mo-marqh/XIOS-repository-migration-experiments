MODULE CONTEXT_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_set_context_calendar_type(context_hdl, calendar_type ,calendar_type_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: context_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: calendar_type
         INTEGER  (kind = C_INT)     , VALUE        :: calendar_type_size
      END SUBROUTINE xios_set_context_calendar_type

      SUBROUTINE xios_set_context_start_date(context_hdl, start_date ,start_date_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: context_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: start_date
         INTEGER  (kind = C_INT)     , VALUE        :: start_date_size
      END SUBROUTINE xios_set_context_start_date

      SUBROUTINE xios_set_context_output_dir(context_hdl, output_dir ,output_dir_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: context_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_dir
         INTEGER  (kind = C_INT)     , VALUE        :: output_dir_size
      END SUBROUTINE xios_set_context_output_dir
      
      SUBROUTINE xios_context_handle_create(ret, idt, idt_size) BIND(C)
         import C_CHAR, C_INTPTR_T, C_INT
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_context_handle_create
      
      SUBROUTINE xios_context_set_current(context, withswap) BIND(C)
         import C_BOOL, C_INT, C_INTPTR_T
         INTEGER (kind = C_INTPTR_T), VALUE :: context
         LOGICAL (kind = C_BOOL)    , VALUE :: withswap
      END SUBROUTINE xios_context_set_current

      SUBROUTINE xios_context_create(context, context_id, context_id_size, calendar_type, &
                                     year, month, day, hour, minute, second) BIND(C)
         import C_CHAR, C_INT, C_INTPTR_T
         INTEGER  (kind = C_INTPTR_T)               :: context
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: context_id
         INTEGER  (kind = C_INT)     , VALUE        :: context_id_size
         INTEGER  (kind = C_INT)     , VALUE        :: calendar_type, year, month, day, hour, minute, second
      END SUBROUTINE xios_context_create
     
   END INTERFACE
     
END MODULE CONTEXT_INTERFACE
