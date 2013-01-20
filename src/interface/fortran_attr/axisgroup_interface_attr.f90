! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE axisgroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_axisgroup_group_ref(axisgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_axisgroup_group_ref
    
    SUBROUTINE cxios_get_axisgroup_group_ref(axisgroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_axisgroup_group_ref
    
    
    SUBROUTINE cxios_set_axisgroup_long_name(axisgroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_set_axisgroup_long_name
    
    SUBROUTINE cxios_get_axisgroup_long_name(axisgroup_hdl, long_name, long_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
      INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
    END SUBROUTINE cxios_get_axisgroup_long_name
    
    
    SUBROUTINE cxios_set_axisgroup_name(axisgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_axisgroup_name
    
    SUBROUTINE cxios_get_axisgroup_name(axisgroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_axisgroup_name
    
    
    SUBROUTINE cxios_set_axisgroup_positive(axisgroup_hdl, positive, positive_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: positive
      INTEGER  (kind = C_INT)     , VALUE        :: positive_size
    END SUBROUTINE cxios_set_axisgroup_positive
    
    SUBROUTINE cxios_get_axisgroup_positive(axisgroup_hdl, positive, positive_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: positive
      INTEGER  (kind = C_INT)     , VALUE        :: positive_size
    END SUBROUTINE cxios_get_axisgroup_positive
    
    
    SUBROUTINE cxios_set_axisgroup_size(axisgroup_hdl, size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: size
    END SUBROUTINE cxios_set_axisgroup_size
    
    SUBROUTINE cxios_get_axisgroup_size(axisgroup_hdl, size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)             :: size
    END SUBROUTINE cxios_get_axisgroup_size
    
    
    SUBROUTINE cxios_set_axisgroup_standard_name(axisgroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_set_axisgroup_standard_name
    
    SUBROUTINE cxios_get_axisgroup_standard_name(axisgroup_hdl, standard_name, standard_name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
      INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
    END SUBROUTINE cxios_get_axisgroup_standard_name
    
    
    SUBROUTINE cxios_set_axisgroup_unit(axisgroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_set_axisgroup_unit
    
    SUBROUTINE cxios_get_axisgroup_unit(axisgroup_hdl, unit, unit_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: unit
      INTEGER  (kind = C_INT)     , VALUE        :: unit_size
    END SUBROUTINE cxios_get_axisgroup_unit
    
    
    SUBROUTINE cxios_set_axisgroup_value(axisgroup_hdl, value, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: axisgroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: value
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_set_axisgroup_value
    
    SUBROUTINE cxios_get_axisgroup_value(axisgroup_hdl, value, extent1) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE       :: axisgroup_hdl
      REAL (KIND=C_DOUBLE)     , DIMENSION(*) :: value
      INTEGER (kind = C_INT), VALUE  :: extent1
    END SUBROUTINE cxios_get_axisgroup_value
    
    
    SUBROUTINE cxios_set_axisgroup_zoom_begin(axisgroup_hdl, zoom_begin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_begin
    END SUBROUTINE cxios_set_axisgroup_zoom_begin
    
    SUBROUTINE cxios_get_axisgroup_zoom_begin(axisgroup_hdl, zoom_begin) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_begin
    END SUBROUTINE cxios_get_axisgroup_zoom_begin
    
    
    SUBROUTINE cxios_set_axisgroup_zoom_end(axisgroup_hdl, zoom_end) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_end
    END SUBROUTINE cxios_set_axisgroup_zoom_end
    
    SUBROUTINE cxios_get_axisgroup_zoom_end(axisgroup_hdl, zoom_end) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_end
    END SUBROUTINE cxios_get_axisgroup_zoom_end
    
    
    SUBROUTINE cxios_set_axisgroup_zoom_size(axisgroup_hdl, zoom_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: zoom_size
    END SUBROUTINE cxios_set_axisgroup_zoom_size
    
    SUBROUTINE cxios_get_axisgroup_zoom_size(axisgroup_hdl, zoom_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: axisgroup_hdl
      INTEGER (KIND=C_INT)             :: zoom_size
    END SUBROUTINE cxios_get_axisgroup_zoom_size
    
    
    END INTERFACE
  
END MODULE axisgroup_interface_attr
