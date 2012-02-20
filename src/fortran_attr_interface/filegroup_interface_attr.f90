! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *

MODULE filegroup_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING
  
  INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99
    
    
    SUBROUTINE cxios_set_filegroup_description(filegroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_set_filegroup_description
    
    SUBROUTINE cxios_get_filegroup_description(filegroup_hdl, description, description_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: description
      INTEGER  (kind = C_INT)     , VALUE        :: description_size
    END SUBROUTINE cxios_get_filegroup_description
    
    
    SUBROUTINE cxios_set_filegroup_enabled(filegroup_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: enabled
    END SUBROUTINE cxios_set_filegroup_enabled
    
    SUBROUTINE cxios_get_filegroup_enabled(filegroup_hdl, enabled) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      LOGICAL (KIND=C_BOOL)             :: enabled
    END SUBROUTINE cxios_get_filegroup_enabled
    
    
    SUBROUTINE cxios_set_filegroup_group_ref(filegroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_set_filegroup_group_ref
    
    SUBROUTINE cxios_get_filegroup_group_ref(filegroup_hdl, group_ref, group_ref_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: group_ref
      INTEGER  (kind = C_INT)     , VALUE        :: group_ref_size
    END SUBROUTINE cxios_get_filegroup_group_ref
    
    
    SUBROUTINE cxios_set_filegroup_name(filegroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_set_filegroup_name
    
    SUBROUTINE cxios_get_filegroup_name(filegroup_hdl, name, name_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
      INTEGER  (kind = C_INT)     , VALUE        :: name_size
    END SUBROUTINE cxios_get_filegroup_name
    
    
    SUBROUTINE cxios_set_filegroup_name_suffix(filegroup_hdl, name_suffix, name_suffix_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name_suffix
      INTEGER  (kind = C_INT)     , VALUE        :: name_suffix_size
    END SUBROUTINE cxios_set_filegroup_name_suffix
    
    SUBROUTINE cxios_get_filegroup_name_suffix(filegroup_hdl, name_suffix, name_suffix_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name_suffix
      INTEGER  (kind = C_INT)     , VALUE        :: name_suffix_size
    END SUBROUTINE cxios_get_filegroup_name_suffix
    
    
    SUBROUTINE cxios_set_filegroup_output_freq(filegroup_hdl, output_freq, output_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_freq
      INTEGER  (kind = C_INT)     , VALUE        :: output_freq_size
    END SUBROUTINE cxios_set_filegroup_output_freq
    
    SUBROUTINE cxios_get_filegroup_output_freq(filegroup_hdl, output_freq, output_freq_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_freq
      INTEGER  (kind = C_INT)     , VALUE        :: output_freq_size
    END SUBROUTINE cxios_get_filegroup_output_freq
    
    
    SUBROUTINE cxios_set_filegroup_output_level(filegroup_hdl, output_level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      INTEGER (KIND=C_INT)      , VALUE :: output_level
    END SUBROUTINE cxios_set_filegroup_output_level
    
    SUBROUTINE cxios_get_filegroup_output_level(filegroup_hdl, output_level) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      INTEGER (KIND=C_INT)             :: output_level
    END SUBROUTINE cxios_get_filegroup_output_level
    
    
    SUBROUTINE cxios_set_filegroup_type(filegroup_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_set_filegroup_type
    
    SUBROUTINE cxios_get_filegroup_type(filegroup_hdl, type, type_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: filegroup_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: type
      INTEGER  (kind = C_INT)     , VALUE        :: type_size
    END SUBROUTINE cxios_get_filegroup_type
    
    
    END INTERFACE
  
END MODULE filegroup_interface_attr
