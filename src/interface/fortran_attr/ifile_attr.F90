! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE ifile_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE ifile
  USE file_interface_attr
  
CONTAINS
  
  SUBROUTINE xios(set_file_attr)  &
    ( file_id, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
    , sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(file))  :: file_hdl
      CHARACTER(LEN=*), INTENT(IN) ::file_id
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(get_file_handle)(file_id,file_hdl)
      CALL xios(set_file_attr_hdl_)   &
      ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
      , sync_freq, type )
    
  END SUBROUTINE xios(set_file_attr)
  
  SUBROUTINE xios(set_file_attr_hdl)  &
    ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
    , sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(file)) , INTENT(IN) :: file_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type
      
      CALL xios(set_file_attr_hdl_)  &
      ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
      , sync_freq, type )
    
  END SUBROUTINE xios(set_file_attr_hdl)
  
  SUBROUTINE xios(set_file_attr_hdl_)   &
    ( file_hdl, description_, enabled_, name_, name_suffix_, output_freq_, output_level_, par_access_  &
    , split_freq_, sync_freq_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(file)) , INTENT(IN) :: file_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: description_
      LOGICAL  , OPTIONAL, INTENT(IN) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER  , OPTIONAL, INTENT(IN) :: output_level_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: par_access_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: sync_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: type_
      
      IF (PRESENT(description_)) THEN
        CALL cxios_set_file_description(file_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        enabled__tmp=enabled_
        CALL cxios_set_file_enabled(file_hdl%daddr, enabled__tmp)
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_set_file_name(file_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(name_suffix_)) THEN
        CALL cxios_set_file_name_suffix(file_hdl%daddr, name_suffix_, len(name_suffix_))
      ENDIF
      
      IF (PRESENT(output_freq_)) THEN
        CALL cxios_set_file_output_freq(file_hdl%daddr, output_freq_, len(output_freq_))
      ENDIF
      
      IF (PRESENT(output_level_)) THEN
        CALL cxios_set_file_output_level(file_hdl%daddr, output_level_)
      ENDIF
      
      IF (PRESENT(par_access_)) THEN
        CALL cxios_set_file_par_access(file_hdl%daddr, par_access_, len(par_access_))
      ENDIF
      
      IF (PRESENT(split_freq_)) THEN
        CALL cxios_set_file_split_freq(file_hdl%daddr, split_freq_, len(split_freq_))
      ENDIF
      
      IF (PRESENT(sync_freq_)) THEN
        CALL cxios_set_file_sync_freq(file_hdl%daddr, sync_freq_, len(sync_freq_))
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_set_file_type(file_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(set_file_attr_hdl_)
  
  SUBROUTINE xios(get_file_attr)  &
    ( file_id, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
    , sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(file))  :: file_hdl
      CHARACTER(LEN=*), INTENT(IN) ::file_id
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_file_handle)(file_id,file_hdl)
      CALL xios(get_file_attr_hdl_)   &
      ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
      , sync_freq, type )
    
  END SUBROUTINE xios(get_file_attr)
  
  SUBROUTINE xios(get_file_attr_hdl)  &
    ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
    , sync_freq, type )
    
    IMPLICIT NONE
      TYPE(txios(file)) , INTENT(IN) :: file_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled
      LOGICAL (KIND=C_BOOL) :: enabled_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type
      
      CALL xios(get_file_attr_hdl_)  &
      ( file_hdl, description, enabled, name, name_suffix, output_freq, output_level, par_access, split_freq  &
      , sync_freq, type )
    
  END SUBROUTINE xios(get_file_attr_hdl)
  
  SUBROUTINE xios(get_file_attr_hdl_)   &
    ( file_hdl, description_, enabled_, name_, name_suffix_, output_freq_, output_level_, par_access_  &
    , split_freq_, sync_freq_, type_ )
    
    IMPLICIT NONE
      TYPE(txios(file)) , INTENT(IN) :: file_hdl
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: description_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: enabled_
      LOGICAL (KIND=C_BOOL) :: enabled__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: name_suffix_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_freq_
      INTEGER  , OPTIONAL, INTENT(OUT) :: output_level_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: par_access_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: split_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: sync_freq_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: type_
      
      IF (PRESENT(description_)) THEN
        CALL cxios_get_file_description(file_hdl%daddr, description_, len(description_))
      ENDIF
      
      IF (PRESENT(enabled_)) THEN
        CALL cxios_get_file_enabled(file_hdl%daddr, enabled__tmp)
        enabled_=enabled__tmp
      ENDIF
      
      IF (PRESENT(name_)) THEN
        CALL cxios_get_file_name(file_hdl%daddr, name_, len(name_))
      ENDIF
      
      IF (PRESENT(name_suffix_)) THEN
        CALL cxios_get_file_name_suffix(file_hdl%daddr, name_suffix_, len(name_suffix_))
      ENDIF
      
      IF (PRESENT(output_freq_)) THEN
        CALL cxios_get_file_output_freq(file_hdl%daddr, output_freq_, len(output_freq_))
      ENDIF
      
      IF (PRESENT(output_level_)) THEN
        CALL cxios_get_file_output_level(file_hdl%daddr, output_level_)
      ENDIF
      
      IF (PRESENT(par_access_)) THEN
        CALL cxios_get_file_par_access(file_hdl%daddr, par_access_, len(par_access_))
      ENDIF
      
      IF (PRESENT(split_freq_)) THEN
        CALL cxios_get_file_split_freq(file_hdl%daddr, split_freq_, len(split_freq_))
      ENDIF
      
      IF (PRESENT(sync_freq_)) THEN
        CALL cxios_get_file_sync_freq(file_hdl%daddr, sync_freq_, len(sync_freq_))
      ENDIF
      
      IF (PRESENT(type_)) THEN
        CALL cxios_get_file_type(file_hdl%daddr, type_, len(type_))
      ENDIF
      
      
    
  END SUBROUTINE xios(get_file_attr_hdl_)
  
END MODULE ifile_attr
