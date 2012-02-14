#include "xios_fortran_prefix.hpp"

MODULE IFILE
   USE, INTRINSIC :: ISO_C_BINDING
   USE FILE_INTERFACE
   USE FILEGROUP_INTERFACE
   
   TYPE txios(file)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(file)
   
   TYPE txios(filegroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(filegroup)
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.


   SUBROUTINE xios(set_file_attr)(file_id, name , description, name_suffix, output_freq, output_level, enabled, type)
      IMPLICIT NONE
      TYPE(txios(file))                       :: file_hdl
      CHARACTER(len = *)          , INTENT(IN) :: file_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type
            
      CALL xios(get_file_handle)(file_id,file_hdl)
      CALL xios(set_file_attr_hdl_)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled, type)
      
   END SUBROUTINE xios(set_file_attr)
   

   SUBROUTINE xios(set_file_attr_hdl)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled,type)
      TYPE(txios(file))          , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type

      CALL xios(set_file_attr_hdl_)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled, type)
      
   END SUBROUTINE xios(set_file_attr_hdl)

   SUBROUTINE xios(set_file_attr_hdl_)(file_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_, type_)
      TYPE(txios(file))          , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type_
            
      IF (PRESENT(name_))         THEN
         CALL cxios_set_file_name(file_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_))  THEN
         CALL cxios_set_file_description(file_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(name_suffix_))  THEN
         CALL cxios_set_file_name_suffix(file_hdl%daddr, name_suffix_, len(name_suffix_))
      END IF
      IF (PRESENT(output_freq_))  THEN
         CALL cxios_set_file_output_freq(file_hdl%daddr, output_freq_, len(output_freq_))
      END IF
      IF (PRESENT(output_level_)) THEN
         CALL cxios_set_file_output_level(file_hdl%daddr, output_level_)
      END IF
      IF (PRESENT(enabled_))      THEN
         enabled__ = enabled_        
         CALL cxios_set_file_enabled(file_hdl%daddr, enabled__)
      END IF
      
      IF (PRESENT(type_))         THEN
         CALL cxios_set_file_type(file_hdl%daddr, type_, len(type_))
      END IF
      
   END SUBROUTINE xios(set_file_attr_hdl_)


   
   SUBROUTINE xios(set_filegroup_attr)(filegroup_id, name , description, name_suffix, output_freq, output_level, enabled, type)
      IMPLICIT NONE
      TYPE(txios(filegroup))                  :: filegroup_hdl
      CHARACTER(len = *)          , INTENT(IN) :: filegroup_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type
      
      CALL xios(get_filegroup_handle)(filegroup_id,filegroup_hdl)
      CALL xios(set_filegroup_attr_hdl_)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled, type)
      
   END SUBROUTINE xios(set_filegroup_attr)


   SUBROUTINE xios(set_filegroup_attr_hdl)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled, type)
      IMPLICIT NONE
      TYPE(txios(filegroup))     , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type
      
     CALL xios(set_filegroup_attr_hdl_)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled, type)

   END SUBROUTINE xios(set_filegroup_attr_hdl)
      
   
   SUBROUTINE xios(set_filegroup_attr_hdl_)(filegroup_hdl, name_ , description_, name_suffix_, output_freq_, output_level_,     &
                   enabled_,type_)
      IMPLICIT NONE
      TYPE(txios(filegroup))     , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: type_
      
      IF (PRESENT(name_))         THEN
         CALL cxios_set_filegroup_name(filegroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_))  THEN
         CALL cxios_set_filegroup_description(filegroup_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(name_suffix_))  THEN
         CALL cxios_set_filegroup_name_suffix(filegroup_hdl%daddr, name_suffix_, len(name_suffix_))
      END IF
      IF (PRESENT(output_freq_))  THEN
         CALL cxios_set_filegroup_output_freq(filegroup_hdl%daddr, output_freq_, len(output_freq_))
      END IF
      IF (PRESENT(output_level_)) THEN
         CALL cxios_set_filegroup_output_level(filegroup_hdl%daddr, output_level_)
      END IF
      IF (PRESENT(enabled_))      THEN
        enabled__ = enabled_ 
        CALL cxios_set_filegroup_enabled(filegroup_hdl%daddr, enabled__)
      END IF
      
      IF (PRESENT(type_))         THEN
         CALL cxios_set_filegroup_type(filegroup_hdl%daddr, type_, len(type_))
      END IF

   END SUBROUTINE xios(set_filegroup_attr_hdl_)


   SUBROUTINE xios(get_file_handle)( idt, ret)
      IMPLICIT NONE
      CHARACTER(len = *),   INTENT(IN) :: idt      
      TYPE(txios(file)) , INTENT(OUT):: ret

      CALL cxios_file_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_file_handle)
   
   SUBROUTINE xios(get_filegroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)    ,   INTENT(IN) :: idt      
      TYPE(txios(filegroup)), INTENT(OUT):: ret

      CALL cxios_filegroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_filegroup_handle)

   LOGICAL FUNCTION xios(is_valid_file)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_file_valid_id(val, idt, len(idt));
      xios(is_valid_file) = val

   END FUNCTION  xios(is_valid_file)

   LOGICAL FUNCTION xios(is_valid_filegroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_filegroup_valid_id(val, idt, len(idt));
      xios(is_valid_filegroup) = val

   END FUNCTION  xios(is_valid_filegroup)

   
END MODULE IFILE
