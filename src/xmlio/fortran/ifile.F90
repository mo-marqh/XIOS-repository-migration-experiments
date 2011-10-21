#include "xios_fortran_prefix.hpp"

MODULE IFILE
   USE, INTRINSIC :: ISO_C_BINDING
   USE FILE_INTERFACE
   USE FILEGROUP_INTERFACE
   
   TYPE XFileHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XFileHandle
   
   TYPE XFileGroupHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XFileGroupHandle

   TYPE txios(file)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(file)
   
   TYPE txios(filegroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(filegroup)
   
   !----------------------------------------------------------------------------
   INTERFACE set_file_attributes
      MODULE PROCEDURE set_file_attributes_id,set_file_attributes_hdl
   END INTERFACE  
   
   INTERFACE set_file_group_attributes
      MODULE PROCEDURE set_filegroup_attributes_id,set_filegroup_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
  
   CONTAINS ! Fonctions disponibles pour les utilisateurs.


   SUBROUTINE xios(set_file_attr)(file_id, name , description, name_suffix, output_freq, output_level, enabled)
      IMPLICIT NONE
      TYPE(txios(file))                       :: file_hdl
      CHARACTER(len = *)          , INTENT(IN) :: file_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      
      CALL xios(get_file_handle)(file_id,file_hdl)
      CALL xios(set_file_attr_hdl_)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled)
      
   END SUBROUTINE xios(set_file_attr)
   

   SUBROUTINE xios(set_file_attr_hdl)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled)
      TYPE(txios(file))          , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled

      CALL xios(set_file_attr_hdl_)(file_hdl, name , description, name_suffix, output_freq, output_level, enabled)
      
   END SUBROUTINE xios(set_file_attr_hdl)

   SUBROUTINE xios(set_file_attr_hdl_)(file_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      TYPE(txios(file))          , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
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

   END SUBROUTINE xios(set_file_attr_hdl_)


   
   SUBROUTINE xios(set_filegroup_attr)(filegroup_id, name , description, name_suffix, output_freq, output_level, enabled)
      IMPLICIT NONE
      TYPE(txios(filegroup))                  :: filegroup_hdl
      CHARACTER(len = *)          , INTENT(IN) :: filegroup_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      
      CALL xios(get_filegroup_handle)(filegroup_id,filegroup_hdl)
      CALL xios(set_filegroup_attr_hdl_)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled)
      
   END SUBROUTINE xios(set_filegroup_attr)


   SUBROUTINE xios(set_filegroup_attr_hdl)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled)
      IMPLICIT NONE
      TYPE(txios(filegroup))     , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled
      
     CALL xios(set_filegroup_attr_hdl_)(filegroup_hdl, name , description, name_suffix, output_freq, output_level, enabled)

   END SUBROUTINE xios(set_filegroup_attr_hdl)
      
   
   SUBROUTINE xios(set_filegroup_attr_hdl_)(filegroup_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(txios(filegroup))     , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
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
















!!!!!!!!!!!!!! Anciennes interfaces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   SUBROUTINE set_file_attributes_id(file_id, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileHandle)                        :: file_hdl
      CHARACTER(len = *)          , INTENT(IN) :: file_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      CALL file_handle_create(file_hdl, file_id)
      CALL set_file_attributes_hdl(file_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      
   END SUBROUTINE set_file_attributes_id
   
   SUBROUTINE set_file_attributes_hdl(file_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      TYPE(XFileHandle)           , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
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

   END SUBROUTINE set_file_attributes_hdl
   
   SUBROUTINE set_filegroup_attributes_id(filegroup_id, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileGroupHandle)                   :: filegroup_hdl
      CHARACTER(len = *)          , INTENT(IN) :: filegroup_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      CALL filegroup_handle_create(filegroup_hdl, filegroup_id)
      CALL set_filegroup_attributes_hdl(filegroup_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      
   END SUBROUTINE set_filegroup_attributes_id
   
   SUBROUTINE set_filegroup_attributes_hdl(filegroup_hdl, name_ , description_, name_suffix_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileGroupHandle)      , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_suffix_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
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

   END SUBROUTINE set_filegroup_attributes_hdl
   
   SUBROUTINE file_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFileHandle) , INTENT(OUT):: ret
      CHARACTER(len = *), INTENT(IN) :: idt      
      CALL cxios_file_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE file_handle_create
   
   SUBROUTINE filegroup_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFileGroupHandle), INTENT(OUT):: ret
      CHARACTER(len = *)    , INTENT(IN) :: idt      
      CALL cxios_filegroup_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE filegroup_handle_create

   LOGICAL FUNCTION file_valid_id(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_file_valid_id(val, idt, len(idt));
      file_valid_id = val
   END FUNCTION  file_valid_id

   LOGICAL FUNCTION filegroup_valid_id(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_filegroup_valid_id(val, idt, len(idt));
      filegroup_valid_id = val
   END FUNCTION  filegroup_valid_id
   
END MODULE IFILE
