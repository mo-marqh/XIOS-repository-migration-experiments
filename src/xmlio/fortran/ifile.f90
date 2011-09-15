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
   
   !----------------------------------------------------------------------------
   INTERFACE set_file_attributes
      MODULE PROCEDURE set_file_attributes_id,set_file_attributes_hdl
   END INTERFACE  
   
   INTERFACE set_file_group_attributes
      MODULE PROCEDURE set_filegroup_attributes_id,set_filegroup_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
  
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
   SUBROUTINE set_file_attributes_id(file_id, name_ , description_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileHandle)                        :: file_hdl
      CHARACTER(len = *)          , INTENT(IN) :: file_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      CALL file_handle_create(file_hdl, file_id)
      CALL set_file_attributes_hdl(file_hdl, name_ , description_, output_freq_, output_level_, enabled_)
      
   END SUBROUTINE set_file_attributes_id
   
   SUBROUTINE set_file_attributes_hdl(file_hdl, name_ , description_, output_freq_, output_level_, enabled_)
      TYPE(XFileHandle)           , INTENT(IN) :: file_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      enabled__ = enabled_        
      IF (PRESENT(name_))         THEN
         CALL xios_set_file_name(file_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_))  THEN
         CALL xios_set_file_description(file_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(output_freq_))  THEN
         CALL xios_set_file_output_freq(file_hdl%daddr, output_freq_, len(output_freq_))
      END IF
      IF (PRESENT(output_level_)) THEN
         CALL xios_set_file_output_level(file_hdl%daddr, output_level_)
      END IF
      IF (PRESENT(enabled_))      THEN
         CALL xios_set_file_enabled(file_hdl%daddr, enabled__)
      END IF

   END SUBROUTINE set_file_attributes_hdl
   
   SUBROUTINE set_filegroup_attributes_id(filegroup_id, name_ , description_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileGroupHandle)                   :: filegroup_hdl
      CHARACTER(len = *)          , INTENT(IN) :: filegroup_id
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      CALL filegroup_handle_create(filegroup_hdl, filegroup_id)
      CALL set_filegroup_attributes_hdl(filegroup_hdl, name_ , description_, output_freq_, output_level_, enabled_)
      
   END SUBROUTINE set_filegroup_attributes_id
   
   SUBROUTINE set_filegroup_attributes_hdl(filegroup_hdl, name_ , description_, output_freq_, output_level_, enabled_)
      IMPLICIT NONE
      TYPE(XFileGroupHandle)      , INTENT(IN) :: filegroup_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: description_
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: output_freq_
      INTEGER           , OPTIONAL, INTENT(IN) :: output_level_
      LOGICAL(kind = 1)                        :: enabled__
      LOGICAL           , OPTIONAL, INTENT(IN) :: enabled_
      
      enabled__ = enabled_         
      IF (PRESENT(name_))         THEN
         CALL xios_set_filegroup_name(filegroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(description_))  THEN
         CALL xios_set_filegroup_description(filegroup_hdl%daddr, description_, len(description_))
      END IF
      IF (PRESENT(output_freq_))  THEN
         CALL xios_set_filegroup_output_freq(filegroup_hdl%daddr, output_freq_, len(output_freq_))
      END IF
      IF (PRESENT(output_level_)) THEN
         CALL xios_set_filegroup_output_level(filegroup_hdl%daddr, output_level_)
      END IF
      IF (PRESENT(enabled_))      THEN
         CALL xios_set_filegroup_enabled(filegroup_hdl%daddr, enabled__)
      END IF

   END SUBROUTINE set_filegroup_attributes_hdl
   
   SUBROUTINE file_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFileHandle) , INTENT(OUT):: ret
      CHARACTER(len = *), INTENT(IN) :: idt      
      CALL xios_file_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE file_handle_create
   
   SUBROUTINE filegroup_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XFileGroupHandle), INTENT(OUT):: ret
      CHARACTER(len = *)    , INTENT(IN) :: idt      
      CALL xios_filegroup_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE filegroup_handle_create
   
END MODULE IFILE
