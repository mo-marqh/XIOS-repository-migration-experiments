MODULE IXML_TREE
   USE, INTRINSIC :: ISO_C_BINDING
   USE IAXIS
   USE IFILE
   USE IFIELD
   USE IGRID
   USE IDOMAIN
     
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
     
      SUBROUTINE xios_xml_tree_add_field(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_field
      
      SUBROUTINE xios_xml_tree_add_grid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_grid
      
      SUBROUTINE xios_xml_tree_add_file(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_file
      
      SUBROUTINE xios_xml_tree_add_axis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_axis
      
      SUBROUTINE xios_xml_tree_add_domain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_domain
      
      SUBROUTINE xios_xml_tree_add_fieldtofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_fieldtofile

      SUBROUTINE xios_xml_tree_add_fieldgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_fieldgroup

      SUBROUTINE xios_xml_tree_add_gridgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_gridgroup

      SUBROUTINE xios_xml_tree_add_filegroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_filegroup

      SUBROUTINE xios_xml_tree_add_axisgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_axisgroup

      SUBROUTINE xios_xml_tree_add_domaingroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_domaingroup
      
      SUBROUTINE xios_xml_tree_add_fieldgrouptofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: parent_
         INTEGER  (kind = C_INTPTR_T)               :: child_
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: child_id
         INTEGER  (kind = C_INT)     , VALUE        :: child_id_size
      END SUBROUTINE xios_xml_tree_add_fieldgrouptofile    

      SUBROUTINE xios_xml_tree_show(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER  (kind = C_INT) , VALUE        :: filename_size
      END SUBROUTINE xios_xml_tree_show

      SUBROUTINE xios_xml_parse_file(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER  (kind = C_INT) , VALUE        :: filename_size
      END SUBROUTINE xios_xml_parse_file

      SUBROUTINE xios_xml_parse_string(xmlcontent, xmlcontent_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: xmlcontent
         INTEGER  (kind = C_INT) , VALUE        :: xmlcontent_size
      END SUBROUTINE xios_xml_parse_string
     
   END INTERFACE
   
   !----------------------------------------------------------------------------
   INTERFACE xml_tree_add
      MODULE PROCEDURE xml_tree_add_axis,   xml_tree_add_axisgroup,    &
                       xml_tree_add_file,   xml_tree_add_filegroup,    &
                       xml_tree_add_grid,   xml_tree_add_gridgroup,    &
                       xml_tree_add_field,  xml_tree_add_fieldgroup,   &
                       xml_tree_add_domain, xml_tree_add_domaingroup,  &
                       xml_tree_add_fieldgrouptofile, xml_tree_add_fieldtofile
   END INTERFACE  
   !----------------------------------------------------------------------------
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xml_tree_add_axis(parent_hdl, child_hdl, child_id)
      TYPE(XAxisGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XAxisHandle)           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_axis
   
   SUBROUTINE xml_tree_add_file(parent_hdl, child_hdl, child_id)
      TYPE(XFileGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XFileHandle)           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_file
   
   SUBROUTINE xml_tree_add_grid(parent_hdl, child_hdl, child_id)
      TYPE(XGridGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XGridHandle)           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_grid
   
   SUBROUTINE xml_tree_add_field(parent_hdl, child_hdl, child_id)
      TYPE(XFieldGroupHandle)     , INTENT(IN) :: parent_hdl
      TYPE(XFieldHandle)          , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_field
   
   SUBROUTINE xml_tree_add_domain(parent_hdl, child_hdl, child_id)
      TYPE(XDomainGroupHandle)     , INTENT(IN) :: parent_hdl
      TYPE(XDomainHandle)          , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_domain
   
   SUBROUTINE xml_tree_add_fieldtofile(parent_hdl, child_hdl, child_id)
      TYPE(XFileHandle)            , INTENT(IN) :: parent_hdl
      TYPE(XFieldHandle)           , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_fieldtofile

   SUBROUTINE xml_tree_add_axisgroup(parent_hdl, child_hdl, child_id)
      TYPE(XAxisGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XAxisGroupHandle)      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_axisgroup

   SUBROUTINE xml_tree_add_filegroup(parent_hdl, child_hdl, child_id)
      TYPE(XFileGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XFileGroupHandle)      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_filegroup

   SUBROUTINE xml_tree_add_gridgroup(parent_hdl, child_hdl, child_id)
      TYPE(XGridGroupHandle)      , INTENT(IN) :: parent_hdl
      TYPE(XGridGroupHandle)      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_gridgroup

   SUBROUTINE xml_tree_add_fieldgroup(parent_hdl, child_hdl, child_id)
      TYPE(XFieldGroupHandle)     , INTENT(IN) :: parent_hdl
      TYPE(XFieldGroupHandle)     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_fieldgroup

   SUBROUTINE xml_tree_add_domaingroup(parent_hdl, child_hdl, child_id)
      TYPE(XDomainGroupHandle)     , INTENT(IN) :: parent_hdl
      TYPE(XDomainGroupHandle)     , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_domaingroup

   SUBROUTINE xml_tree_add_fieldgrouptofile(parent_hdl, child_hdl, child_id)
      TYPE(XFileHandle)            , INTENT(IN) :: parent_hdl
      TYPE(XFieldGroupHandle)      , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL xios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL xios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xml_tree_add_fieldgrouptofile

   SUBROUTINE xml_tree_show(filename)
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: filename
      IF (PRESENT(filename)) THEN
         CALL xios_xml_tree_show(filename, len(filename))
      ELSE
         CALL xios_xml_tree_show("NONE", -1)
      END IF
   END SUBROUTINE xml_tree_show
   
   SUBROUTINE xml_parse_file(filename)
      CHARACTER(len = *), INTENT(IN) :: filename
      CALL xios_xml_parse_file(filename, len(filename))
   END SUBROUTINE xml_Parse_File
   
   SUBROUTINE xml_parse_string(xmlcontent)
      CHARACTER(len = *), INTENT(IN) :: xmlcontent
      CALL xios_xml_parse_string(xmlcontent, len(xmlcontent))
   END SUBROUTINE xml_Parse_String
      
END MODULE IXML_TREE
