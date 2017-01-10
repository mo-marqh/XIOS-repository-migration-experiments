

MODULE IXML_TREE
   USE, INTRINSIC :: ISO_C_BINDING
   USE ISCALAR
   USE IAXIS
   USE IFILE
   USE IFIELD
   USE IGRID
   USE IDOMAIN
   USE IVARIABLE

   USE IZOOM_DOMAIN
   USE IINTERPOLATE_DOMAIN
   USE IGENERATE_RECTILINEAR_DOMAIN
   USE ICOMPUTE_CONNECTIVITY_DOMAIN
   USE IEXPAND_DOMAIN

   USE IZOOM_AXIS
   USE IINTERPOLATE_AXIS
   USE IINVERSE_AXIS
   USE IREDUCE_DOMAIN_TO_AXIS
   USE IEXTRACT_DOMAIN_TO_AXIS

   USE IREDUCE_AXIS_TO_SCALAR
   USE IEXTRACT_AXIS_TO_SCALAR
   USE IREDUCE_DOMAIN_TO_SCALAR

   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99

      SUBROUTINE cxios_xml_tree_add_field(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_field

      SUBROUTINE cxios_xml_tree_add_grid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_grid

      SUBROUTINE cxios_xml_tree_add_file(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_file

      SUBROUTINE cxios_xml_tree_add_scalar(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_scalar

      SUBROUTINE cxios_xml_tree_add_axis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_axis

      SUBROUTINE cxios_xml_tree_add_domain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_domain

      SUBROUTINE cxios_xml_tree_add_fieldtofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldtofile

      SUBROUTINE cxios_xml_tree_add_variabletofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_variabletofile


      SUBROUTINE cxios_xml_tree_add_variabletofield(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_variabletofield


      SUBROUTINE cxios_xml_tree_add_fieldgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldgroup

      SUBROUTINE cxios_xml_tree_add_gridgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_gridgroup

      SUBROUTINE cxios_xml_tree_add_filegroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_filegroup

      SUBROUTINE cxios_xml_tree_add_scalargroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_scalargroup

      SUBROUTINE cxios_xml_tree_add_axisgroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_axisgroup

      SUBROUTINE cxios_xml_tree_add_domaingroup(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_domaingroup

      SUBROUTINE cxios_xml_tree_add_fieldgrouptofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_fieldgrouptofile

      SUBROUTINE cxios_xml_tree_add_variablegrouptofile(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_variablegrouptofile

      SUBROUTINE cxios_xml_tree_add_variablegrouptofield(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_variablegrouptofield

      SUBROUTINE cxios_xml_tree_add_scalartogrid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_scalartogrid

      SUBROUTINE cxios_xml_tree_add_axistogrid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_axistogrid

      SUBROUTINE cxios_xml_tree_add_domaintogrid(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_domaintogrid

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!! TRANSFORMATIONS
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !!! DOMAIN
      SUBROUTINE cxios_xml_tree_add_zoomdomaintodomain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_zoomdomaintodomain

      SUBROUTINE cxios_xml_tree_add_interpolatedomaintodomain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_interpolatedomaintodomain

      SUBROUTINE cxios_xml_tree_add_generatedomaintodomain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_generatedomaintodomain

      SUBROUTINE cxios_xml_tree_add_computeconnectivitydomaintodomain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_computeconnectivitydomaintodomain

      SUBROUTINE cxios_xml_tree_add_expanddomaintodomain(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_expanddomaintodomain

      !!!! AXIS
      SUBROUTINE cxios_xml_tree_add_zoomaxistoaxis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_zoomaxistoaxis

      SUBROUTINE cxios_xml_tree_add_interpolateaxistoaxis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_interpolateaxistoaxis

      SUBROUTINE cxios_xml_tree_add_inverseaxistoaxis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_inverseaxistoaxis

      SUBROUTINE cxios_xml_tree_add_reducedomaintoaxistoaxis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_reducedomaintoaxistoaxis

      SUBROUTINE cxios_xml_tree_add_extractdomaintoaxistoaxis(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_extractdomaintoaxistoaxis

      !!! SCALAR
      SUBROUTINE cxios_xml_tree_add_reduceaxistoscalartoscalar(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_reduceaxistoscalartoscalar

      SUBROUTINE cxios_xml_tree_add_extractaxistoscalartoscalar(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_extractaxistoscalartoscalar

      SUBROUTINE cxios_xml_tree_add_reducedomaintoscalartoscalar(parent_, child_, child_id, child_id_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: parent_
         INTEGER (kind = C_INTPTR_T) :: child_
         CHARACTER(kind = C_CHAR) , DIMENSION(*) :: child_id
         INTEGER (kind = C_INT) , VALUE :: child_id_size
      END SUBROUTINE cxios_xml_tree_add_reducedomaintoscalartoscalar

      SUBROUTINE cxios_xml_tree_show(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER (kind = C_INT) , VALUE :: filename_size
      END SUBROUTINE cxios_xml_tree_show

      SUBROUTINE cxios_xml_parse_file(filename, filename_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: filename
         INTEGER (kind = C_INT) , VALUE :: filename_size
      END SUBROUTINE cxios_xml_parse_file

      SUBROUTINE cxios_xml_parse_string(xmlcontent, xmlcontent_size) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR), DIMENSION(*) :: xmlcontent
         INTEGER (kind = C_INT) , VALUE :: xmlcontent_size
      END SUBROUTINE cxios_xml_parse_string

   END INTERFACE


   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE xios_add_scalar(parent_hdl, child_hdl, child_id)
      TYPE(xios_scalargroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_scalar) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_scalar(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_scalar(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_scalar

   SUBROUTINE xios_add_axis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axisgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_axis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_axis

   SUBROUTINE xios_add_file(parent_hdl, child_hdl, child_id)
      TYPE(xios_filegroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_file) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_file(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_file

   SUBROUTINE xios_add_grid(parent_hdl, child_hdl, child_id)
      TYPE(xios_gridgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_grid) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_grid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_grid


   SUBROUTINE xios_add_field(parent_hdl, child_hdl, child_id)
      TYPE(xios_fieldgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_field) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_field(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_field


   SUBROUTINE xios_add_domain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domaingroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_domain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_domain

   SUBROUTINE xios_add_fieldtofile(parent_hdl, child_hdl, child_id)
      TYPE(xios_file) , INTENT(IN) :: parent_hdl
      TYPE(xios_field) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldtofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_fieldtofile

   SUBROUTINE xios_add_variabletofile(parent_hdl, child_hdl, child_id)
      TYPE(xios_file) , INTENT(IN) :: parent_hdl
      TYPE(xios_variable) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_variabletofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_variabletofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_variabletofile

   SUBROUTINE xios_add_variabletofield(parent_hdl, child_hdl, child_id)
      TYPE(xios_field) , INTENT(IN) :: parent_hdl
      TYPE(xios_variable) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_variabletofield(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_variabletofield(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_variabletofield

   SUBROUTINE xios_add_scalargroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_scalargroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_scalargroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_scalargroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_scalargroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_scalargroup

   SUBROUTINE xios_add_axisgroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_axisgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_axisgroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_axisgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_axisgroup


   SUBROUTINE xios_add_filegroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_filegroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_filegroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_filegroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_filegroup

   SUBROUTINE xios_add_gridgroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_gridgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_gridgroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_gridgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_gridgroup


   SUBROUTINE xios_add_fieldgroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_fieldgroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_fieldgroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL, INTENT(IN) :: child_id
      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldgroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF
   END SUBROUTINE xios_add_fieldgroup

   SUBROUTINE xios_add_domaingroup(parent_hdl, child_hdl, child_id)
      TYPE(xios_domaingroup) , INTENT(IN) :: parent_hdl
      TYPE(xios_domaingroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_domaingroup(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_domaingroup

   SUBROUTINE xios_add_fieldgrouptofile(parent_hdl, child_hdl, child_id)
      TYPE(xios_file) , INTENT(IN) :: parent_hdl
      TYPE(xios_fieldgroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_fieldgrouptofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_fieldgrouptofile

   SUBROUTINE xios_add_variablegrouptofile(parent_hdl, child_hdl, child_id)
      TYPE(xios_file) , INTENT(IN) :: parent_hdl
      TYPE(xios_variablegroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_variablegrouptofile(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_variablegrouptofile(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_variablegrouptofile

   SUBROUTINE xios_add_variablegrouptofield(parent_hdl, child_hdl, child_id)
      TYPE(xios_field) , INTENT(IN) :: parent_hdl
      TYPE(xios_variablegroup) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_variablegrouptofield(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_variablegrouptofield(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_variablegrouptofield

   SUBROUTINE xios_add_scalartogrid(parent_hdl, child_hdl, child_id)
      TYPE(xios_grid) , INTENT(IN) :: parent_hdl
      TYPE(xios_scalar) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_scalartogrid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_scalartogrid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_scalartogrid

   SUBROUTINE xios_add_axistogrid(parent_hdl, child_hdl, child_id)
      TYPE(xios_grid) , INTENT(IN) :: parent_hdl
      TYPE(xios_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_axistogrid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_axistogrid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_axistogrid

   SUBROUTINE xios_add_domaintogrid(parent_hdl, child_hdl, child_id)
      TYPE(xios_grid) , INTENT(IN) :: parent_hdl
      TYPE(xios_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_domaintogrid(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_domaintogrid(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_domaintogrid

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!! TRANSFORMATIONS
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!! DOAMIN
   SUBROUTINE xios_add_zoomdomaintodomain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domain) , INTENT(IN) :: parent_hdl
      TYPE(xios_zoom_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_zoomdomaintodomain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_zoomdomaintodomain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_zoomdomaintodomain

   SUBROUTINE xios_add_interpolatedomaintodomain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domain) , INTENT(IN) :: parent_hdl
      TYPE(xios_interpolate_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_interpolatedomaintodomain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_interpolatedomaintodomain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_interpolatedomaintodomain

   SUBROUTINE xios_add_generatedomaintodomain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domain) , INTENT(IN) :: parent_hdl
      TYPE(xios_generate_rectilinear_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_generatedomaintodomain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_generatedomaintodomain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_generatedomaintodomain

   SUBROUTINE xios_add_computeconnectivitydomaintodomain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domain) , INTENT(IN) :: parent_hdl
      TYPE(xios_compute_connectivity_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_computeconnectivitydomaintodomain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_computeconnectivitydomaintodomain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_computeconnectivitydomaintodomain

   SUBROUTINE xios_add_expanddomaintodomain(parent_hdl, child_hdl, child_id)
      TYPE(xios_domain) , INTENT(IN) :: parent_hdl
      TYPE(xios_expand_domain) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_expanddomaintodomain(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_expanddomaintodomain(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_expanddomaintodomain

   !!! AXIS
   SUBROUTINE xios_add_zoomaxistoaxis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axis) , INTENT(IN) :: parent_hdl
      TYPE(xios_zoom_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_zoomaxistoaxis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_zoomaxistoaxis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_zoomaxistoaxis

   SUBROUTINE xios_add_interpolateaxistoaxis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axis) , INTENT(IN) :: parent_hdl
      TYPE(xios_interpolate_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_interpolateaxistoaxis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_interpolateaxistoaxis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_interpolateaxistoaxis

   SUBROUTINE xios_add_inverseaxistoaxis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axis) , INTENT(IN) :: parent_hdl
      TYPE(xios_inverse_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_inverseaxistoaxis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_inverseaxistoaxis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_inverseaxistoaxis

   SUBROUTINE xios_add_reducedomaintoaxistoaxis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axis) , INTENT(IN) :: parent_hdl
      TYPE(xios_reduce_domain_to_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_reducedomaintoaxistoaxis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_reducedomaintoaxistoaxis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_reducedomaintoaxistoaxis

   SUBROUTINE xios_add_extractdomaintoaxistoaxis(parent_hdl, child_hdl, child_id)
      TYPE(xios_axis) , INTENT(IN) :: parent_hdl
      TYPE(xios_extract_domain_to_axis) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_extractdomaintoaxistoaxis(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_extractdomaintoaxistoaxis(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_extractdomaintoaxistoaxis

   !!! SCALAR
   SUBROUTINE xios_add_reduceaxistoscalartoscalar(parent_hdl, child_hdl, child_id)
      TYPE(xios_scalar) , INTENT(IN) :: parent_hdl
      TYPE(xios_reduce_axis_to_scalar) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_reduceaxistoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_reduceaxistoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_reduceaxistoscalartoscalar

   SUBROUTINE xios_add_extractaxistoscalartoscalar(parent_hdl, child_hdl, child_id)
      TYPE(xios_scalar) , INTENT(IN) :: parent_hdl
      TYPE(xios_extract_axis_to_scalar) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_extractaxistoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_extractaxistoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_extractaxistoscalartoscalar

   SUBROUTINE xios_add_reducedomaintoscalartoscalar(parent_hdl, child_hdl, child_id)
      TYPE(xios_scalar) , INTENT(IN) :: parent_hdl
      TYPE(xios_reduce_domain_to_scalar) , INTENT(OUT):: child_hdl
      CHARACTER(len = *), OPTIONAL , INTENT(IN) :: child_id

      IF (PRESENT(child_id)) THEN
         CALL cxios_xml_tree_add_reducedomaintoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, child_id, len(child_id))
      ELSE
         CALL cxios_xml_tree_add_reducedomaintoscalartoscalar(parent_hdl%daddr, child_hdl%daddr, "NONE", -1)
      END IF

   END SUBROUTINE xios_add_reducedomaintoscalartoscalar

END MODULE IXML_TREE
