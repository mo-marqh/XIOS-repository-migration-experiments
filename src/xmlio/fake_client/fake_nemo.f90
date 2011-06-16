! --------------------------------------------------- !
!          XMLIO SERVER MAIN TEST (NEMO)              !
! --------------------------------------------------- !

MODULE NEMO_FAKE

   ! Modules de la bibliothèque xmlioserver
   USE IXMLIOSERVER
   USE ISO_C_BINDING

include 'mpif.h'

   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE NEMO_FAKE_ENTRY(comm_client, comm_client_grp, comm_client_server) BIND(C)
      INTEGER(kind = C_INT), INTENT(IN), VALUE :: comm_client,       & ! communicateur des clients
                                                  comm_client_grp,   & ! communicateur du groupe de clients
                                                  comm_client_server   ! communicateur client-serveur
      REAL(kind = 8), DIMENSION(10000)  :: real_array
      INTEGER                           :: rankGrp, sizeGrp, error, i
      INTEGER                           :: ibegin, iend, jbegin, jend, data_ni, data_ibegin, ni_glo, nj_glo
      TYPE(XDate)                       :: init_date_nemo  = XDate(1985, 03, 15, 17, 35, 00)
      TYPE(XHandle)                     :: nemo_style_ctxt = NULLHANDLE
      TYPE(XHandle)                     :: temp_mod    = NULLHANDLE, &
                                           temp_mod_   = NULLHANDLE, &
                                           temp_mod__  = NULLHANDLE, &
                                           temp_mod___ = NULLHANDLE
                                           
      CALL MPI_COMM_RANK(comm_client, rankGrp, error)
      CALL MPI_COMM_SIZE(comm_client, sizeGrp, error)
      
      DO i = 1, 10000
         real_array(i) = i;
      END DO
      
      IF (rankGrp .EQ. 0) THEN
          PRINT*," Starting NEMO Client Tests ..."
      END IF

      ! Parsing du document xml de définition à partir d'une faire de caractère.
      CALL xml_parse_string ("<? xml version=1.0 ?><simulation></simulation>")

      !!!!!!!!!!!!!!!!!!!! EXEMPLE RECONSTRUCTION !!!!!!!!!!!!!!!!!!!!!

      ! On crée un nouveau context et on lui associe un handle.
      CALL context_create(context_hdl   = nemo_style_ctxt, &
                          context_id    = "nemo_style",    &
                          calendar_type = GREGORIAN,       &
                          init_date     = init_date_nemo)

      ! ---------> field_definition
      CALL handle_create(temp_mod, GFIELD,  "field_definition")

      ! Ajout d'un groupe de champ anomyme
      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod_,       &
                        child_type  = GFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod_, &
                                ftype          = GFIELD,    &
                                unit_          = "SI",      &
                                default_value_ = 10E-10_8,  &
                                prec_          = 8)

      CALL xml_tree_add(parent_hdl  = temp_mod_,       &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod,        &
                        child_type  = GFIELD,          &
                        child_id    = "field_enabled")

      CALL set_field_attributes(field_hdl      = temp_mod,     &
                                ftype          = GFIELD,       &
                                operation_     = "instant",    &
                                enabled_       = .TRUE._1,     &
                                freq_op_       = "1h")

      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_2D_k8_inst")

      CALL set_field_attributes(field_hdl      = temp_mod__,   &
                                ftype          = EFIELD,       &
                                name_          = "champ1",     &
                                standard_name_ = "lechamp1",   &
                                long_name_     = "le champ 1", &
                                domain_ref_    = "simple_domaine0")

      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_2D_k4_once_dis")

      CALL set_field_attributes(field_hdl      = temp_mod__,   &
                                ftype          = EFIELD,       &
                                prec_          = 4,            &
                                operation_     = "once",       &
                                enabled_       = .FALSE._1,    &
                                name_          = "champ2",     &
                                standard_name_ = "lechamp2",   &
                                long_name_     = "le champ 2", &
                                domain_ref_    = "simple_domaine1")

      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_3D_k4_once")

      CALL set_field_attributes(field_hdl      = temp_mod__,   &
                                ftype          = EFIELD,       &
                                prec_          = 4,            &
                                operation_     = "once",       &
                                name_          = "champ3",     &
                                standard_name_ = "lechamp3",   &
                                long_name_     = "le champ 3", &
                                grid_ref_      = "simple_grille")

      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_3D_k8_average")

      CALL set_field_attributes(field_hdl      = temp_mod__,   &
                                ftype          = EFIELD,       &
                                prec_          = 8,            &
                                operation_     = "average",    &
                                name_          = "champ4",     &
                                standard_name_ = "lechamp4",   &
                                long_name_     = "le champ 4", &
                                grid_ref_       = "simple_grille")
                                
      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_3D_k8_average_zoom")

      CALL set_field_attributes(field_hdl      = temp_mod__,   &
                                ftype          = EFIELD,       &
                                prec_          = 8,            &
                                operation_     = "average",    &
                                name_          = "champ5",     &
                                standard_name_ = "lechamp5",   &
                                long_name_     = "le champ 5", &
                                grid_ref_      = "simple_grille_zoom")

      CALL xml_tree_add(parent_hdl  = temp_mod_,       &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod,        &
                        child_type  = GFIELD,          &
                        child_id    = "field_disabled")

      CALL set_field_attributes(field_hdl  = temp_mod,  &
                                ftype      = GFIELD,    &
                                operation_ = "instant", &
                                enabled_   = .FALSE._1)

      CALL xml_tree_add(parent_hdl  = temp_mod,        &
                        parent_type = GFIELD,          &
                        child_hdl   = temp_mod__,      &
                        child_type  = EFIELD,          &
                        child_id    = "champ_3D_k8_inst")

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                prec_          = 8,                &
                                name_          = "champ5",         &
                                standard_name_ = "lechamp5",       &
                                long_name_     = "le champ 5",     &
                                domain_ref_    = "simple_domaine", &
                                axis_ref_      = "simple_axe")

      ! ---------> axis_definition
      
      CALL handle_create(temp_mod, GAXIS,  "axis_definition")
      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GAXIS,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = GAXIS,      &
                        child_id    = "all_axis")

      CALL set_axis_attributes(axis_hdl = temp_mod_, &
                               ftype    = GAXIS,     &
                               unit_    = "km")

      CALL xml_tree_add(parent_hdl     = temp_mod_,       &
                        parent_type    = GAXIS,           &
                        child_hdl      = temp_mod,        &
                        child_type     = EAXIS,           &
                        child_id       = "simple_axe")

      CALL set_axis_attributes(axis_hdl       = temp_mod, &
                               ftype          = EAXIS,    &
                               name_          = "axe1",   &
                               standard_name_ = "laxe1",  &
                               long_name_     = "l axe1", &
                               size_          = 30,       &
                               zvalue_        = real_array(1:30))

      ! ---------> domain_definition
      CALL handle_create(temp_mod, GDOMAIN,  "domain_definition")
      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GDOMAIN,    &
                        child_hdl   = temp_mod_,  &
                        child_type  = GDOMAIN,    &
                        child_id    = "all_domain")
                        
      CALL xml_tree_add(parent_hdl  = temp_mod_,   &
                        parent_type = GDOMAIN,     &
                        child_hdl   = temp_mod___, &
                        child_type  = GDOMAIN,     &
                        child_id    = "simple_domaine_grp")


      ni_glo = 100
      nj_glo = 50
      IF (rankGrp .EQ. 0) THEN
         ibegin = 1
         iend   = 26
         jbegin = 1
         jend   = 50
         data_ni     = 25
         data_ibegin = 0
      ELSE IF (rankGrp .EQ. 1) THEN
         ibegin = 25
         iend   = 51
         jbegin = 1
         jend   = 50
         data_ni     = 25
         data_ibegin = 1
      ELSE IF (rankGrp .EQ. 2) THEN
         ibegin = 50
         iend   = 76
         jbegin = 1
         jend   = 50
         data_ni     = 25
         data_ibegin = 1
      ELSE IF (rankGrp .EQ. 3) THEN
         ibegin = 75
         iend   = 100
         jbegin = 1
         jend   = 50
         data_ni     = 25
         data_ibegin = 1         
      END IF 

      CALL set_domain_attributes(domain_hdl     = temp_mod___,    &
                                 ftype          = GDOMAIN,        &
                                 lonvalue_      = real_array(1:((jend-jbegin+1)*(iend-ibegin+1))), &
                                 latvalue_      = real_array(1:((jend-jbegin+1)*(iend-ibegin+1))), &
                                 data_dim_      = 2,              &
                                 ni_glo_        = ni_glo,         &
                                 nj_glo_        = nj_glo,         &
                                 ibegin_        = ibegin,         &
                                 iend_          = iend,           &
                                 jbegin_        = jbegin,         &
                                 jend_          = jend,           &
                                 data_ni_       = data_ni,        &
                                 data_ibegin_   = data_ibegin)                       

      CALL xml_tree_add(parent_hdl  = temp_mod___,&
                        parent_type = GDOMAIN,    &
                        child_hdl   = temp_mod,   &
                        child_type  = EDOMAIN,    &
                        child_id    = "simple_domaine0")

      CALL set_domain_attributes(domain_hdl     = temp_mod,       &
                                 ftype          = EDOMAIN,        &
                                 name_          = "domaine0",     &
                                 standard_name_ = "ledomaine0",   &
                                 long_name_     = "le domaine 0")
                                 
      CALL xml_tree_add(parent_hdl  = temp_mod___,&
                        parent_type = GDOMAIN,    &
                        child_hdl   = temp_mod,   &
                        child_type  = EDOMAIN,    &
                        child_id    = "simple_domaine0_zoom")
                        
      CALL set_domain_attributes(domain_hdl     = temp_mod,           &
                                 ftype          = EDOMAIN,            &
                                 name_          = "domaine0zoom",     &
                                 standard_name_ = "ledomaine0zoom",   &
                                 long_name_     = "le domaine 0 zoom",&
                                 zoom_ibegin_   = 21,                 &
                                 zoom_jbegin_   = 5 ,                 &
                                 zoom_ni_       = 20,                 &
                                 zoom_nj_       = 15)

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = GDOMAIN,    &
                        child_hdl   = temp_mod,   &
                        child_type  = EDOMAIN,    &
                        child_id    = "simple_domaine1")


      ni_glo = 90
      nj_glo = 20
      IF (rankGrp .EQ. 0) THEN
         ibegin = 1
         iend   = 90
         jbegin = 1
         jend   = 5        
      ELSE IF (rankGrp .EQ. 1) THEN
         ibegin = 1
         iend   = 90
         jbegin = 6
         jend   = 10
      ELSE IF (rankGrp .EQ. 2) THEN
         ibegin = 1
         iend   = 90
         jbegin = 11
         jend   = 15
      ELSE IF (rankGrp .EQ. 3) THEN
         ibegin = 1
         iend   = 90
         jbegin = 16
         jend   = 20
      END IF

      CALL set_domain_attributes(domain_hdl     = temp_mod,       &
                                 ftype          = EDOMAIN,        &
                                 name_          = "domaine1",     &
                                 standard_name_ = "ledomaine1",   &
                                 long_name_     = "le domaine 1", &
                                 lonvalue_      = real_array(1:((jend-jbegin+1)*(iend-ibegin+1))), &
                                 latvalue_      = real_array(1:((jend-jbegin+1)*(iend-ibegin+1))), &
                                 data_dim_      = 2,              &
                                 ni_glo_        = ni_glo,         &
                                 nj_glo_        = nj_glo,         &
                                 ibegin_        = ibegin,         &
                                 iend_          = iend,           &
                                 jbegin_        = jbegin,         &
                                 jend_          = jend)

      ! ---------> grid_definition
      CALL handle_create(temp_mod, GGRID, "grid_definition")
      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GGRID,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = EGRID,      &
                        child_id    = "simple_grille")

      CALL set_grid_attributes(grid_hdl     = temp_mod_,         &
                               ftype        = EGRID,             &
                               name_        = "grille1",         &
                               description_ = "la grille 1",     &
                               domain_ref_  = "simple_domaine1", &
                               axis_ref_    = "simple_axe")
                               
      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GGRID,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = EGRID,      &
                        child_id    = "simple_grille_zoom")

      CALL set_grid_attributes(grid_hdl     = temp_mod_,              &
                               ftype        = EGRID,                  &
                               name_        = "grille2",              &
                               description_ = "la grille 2",          &
                               domain_ref_  = "simple_domaine0_zoom", &
                               axis_ref_    = "simple_axe")

      ! ---------> file_definition
      CALL handle_create(temp_mod, GFILE, "file_definition")

      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GFILE,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = EFILE,      &
                        child_id    = "simple_fichier1")

      CALL set_file_attributes(file_hdl      = temp_mod_,        &
                               ftype         = EFILE,            &
                               name_         = "fichier1",       &
                               description_  = "mon fichier 1 ", &
                               output_freq_  = "12h",            &
                               output_level_ = 3,                &
                               enabled_      =  .TRUE._1)

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)
                        
      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_3D_k8_average_zoom")  

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_2D_k8_inst")

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_2D_k4_once_dis")

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_3D_k4_once")

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_3D_k8_average")

      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GFILE,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = EFILE,      &
                        child_id    = "simple_fichier2")

      CALL set_file_attributes(file_hdl      = temp_mod_,        &
                               ftype         = EFILE,            &
                               name_         = "fichier2",       &
                               description_  = "mon fichier 2 ", &
                               output_freq_  = "1d",             &
                               output_level_ = 3,                &
                               enabled_      =  .TRUE._1)

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_2D_k8_inst")

      CALL xml_tree_add(parent_hdl  = temp_mod,   &
                        parent_type = GFILE,      &
                        child_hdl   = temp_mod_,  &
                        child_type  = EFILE,      &
                        child_id    = "simple_fichier3")

      CALL set_file_attributes(file_hdl      = temp_mod_,        &
                               ftype         = EFILE,            &
                               name_         = "fichier3",       &
                               description_  = "mon fichier 3 ", &
                               output_freq_  = "18h",            &
                               output_level_ = 5,                &
                               enabled_      =  .FALSE._1)

      CALL xml_tree_add(parent_hdl  = temp_mod_,  &
                        parent_type = EFILE,      &
                        child_hdl   = temp_mod__, &
                        child_type  = EFIELD)

      CALL set_field_attributes(field_hdl      = temp_mod__,       &
                                ftype          = EFIELD,           &
                                field_ref_     = "champ_2D_k8_inst")

      !!!!!!!!!!!!!!!!!!!!!!! Début du traitement !!!!!!!!!!!!!!!!!!!!!

      ! On choisit le context dans lequel on va travailler
      ! et on commence le traitement des données.
      CALL dtreatment_start(nemo_style_ctxt, NETCDF4, comm_client_server)
      
      CALL dtreatment_end(nemo_style_ctxt)

   END SUBROUTINE NEMO_FAKE_ENTRY

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE NEMO_FAKE
