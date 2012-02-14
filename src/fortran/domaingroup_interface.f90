MODULE DOMAINGROUP_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
       
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
       
      SUBROUTINE cxios_set_domaingroup_name(domaingroup_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
         INTEGER  (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE cxios_set_domaingroup_name

      SUBROUTINE cxios_set_domaingroup_standard_name(domaingroup_hdl, standard_name, standard_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
         INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
      END SUBROUTINE cxios_set_domaingroup_standard_name

      SUBROUTINE cxios_set_domaingroup_long_name(domaingroup_hdl, long_name, long_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
         INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
      END SUBROUTINE cxios_set_domaingroup_long_name

      SUBROUTINE cxios_set_domaingroup_domain_group_ref(domaingroup_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
         INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
      END SUBROUTINE cxios_set_domaingroup_domain_group_ref

      SUBROUTINE cxios_set_domaingroup_ni_glo(domaingroup_hdl, ni_glo) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: ni_glo
      END SUBROUTINE cxios_set_domaingroup_ni_glo

      SUBROUTINE cxios_set_domaingroup_nj_glo(domaingroup_hdl, nj_glo) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: nj_glo
      END SUBROUTINE cxios_set_domaingroup_nj_glo

      SUBROUTINE cxios_set_domaingroup_ibegin(domaingroup_hdl, ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: ibegin
      END SUBROUTINE cxios_set_domaingroup_ibegin

      SUBROUTINE cxios_set_domaingroup_iend(domaingroup_hdl, iend) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: iend
      END SUBROUTINE cxios_set_domaingroup_iend

      SUBROUTINE cxios_set_domaingroup_ni(domaingroup_hdl, ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: ni
      END SUBROUTINE cxios_set_domaingroup_ni

      SUBROUTINE cxios_set_domaingroup_jbegin(domaingroup_hdl, jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: jbegin
      END SUBROUTINE cxios_set_domaingroup_jbegin

      SUBROUTINE cxios_set_domaingroup_jend(domaingroup_hdl, jend) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: jend
      END SUBROUTINE cxios_set_domaingroup_jend

      SUBROUTINE cxios_set_domaingroup_nj(domaingroup_hdl, nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: nj
      END SUBROUTINE cxios_set_domaingroup_nj

      SUBROUTINE cxios_set_domaingroup_mask(domaingroup_hdl, mask, mask_extent1, mask_extent2) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         LOGICAL(kind = C_BOOL)     , DIMENSION(*) :: mask
         INTEGER (kind = C_INT)     , VALUE        :: mask_extent1
         INTEGER (kind = C_INT)     , VALUE        :: mask_extent2
      END SUBROUTINE cxios_set_domaingroup_mask

      SUBROUTINE cxios_set_domaingroup_data_dim(domaingroup_hdl, data_dim) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_dim
      END SUBROUTINE cxios_set_domaingroup_data_dim

      SUBROUTINE cxios_set_domaingroup_data_ni(domaingroup_hdl, data_ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_ni
      END SUBROUTINE cxios_set_domaingroup_data_ni

      SUBROUTINE cxios_set_domaingroup_data_nj(domaingroup_hdl, data_nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_nj
      END SUBROUTINE cxios_set_domaingroup_data_nj

      SUBROUTINE cxios_set_domaingroup_data_ibegin(domaingroup_hdl, data_ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_ibegin
      END SUBROUTINE cxios_set_domaingroup_data_ibegin

      SUBROUTINE cxios_set_domaingroup_data_jbegin(domaingroup_hdl, data_jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE  :: data_jbegin
      END SUBROUTINE cxios_set_domaingroup_data_jbegin

      SUBROUTINE cxios_set_domaingroup_zoom_ni(domaingroup_hdl, zoom_ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ni
      END SUBROUTINE cxios_set_domaingroup_zoom_ni

      SUBROUTINE cxios_set_domaingroup_zoom_nj(domaingroup_hdl, zoom_nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_nj
      END SUBROUTINE cxios_set_domaingroup_zoom_nj

      SUBROUTINE cxios_set_domaingroup_zoom_ibegin(domaingroup_hdl, zoom_ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ibegin
      END SUBROUTINE cxios_set_domaingroup_zoom_ibegin

      SUBROUTINE cxios_set_domaingroup_zoom_jbegin(domaingroup_hdl, zoom_jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_jbegin
      END SUBROUTINE cxios_set_domaingroup_zoom_jbegin

      SUBROUTINE cxios_set_domaingroup_data_n_index(domaingroup_hdl, data_n_index) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domaingroup_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_n_index
      END SUBROUTINE cxios_set_domaingroup_data_n_index

      SUBROUTINE cxios_set_domaingroup_data_i_index(domaingroup_hdl, data_i_index, data_i_index_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         INTEGER (kind = C_INT)     , DIMENSION(*) :: data_i_index
         INTEGER (kind = C_INT)     , VALUE        :: data_i_index_extent1
      END SUBROUTINE cxios_set_domaingroup_data_i_index

      SUBROUTINE cxios_set_domaingroup_data_j_index(domaingroup_hdl, data_j_index, data_j_index_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         INTEGER (kind = C_INT)     , DIMENSION(*) :: data_j_index
         INTEGER (kind = C_INT)     , VALUE        :: data_j_index_extent1
      END SUBROUTINE cxios_set_domaingroup_data_j_index

      SUBROUTINE cxios_set_domaingroup_lonvalue(domaingroup_hdl, lonvalue, lonvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: lonvalue
         INTEGER (kind = C_INT)     , VALUE        :: lonvalue_extent1
      END SUBROUTINE cxios_set_domaingroup_lonvalue

      SUBROUTINE cxios_set_domaingroup_latvalue(domaingroup_hdl, latvalue, latvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domaingroup_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: latvalue
         INTEGER (kind = C_INT)     , VALUE        :: latvalue_extent1
      END SUBROUTINE cxios_set_domaingroup_latvalue
      
      SUBROUTINE cxios_domaingroup_handle_create(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_domaingroup_handle_create

      SUBROUTINE cxios_domaingroup_valid_id(ret, idt, idt_size) BIND(C)
         USE ISO_C_BINDING
         LOGICAL  (kind = C_BOOL)                   :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE cxios_domaingroup_valid_id

   END INTERFACE
       
END MODULE DOMAINGROUP_INTERFACE
