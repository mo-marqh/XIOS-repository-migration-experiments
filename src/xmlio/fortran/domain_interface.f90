MODULE DOMAIN_INTERFACE
   USE, INTRINSIC :: ISO_C_BINDING
       
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99
       
      SUBROUTINE xios_set_domain_name(domain_hdl, name, name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domain_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: name
         INTEGER  (kind = C_INT)     , VALUE        :: name_size
      END SUBROUTINE xios_set_domain_name

      SUBROUTINE xios_set_domain_standard_name(domain_hdl, standard_name, standard_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domain_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: standard_name
         INTEGER  (kind = C_INT)     , VALUE        :: standard_name_size
      END SUBROUTINE xios_set_domain_standard_name

      SUBROUTINE xios_set_domain_long_name(domain_hdl, long_name, long_name_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domain_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: long_name
         INTEGER  (kind = C_INT)     , VALUE        :: long_name_size
      END SUBROUTINE xios_set_domain_long_name

      SUBROUTINE xios_set_domain_domain_group_ref(domain_hdl, domain_group_ref, domain_group_ref_size) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INTPTR_T), VALUE        :: domain_hdl
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: domain_group_ref
         INTEGER  (kind = C_INT)     , VALUE        :: domain_group_ref_size
      END SUBROUTINE xios_set_domain_domain_group_ref

      SUBROUTINE xios_set_domain_ni_glo(domain_hdl, ni_glo) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: ni_glo
      END SUBROUTINE xios_set_domain_ni_glo

      SUBROUTINE xios_set_domain_nj_glo(domain_hdl, nj_glo) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: nj_glo
      END SUBROUTINE xios_set_domain_nj_glo

      SUBROUTINE xios_set_domain_ibegin(domain_hdl, ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: ibegin
      END SUBROUTINE xios_set_domain_ibegin

      SUBROUTINE xios_set_domain_iend(domain_hdl, iend) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: iend
      END SUBROUTINE xios_set_domain_iend

      SUBROUTINE xios_set_domain_ni(domain_hdl, ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: ni
      END SUBROUTINE xios_set_domain_ni

      SUBROUTINE xios_set_domain_jbegin(domain_hdl, jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: jbegin
      END SUBROUTINE xios_set_domain_jbegin

      SUBROUTINE xios_set_domain_jend(domain_hdl, jend) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: jend
      END SUBROUTINE xios_set_domain_jend

      SUBROUTINE xios_set_domain_nj(domain_hdl, nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: nj
      END SUBROUTINE xios_set_domain_nj

      SUBROUTINE xios_set_domain_mask(domain_hdl, mask, mask_extent1, mask_extent2) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domain_hdl
         LOGICAL(kind = C_BOOL)     , DIMENSION(*) :: mask
         INTEGER (kind = C_INT)     , VALUE        :: mask_extent1
         INTEGER (kind = C_INT)     , VALUE        :: mask_extent2
      END SUBROUTINE xios_set_domain_mask

      SUBROUTINE xios_set_domain_data_dim(domain_hdl, data_dim) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_dim
      END SUBROUTINE xios_set_domain_data_dim

      SUBROUTINE xios_set_domain_data_ni(domain_hdl, data_ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_ni
      END SUBROUTINE xios_set_domain_data_ni

      SUBROUTINE xios_set_domain_data_nj(domain_hdl, data_nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_nj
      END SUBROUTINE xios_set_domain_data_nj

      SUBROUTINE xios_set_domain_data_ibegin(domain_hdl, data_ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_ibegin
      END SUBROUTINE xios_set_domain_data_ibegin

      SUBROUTINE xios_set_domain_data_jbegin(domain_hdl, data_jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_jbegin
      END SUBROUTINE xios_set_domain_data_jbegin

      SUBROUTINE xios_set_domain_zoom_ni(domain_hdl, zoom_ni) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ni
      END SUBROUTINE xios_set_domain_zoom_ni

      SUBROUTINE xios_set_domain_zoom_nj(domain_hdl, zoom_nj) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_nj
      END SUBROUTINE xios_set_domain_zoom_nj

      SUBROUTINE xios_set_domain_zoom_ibegin(domain_hdl, zoom_ibegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ibegin
      END SUBROUTINE xios_set_domain_zoom_ibegin

      SUBROUTINE xios_set_domain_zoom_jbegin(domain_hdl, zoom_jbegin) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_jbegin
      END SUBROUTINE xios_set_domain_zoom_jbegin

      SUBROUTINE xios_set_domain_zoom_ni_loc(domain_hdl, zoom_ni_loc) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ni_loc
      END SUBROUTINE xios_set_domain_zoom_ni_loc

      SUBROUTINE xios_set_domain_zoom_nj_loc(domain_hdl, zoom_nj_loc) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_nj_loc
      END SUBROUTINE xios_set_domain_zoom_nj_loc

      SUBROUTINE xios_set_domain_zoom_ibegin_loc(domain_hdl, zoom_ibegin_loc) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_ibegin_loc
      END SUBROUTINE xios_set_domain_zoom_ibegin_loc

      SUBROUTINE xios_set_domain_zoom_jbegin_loc(domain_hdl, zoom_jbegin_loc) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: zoom_jbegin_loc
      END SUBROUTINE xios_set_domain_zoom_jbegin_loc

      SUBROUTINE xios_set_domain_data_n_index(domain_hdl, data_n_index) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: domain_hdl
         INTEGER (kind = C_INT)     , VALUE :: data_n_index
      END SUBROUTINE xios_set_domain_data_n_index

      SUBROUTINE xios_set_domain_data_i_index(domain_hdl, data_i_index, data_i_index_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domain_hdl
         INTEGER (kind = C_INT)     , DIMENSION(*) :: data_i_index
         INTEGER (kind = C_INT)     , VALUE        :: data_i_index_extent1
      END SUBROUTINE xios_set_domain_data_i_index

      SUBROUTINE xios_set_domain_data_j_index(domain_hdl, data_j_index, data_j_index_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domain_hdl
         INTEGER (kind = C_INT)     , DIMENSION(*) :: data_j_index
         INTEGER (kind = C_INT)     , VALUE        :: data_j_index_extent1
      END SUBROUTINE xios_set_domain_data_j_index

      SUBROUTINE xios_set_domain_lonvalue(domain_hdl, lonvalue, lonvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domain_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: lonvalue
         INTEGER (kind = C_INT)     , VALUE        :: lonvalue_extent1
      END SUBROUTINE xios_set_domain_lonvalue

      SUBROUTINE xios_set_domain_latvalue(domain_hdl, latvalue, latvalue_extent1) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE        :: domain_hdl
         REAL    (kind = C_DOUBLE)  , DIMENSION(*) :: latvalue
         INTEGER (kind = C_INT)     , VALUE        :: latvalue_extent1
      END SUBROUTINE xios_set_domain_latvalue
       
      SUBROUTINE xios_domain_handle_create(ret, idt, idt_size) BIND(C)
         import C_CHAR, C_INTPTR_T, C_INT
         INTEGER  (kind = C_INTPTR_T)               :: ret
         CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: idt
         INTEGER  (kind = C_INT)     , VALUE        :: idt_size
      END SUBROUTINE xios_domain_handle_create
       
   END INTERFACE
       
END MODULE DOMAIN_INTERFACE
