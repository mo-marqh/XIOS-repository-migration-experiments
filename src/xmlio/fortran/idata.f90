MODULE IDATA
   USE, INTRINSIC :: ISO_C_BINDING
   USE ICONTEXT
   
   INTERFACE ! Ne pas appeler directement/Interface FORTRAN 2003 <-> C99

      SUBROUTINE  xios_init_ioserver(comm_client) BIND(C)
         USE ISO_C_BINDING
         INTEGER  (kind = C_INT) :: comm_client
      END SUBROUTINE xios_init_ioserver

      SUBROUTINE xios_dtreatment_start(context_hdl, filetype, comm_client_server, comm_server) BIND(C)
         USE ISO_C_BINDING
         INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
         INTEGER (kind = C_INT)     , VALUE :: filetype, comm_client_server, comm_server
      END SUBROUTINE xios_dtreatment_start

      SUBROUTINE xios_dtreatment_end() BIND(C)
         ! Sans argument
      END SUBROUTINE xios_dtreatment_end

      SUBROUTINE xios_write_data_k81(fieldid, fieldid_size, data_k8, data_Xsize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_DOUBLE), DIMENSION(*) :: data_k8
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize
      END SUBROUTINE xios_write_data_k81
      
      SUBROUTINE xios_write_data_k82(fieldid, fieldid_size, data_k8, data_Xsize, data_Ysize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_DOUBLE), DIMENSION(*) :: data_k8
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize, data_Ysize
      END SUBROUTINE xios_write_data_k82
      
      SUBROUTINE xios_write_data_k83(fieldid, fieldid_size, data_k8, data_Xsize, data_Ysize, data_Zsize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_DOUBLE), DIMENSION(*) :: data_k8
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize, data_Ysize, data_Zsize
      END SUBROUTINE xios_write_data_k83
      
      SUBROUTINE xios_write_data_k41(fieldid, fieldid_size, data_k4, data_Xsize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_FLOAT) , DIMENSION(*) :: data_k4
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize
      END SUBROUTINE xios_write_data_k41
      
      SUBROUTINE xios_write_data_k42(fieldid, fieldid_size, data_k4, data_Xsize, data_Ysize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_FLOAT) , DIMENSION(*) :: data_k4
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize, data_Ysize
      END SUBROUTINE xios_write_data_k42
      
      SUBROUTINE xios_write_data_k43(fieldid, fieldid_size, data_k4, data_Xsize, data_Ysize, data_Zsize) BIND(C)
         USE ISO_C_BINDING
         CHARACTER(kind = C_CHAR)  , DIMENSION(*) :: fieldid
         REAL     (kind = C_FLOAT) , DIMENSION(*) :: data_k4
         INTEGER  (kind = C_INT)   , VALUE        :: fieldid_size
         INTEGER  (kind = C_INT)   , VALUE        :: data_Xsize, data_Ysize, data_Zsize
      END SUBROUTINE xios_write_data_k43
      
   END INTERFACE
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.

   SUBROUTINE  init_ioserver(comm_client)
      INTEGER, INTENT(INOUT) :: comm_client
      CALL xios_init_ioserver(comm_client)
   END SUBROUTINE init_ioserver
   
   SUBROUTINE dtreatment_start(context_hdl, filetype, comm_client_server, comm_server)
      TYPE(XContextHandle), INTENT(IN), VALUE    :: context_hdl
      INTEGER             , INTENT(IN), OPTIONAL :: filetype , comm_client_server , comm_server
      INTEGER                                    :: filetype_, comm_client_server_, comm_server_
      IF (PRESENT(filetype)) THEN
         filetype_ = filetype
      ELSE
         filetype_ = NETCDF4
      END IF
      IF (PRESENT(comm_client_server)) THEN
         comm_client_server_ = comm_client_server
      ELSE
         comm_client_server_ = -1
      END IF
      IF (PRESENT(comm_server)) THEN
         comm_server_ = comm_server
      ELSE
         comm_server_ = -1
      END IF
      CALL context_set_current(context_hdl)
      CALL xios_dtreatment_start(context_hdl%daddr, filetype_, comm_client_server_, comm_server_)
   END SUBROUTINE dtreatment_start
   
   SUBROUTINE dtreatment_end(context_hdl)
      TYPE(XContextHandle), INTENT(IN), VALUE :: context_hdl
      CALL context_set_current(context_hdl)
      CALL xios_dtreatment_end()
   END SUBROUTINE dtreatment_end
   
   SUBROUTINE write_data_k81(fieldid, data1d_k8)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 8), DIMENSION(*), INTENT(IN) :: data1d_k8(:)
      CALL xios_write_data_k81(fieldid, len(fieldid), data1d_k8, size(data1d_k8, 1))
   END SUBROUTINE write_data_k81
   
   SUBROUTINE write_data_k82(fieldid, data2d_k8)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 8), DIMENSION(*), INTENT(IN) :: data2d_k8(:,:)
      CALL xios_write_data_k82(fieldid, len(fieldid), data2d_k8, size(data2d_k8, 1), size(data2d_k8, 2))
   END SUBROUTINE write_data_k82
   
   SUBROUTINE write_data_k83(fieldid, data3d_k8)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 8), DIMENSION(*), INTENT(IN) :: data3d_k8(:,:,:)
      CALL xios_write_data_k83(fieldid, len(fieldid), data3d_k8, size(data3d_k8, 1), size(data3d_k8, 2), size(data3d_k8, 3))
   END SUBROUTINE write_data_k83
   
   SUBROUTINE write_data_k41(fieldid, data1d_k4)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 4), DIMENSION(*), INTENT(IN) :: data1d_k4(:)
      CALL xios_write_data_k41(fieldid, len(fieldid), data1d_k4, size(data1d_k4, 1))
   END SUBROUTINE write_data_k41
   
   SUBROUTINE write_data_k42(fieldid, data2d_k4)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 4), DIMENSION(*), INTENT(IN) :: data2d_k4(:,:)
      CALL xios_write_data_k42(fieldid, len(fieldid), data2d_k4, size(data2d_k4, 1), size(data2d_k4, 2))
   END SUBROUTINE write_data_k42
   
   SUBROUTINE write_data_k43(fieldid, data3d_k4)
      CHARACTER(len = *)               , INTENT(IN) :: fieldid
      REAL     (kind = 4), DIMENSION(*), INTENT(IN) :: data3d_k4(:,:,:)
      CALL xios_write_data_k43(fieldid, len(fieldid), data3d_k4, size(data3d_k4, 1), size(data3d_k4, 2), size(data3d_k4, 3))
   END SUBROUTINE write_data_k43
   
END MODULE IDATA
