! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "../fortran/xios_fortran_prefix.hpp"

MODULE context_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_context_attached_mode(context_hdl, attached_mode) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: attached_mode
    END SUBROUTINE cxios_set_context_attached_mode

    SUBROUTINE cxios_get_context_attached_mode(context_hdl, attached_mode) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      LOGICAL (KIND=C_BOOL)             :: attached_mode
    END SUBROUTINE cxios_get_context_attached_mode

    FUNCTION cxios_is_defined_context_attached_mode(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_attached_mode
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_attached_mode


    SUBROUTINE cxios_set_context_default_gatherer(context_hdl, default_gatherer, default_gatherer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_gatherer
      INTEGER  (kind = C_INT)     , VALUE        :: default_gatherer_size
    END SUBROUTINE cxios_set_context_default_gatherer

    SUBROUTINE cxios_get_context_default_gatherer(context_hdl, default_gatherer, default_gatherer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_gatherer
      INTEGER  (kind = C_INT)     , VALUE        :: default_gatherer_size
    END SUBROUTINE cxios_get_context_default_gatherer

    FUNCTION cxios_is_defined_context_default_gatherer(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_gatherer
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_gatherer


    SUBROUTINE cxios_set_context_default_pool(context_hdl, default_pool, default_pool_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_size
    END SUBROUTINE cxios_set_context_default_pool

    SUBROUTINE cxios_get_context_default_pool(context_hdl, default_pool, default_pool_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_size
    END SUBROUTINE cxios_get_context_default_pool

    FUNCTION cxios_is_defined_context_default_pool(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_pool
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_pool


    SUBROUTINE cxios_set_context_default_pool_gatherer(context_hdl, default_pool_gatherer, default_pool_gatherer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_gatherer
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_gatherer_size
    END SUBROUTINE cxios_set_context_default_pool_gatherer

    SUBROUTINE cxios_get_context_default_pool_gatherer(context_hdl, default_pool_gatherer, default_pool_gatherer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_gatherer
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_gatherer_size
    END SUBROUTINE cxios_get_context_default_pool_gatherer

    FUNCTION cxios_is_defined_context_default_pool_gatherer(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_pool_gatherer
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_pool_gatherer


    SUBROUTINE cxios_set_context_default_pool_reader(context_hdl, default_pool_reader, default_pool_reader_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_reader
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_reader_size
    END SUBROUTINE cxios_set_context_default_pool_reader

    SUBROUTINE cxios_get_context_default_pool_reader(context_hdl, default_pool_reader, default_pool_reader_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_reader
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_reader_size
    END SUBROUTINE cxios_get_context_default_pool_reader

    FUNCTION cxios_is_defined_context_default_pool_reader(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_pool_reader
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_pool_reader


    SUBROUTINE cxios_set_context_default_pool_writer(context_hdl, default_pool_writer, default_pool_writer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_writer
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_writer_size
    END SUBROUTINE cxios_set_context_default_pool_writer

    SUBROUTINE cxios_get_context_default_pool_writer(context_hdl, default_pool_writer, default_pool_writer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_pool_writer
      INTEGER  (kind = C_INT)     , VALUE        :: default_pool_writer_size
    END SUBROUTINE cxios_get_context_default_pool_writer

    FUNCTION cxios_is_defined_context_default_pool_writer(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_pool_writer
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_pool_writer


    SUBROUTINE cxios_set_context_default_reader(context_hdl, default_reader, default_reader_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_reader
      INTEGER  (kind = C_INT)     , VALUE        :: default_reader_size
    END SUBROUTINE cxios_set_context_default_reader

    SUBROUTINE cxios_get_context_default_reader(context_hdl, default_reader, default_reader_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_reader
      INTEGER  (kind = C_INT)     , VALUE        :: default_reader_size
    END SUBROUTINE cxios_get_context_default_reader

    FUNCTION cxios_is_defined_context_default_reader(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_reader
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_reader


    SUBROUTINE cxios_set_context_default_using_server2(context_hdl, default_using_server2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      LOGICAL (KIND=C_BOOL)      , VALUE :: default_using_server2
    END SUBROUTINE cxios_set_context_default_using_server2

    SUBROUTINE cxios_get_context_default_using_server2(context_hdl, default_using_server2) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      LOGICAL (KIND=C_BOOL)             :: default_using_server2
    END SUBROUTINE cxios_get_context_default_using_server2

    FUNCTION cxios_is_defined_context_default_using_server2(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_using_server2
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_using_server2


    SUBROUTINE cxios_set_context_default_writer(context_hdl, default_writer, default_writer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_writer
      INTEGER  (kind = C_INT)     , VALUE        :: default_writer_size
    END SUBROUTINE cxios_set_context_default_writer

    SUBROUTINE cxios_get_context_default_writer(context_hdl, default_writer, default_writer_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: default_writer
      INTEGER  (kind = C_INT)     , VALUE        :: default_writer_size
    END SUBROUTINE cxios_get_context_default_writer

    FUNCTION cxios_is_defined_context_default_writer(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_default_writer
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_default_writer


    SUBROUTINE cxios_set_context_output_dir(context_hdl, output_dir, output_dir_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_dir
      INTEGER  (kind = C_INT)     , VALUE        :: output_dir_size
    END SUBROUTINE cxios_set_context_output_dir

    SUBROUTINE cxios_get_context_output_dir(context_hdl, output_dir, output_dir_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
      CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: output_dir
      INTEGER  (kind = C_INT)     , VALUE        :: output_dir_size
    END SUBROUTINE cxios_get_context_output_dir

    FUNCTION cxios_is_defined_context_output_dir(context_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_context_output_dir
      INTEGER (kind = C_INTPTR_T), VALUE :: context_hdl
    END FUNCTION cxios_is_defined_context_output_dir

  END INTERFACE

END MODULE context_interface_attr
