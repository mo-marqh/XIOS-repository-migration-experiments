! * ************************************************************************** *
! *               Interface auto generated - do not modify                     *
! * ************************************************************************** *
#include "xios_fortran_prefix.hpp"

MODULE icontext_attr
  USE, INTRINSIC :: ISO_C_BINDING
  USE icontext
  USE context_interface_attr
  USE LOGICAL_BOOL_CONVERSION

CONTAINS

  SUBROUTINE xios(set_context_attr)  &
    ( context_id, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      LOGICAL  , OPTIONAL, INTENT(IN) :: attached_mode
      LOGICAL (KIND=C_BOOL) :: attached_mode_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_reader
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_reader
      LOGICAL  , OPTIONAL, INTENT(IN) :: default_using_server2
      LOGICAL (KIND=C_BOOL) :: default_using_server2_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir

      CALL xios(get_context_handle) &
      (context_id,context_hdl)
      CALL xios(set_context_attr_hdl_)   &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(set_context_attr)

  SUBROUTINE xios(set_context_attr_hdl)  &
    ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: attached_mode
      LOGICAL (KIND=C_BOOL) :: attached_mode_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_reader
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_reader
      LOGICAL  , OPTIONAL, INTENT(IN) :: default_using_server2
      LOGICAL (KIND=C_BOOL) :: default_using_server2_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir

      CALL xios(set_context_attr_hdl_)  &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(set_context_attr_hdl)

  SUBROUTINE xios(set_context_attr_hdl_)   &
    ( context_hdl, attached_mode_, default_gatherer_, default_pool_, default_pool_gatherer_, default_pool_reader_  &
    , default_pool_writer_, default_reader_, default_using_server2_, default_writer_, output_dir_  &
     )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL  , OPTIONAL, INTENT(IN) :: attached_mode_
      LOGICAL (KIND=C_BOOL) :: attached_mode__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_gatherer_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_gatherer_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_reader_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_pool_writer_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_reader_
      LOGICAL  , OPTIONAL, INTENT(IN) :: default_using_server2_
      LOGICAL (KIND=C_BOOL) :: default_using_server2__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: default_writer_
      CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: output_dir_

      IF (PRESENT(attached_mode_)) THEN
        attached_mode__tmp = attached_mode_
        CALL xios_logical_to_bool_0d(attached_mode__tmp)
        CALL cxios_set_context_attached_mode &
      (context_hdl%daddr, attached_mode__tmp)
      ENDIF

      IF (PRESENT(default_gatherer_)) THEN
        CALL cxios_set_context_default_gatherer &
      (context_hdl%daddr, default_gatherer_, len(default_gatherer_))
      ENDIF

      IF (PRESENT(default_pool_)) THEN
        CALL cxios_set_context_default_pool &
      (context_hdl%daddr, default_pool_, len(default_pool_))
      ENDIF

      IF (PRESENT(default_pool_gatherer_)) THEN
        CALL cxios_set_context_default_pool_gatherer &
      (context_hdl%daddr, default_pool_gatherer_, len(default_pool_gatherer_))
      ENDIF

      IF (PRESENT(default_pool_reader_)) THEN
        CALL cxios_set_context_default_pool_reader &
      (context_hdl%daddr, default_pool_reader_, len(default_pool_reader_))
      ENDIF

      IF (PRESENT(default_pool_writer_)) THEN
        CALL cxios_set_context_default_pool_writer &
      (context_hdl%daddr, default_pool_writer_, len(default_pool_writer_))
      ENDIF

      IF (PRESENT(default_reader_)) THEN
        CALL cxios_set_context_default_reader &
      (context_hdl%daddr, default_reader_, len(default_reader_))
      ENDIF

      IF (PRESENT(default_using_server2_)) THEN
        default_using_server2__tmp = default_using_server2_
        CALL xios_logical_to_bool_0d(default_using_server2__tmp)
        CALL cxios_set_context_default_using_server2 &
      (context_hdl%daddr, default_using_server2__tmp)
      ENDIF

      IF (PRESENT(default_writer_)) THEN
        CALL cxios_set_context_default_writer &
      (context_hdl%daddr, default_writer_, len(default_writer_))
      ENDIF

      IF (PRESENT(output_dir_)) THEN
        CALL cxios_set_context_output_dir &
      (context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF

  END SUBROUTINE xios(set_context_attr_hdl_)

  SUBROUTINE xios(get_context_attr)  &
    ( context_id, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      LOGICAL  , OPTIONAL, INTENT(OUT) :: attached_mode
      LOGICAL (KIND=C_BOOL) :: attached_mode_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_reader
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_reader
      LOGICAL  , OPTIONAL, INTENT(OUT) :: default_using_server2
      LOGICAL (KIND=C_BOOL) :: default_using_server2_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir

      CALL xios(get_context_handle) &
      (context_id,context_hdl)
      CALL xios(get_context_attr_hdl_)   &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(get_context_attr)

  SUBROUTINE xios(get_context_attr_hdl)  &
    ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: attached_mode
      LOGICAL (KIND=C_BOOL) :: attached_mode_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_gatherer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_reader
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_reader
      LOGICAL  , OPTIONAL, INTENT(OUT) :: default_using_server2
      LOGICAL (KIND=C_BOOL) :: default_using_server2_tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_writer
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir

      CALL xios(get_context_attr_hdl_)  &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(get_context_attr_hdl)

  SUBROUTINE xios(get_context_attr_hdl_)   &
    ( context_hdl, attached_mode_, default_gatherer_, default_pool_, default_pool_gatherer_, default_pool_reader_  &
    , default_pool_writer_, default_reader_, default_using_server2_, default_writer_, output_dir_  &
     )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL  , OPTIONAL, INTENT(OUT) :: attached_mode_
      LOGICAL (KIND=C_BOOL) :: attached_mode__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_gatherer_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_gatherer_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_reader_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_pool_writer_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_reader_
      LOGICAL  , OPTIONAL, INTENT(OUT) :: default_using_server2_
      LOGICAL (KIND=C_BOOL) :: default_using_server2__tmp
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: default_writer_
      CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: output_dir_

      IF (PRESENT(attached_mode_)) THEN
        CALL cxios_get_context_attached_mode &
      (context_hdl%daddr, attached_mode__tmp)
        CALL xios_bool_to_logical_0d(attached_mode__tmp)
        attached_mode_ = attached_mode__tmp
      ENDIF

      IF (PRESENT(default_gatherer_)) THEN
        CALL cxios_get_context_default_gatherer &
      (context_hdl%daddr, default_gatherer_, len(default_gatherer_))
      ENDIF

      IF (PRESENT(default_pool_)) THEN
        CALL cxios_get_context_default_pool &
      (context_hdl%daddr, default_pool_, len(default_pool_))
      ENDIF

      IF (PRESENT(default_pool_gatherer_)) THEN
        CALL cxios_get_context_default_pool_gatherer &
      (context_hdl%daddr, default_pool_gatherer_, len(default_pool_gatherer_))
      ENDIF

      IF (PRESENT(default_pool_reader_)) THEN
        CALL cxios_get_context_default_pool_reader &
      (context_hdl%daddr, default_pool_reader_, len(default_pool_reader_))
      ENDIF

      IF (PRESENT(default_pool_writer_)) THEN
        CALL cxios_get_context_default_pool_writer &
      (context_hdl%daddr, default_pool_writer_, len(default_pool_writer_))
      ENDIF

      IF (PRESENT(default_reader_)) THEN
        CALL cxios_get_context_default_reader &
      (context_hdl%daddr, default_reader_, len(default_reader_))
      ENDIF

      IF (PRESENT(default_using_server2_)) THEN
        CALL cxios_get_context_default_using_server2 &
      (context_hdl%daddr, default_using_server2__tmp)
        CALL xios_bool_to_logical_0d(default_using_server2__tmp)
        default_using_server2_ = default_using_server2__tmp
      ENDIF

      IF (PRESENT(default_writer_)) THEN
        CALL cxios_get_context_default_writer &
      (context_hdl%daddr, default_writer_, len(default_writer_))
      ENDIF

      IF (PRESENT(output_dir_)) THEN
        CALL cxios_get_context_output_dir &
      (context_hdl%daddr, output_dir_, len(output_dir_))
      ENDIF

  END SUBROUTINE xios(get_context_attr_hdl_)

  SUBROUTINE xios(is_defined_context_attr)  &
    ( context_id, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context))  :: context_hdl
      CHARACTER(LEN=*), INTENT(IN) ::context_id
      LOGICAL, OPTIONAL, INTENT(OUT) :: attached_mode
      LOGICAL(KIND=C_BOOL) :: attached_mode_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_gatherer
      LOGICAL(KIND=C_BOOL) :: default_gatherer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool
      LOGICAL(KIND=C_BOOL) :: default_pool_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_gatherer
      LOGICAL(KIND=C_BOOL) :: default_pool_gatherer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_reader
      LOGICAL(KIND=C_BOOL) :: default_pool_reader_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_writer
      LOGICAL(KIND=C_BOOL) :: default_pool_writer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_reader
      LOGICAL(KIND=C_BOOL) :: default_reader_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_using_server2
      LOGICAL(KIND=C_BOOL) :: default_using_server2_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_writer
      LOGICAL(KIND=C_BOOL) :: default_writer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir
      LOGICAL(KIND=C_BOOL) :: output_dir_tmp

      CALL xios(get_context_handle) &
      (context_id,context_hdl)
      CALL xios(is_defined_context_attr_hdl_)   &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(is_defined_context_attr)

  SUBROUTINE xios(is_defined_context_attr_hdl)  &
    ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
    , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: attached_mode
      LOGICAL(KIND=C_BOOL) :: attached_mode_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_gatherer
      LOGICAL(KIND=C_BOOL) :: default_gatherer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool
      LOGICAL(KIND=C_BOOL) :: default_pool_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_gatherer
      LOGICAL(KIND=C_BOOL) :: default_pool_gatherer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_reader
      LOGICAL(KIND=C_BOOL) :: default_pool_reader_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_writer
      LOGICAL(KIND=C_BOOL) :: default_pool_writer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_reader
      LOGICAL(KIND=C_BOOL) :: default_reader_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_using_server2
      LOGICAL(KIND=C_BOOL) :: default_using_server2_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_writer
      LOGICAL(KIND=C_BOOL) :: default_writer_tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir
      LOGICAL(KIND=C_BOOL) :: output_dir_tmp

      CALL xios(is_defined_context_attr_hdl_)  &
      ( context_hdl, attached_mode, default_gatherer, default_pool, default_pool_gatherer, default_pool_reader  &
      , default_pool_writer, default_reader, default_using_server2, default_writer, output_dir )

  END SUBROUTINE xios(is_defined_context_attr_hdl)

  SUBROUTINE xios(is_defined_context_attr_hdl_)   &
    ( context_hdl, attached_mode_, default_gatherer_, default_pool_, default_pool_gatherer_, default_pool_reader_  &
    , default_pool_writer_, default_reader_, default_using_server2_, default_writer_, output_dir_  &
     )

    IMPLICIT NONE
      TYPE(txios(context)) , INTENT(IN) :: context_hdl
      LOGICAL, OPTIONAL, INTENT(OUT) :: attached_mode_
      LOGICAL(KIND=C_BOOL) :: attached_mode__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_gatherer_
      LOGICAL(KIND=C_BOOL) :: default_gatherer__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_
      LOGICAL(KIND=C_BOOL) :: default_pool__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_gatherer_
      LOGICAL(KIND=C_BOOL) :: default_pool_gatherer__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_reader_
      LOGICAL(KIND=C_BOOL) :: default_pool_reader__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_pool_writer_
      LOGICAL(KIND=C_BOOL) :: default_pool_writer__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_reader_
      LOGICAL(KIND=C_BOOL) :: default_reader__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_using_server2_
      LOGICAL(KIND=C_BOOL) :: default_using_server2__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: default_writer_
      LOGICAL(KIND=C_BOOL) :: default_writer__tmp
      LOGICAL, OPTIONAL, INTENT(OUT) :: output_dir_
      LOGICAL(KIND=C_BOOL) :: output_dir__tmp

      IF (PRESENT(attached_mode_)) THEN
        attached_mode__tmp = cxios_is_defined_context_attached_mode &
      (context_hdl%daddr)
        attached_mode_ = attached_mode__tmp
      ENDIF

      IF (PRESENT(default_gatherer_)) THEN
        default_gatherer__tmp = cxios_is_defined_context_default_gatherer &
      (context_hdl%daddr)
        default_gatherer_ = default_gatherer__tmp
      ENDIF

      IF (PRESENT(default_pool_)) THEN
        default_pool__tmp = cxios_is_defined_context_default_pool &
      (context_hdl%daddr)
        default_pool_ = default_pool__tmp
      ENDIF

      IF (PRESENT(default_pool_gatherer_)) THEN
        default_pool_gatherer__tmp = cxios_is_defined_context_default_pool_gatherer &
      (context_hdl%daddr)
        default_pool_gatherer_ = default_pool_gatherer__tmp
      ENDIF

      IF (PRESENT(default_pool_reader_)) THEN
        default_pool_reader__tmp = cxios_is_defined_context_default_pool_reader &
      (context_hdl%daddr)
        default_pool_reader_ = default_pool_reader__tmp
      ENDIF

      IF (PRESENT(default_pool_writer_)) THEN
        default_pool_writer__tmp = cxios_is_defined_context_default_pool_writer &
      (context_hdl%daddr)
        default_pool_writer_ = default_pool_writer__tmp
      ENDIF

      IF (PRESENT(default_reader_)) THEN
        default_reader__tmp = cxios_is_defined_context_default_reader &
      (context_hdl%daddr)
        default_reader_ = default_reader__tmp
      ENDIF

      IF (PRESENT(default_using_server2_)) THEN
        default_using_server2__tmp = cxios_is_defined_context_default_using_server2 &
      (context_hdl%daddr)
        default_using_server2_ = default_using_server2__tmp
      ENDIF

      IF (PRESENT(default_writer_)) THEN
        default_writer__tmp = cxios_is_defined_context_default_writer &
      (context_hdl%daddr)
        default_writer_ = default_writer__tmp
      ENDIF

      IF (PRESENT(output_dir_)) THEN
        output_dir__tmp = cxios_is_defined_context_output_dir &
      (context_hdl%daddr)
        output_dir_ = output_dir__tmp
      ENDIF

  END SUBROUTINE xios(is_defined_context_attr_hdl_)

END MODULE icontext_attr
