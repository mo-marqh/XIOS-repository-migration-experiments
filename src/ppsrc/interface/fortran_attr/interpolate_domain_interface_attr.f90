! * ************************************************************************** *
! * Interface auto generated - do not modify *
! * ************************************************************************** *


MODULE interpolate_domain_interface_attr
  USE, INTRINSIC :: ISO_C_BINDING

  INTERFACE
    ! Do not call directly / interface FORTRAN 2003 <-> C99

    SUBROUTINE cxios_set_interpolate_domain_file(interpolate_domain_hdl, file, file_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      CHARACTER(kind = C_CHAR) , DIMENSION(*) :: file
      INTEGER (kind = C_INT) , VALUE :: file_size
    END SUBROUTINE cxios_set_interpolate_domain_file

    SUBROUTINE cxios_get_interpolate_domain_file(interpolate_domain_hdl, file, file_size) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      CHARACTER(kind = C_CHAR) , DIMENSION(*) :: file
      INTEGER (kind = C_INT) , VALUE :: file_size
    END SUBROUTINE cxios_get_interpolate_domain_file

    FUNCTION cxios_is_defined_interpolate_domain_file(interpolate_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_interpolate_domain_file
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
    END FUNCTION cxios_is_defined_interpolate_domain_file


    SUBROUTINE cxios_set_interpolate_domain_order(interpolate_domain_hdl, order) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      INTEGER (KIND=C_INT) , VALUE :: order
    END SUBROUTINE cxios_set_interpolate_domain_order

    SUBROUTINE cxios_get_interpolate_domain_order(interpolate_domain_hdl, order) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      INTEGER (KIND=C_INT) :: order
    END SUBROUTINE cxios_get_interpolate_domain_order

    FUNCTION cxios_is_defined_interpolate_domain_order(interpolate_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_interpolate_domain_order
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
    END FUNCTION cxios_is_defined_interpolate_domain_order


    SUBROUTINE cxios_set_interpolate_domain_renormalize(interpolate_domain_hdl, renormalize) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      LOGICAL (KIND=C_BOOL) , VALUE :: renormalize
    END SUBROUTINE cxios_set_interpolate_domain_renormalize

    SUBROUTINE cxios_get_interpolate_domain_renormalize(interpolate_domain_hdl, renormalize) BIND(C)
      USE ISO_C_BINDING
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
      LOGICAL (KIND=C_BOOL) :: renormalize
    END SUBROUTINE cxios_get_interpolate_domain_renormalize

    FUNCTION cxios_is_defined_interpolate_domain_renormalize(interpolate_domain_hdl) BIND(C)
      USE ISO_C_BINDING
      LOGICAL(kind=C_BOOL) :: cxios_is_defined_interpolate_domain_renormalize
      INTEGER (kind = C_INTPTR_T), VALUE :: interpolate_domain_hdl
    END FUNCTION cxios_is_defined_interpolate_domain_renormalize

  END INTERFACE

END MODULE interpolate_domain_interface_attr
