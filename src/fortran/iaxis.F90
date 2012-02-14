#include "xios_fortran_prefix.hpp"

MODULE IAXIS
   USE, INTRINSIC :: ISO_C_BINDING
   USE AXIS_INTERFACE
   USE AXISGROUP_INTERFACE

   TYPE txios(axis)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(axis)
   
   TYPE txios(axisgroup)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(axisgroup)
   

         
   CONTAINS ! Fonctions disponibles pour les utilisateurs.



   SUBROUTINE xios(set_axis_attr)(axis_id, name, standard_name, long_name, unit, size, value)
      IMPLICIT NONE
      TYPE(txios(axis))                                     :: axis_hdl
      CHARACTER(len = *)                        , INTENT(IN) :: axis_id
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit
      INTEGER                         , OPTIONAL, INTENT(IN) :: size
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value(:)
      
      CALL xios(get_axis_handle)(axis_id,axis_hdl)
      CALL xios(set_axis_attr_hdl_)(axis_hdl, name, standard_name, long_name, unit, size, value)

   END SUBROUTINE xios(set_axis_attr)
   

   SUBROUTINE xios(set_axis_attr_hdl)(axis_hdl, name, standard_name, long_name, unit, size, value)
      IMPLICIT NONE
      TYPE(txios(axis))                        , INTENT(IN) :: axis_hdl
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit
      INTEGER                         , OPTIONAL, INTENT(IN) :: size
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value(:)

      CALL xios(set_axis_attr_hdl_)(axis_hdl, name, standard_name, long_name, unit, size, value)

   END SUBROUTINE xios(set_axis_attr_hdl)
   
   
   SUBROUTINE xios(set_axis_attr_hdl_)(axis_hdl, name_, standard_name_, long_name_, unit_, size_, value_)
      IMPLICIT NONE
      TYPE(txios(axis))                        , INTENT(IN) :: axis_hdl
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit_
      INTEGER                         , OPTIONAL, INTENT(IN) :: size_
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value_(:)
      
      IF (PRESENT(name_))           THEN
         CALL cxios_set_axis_name(axis_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))  THEN
         CALL cxios_set_axis_standard_name(axis_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))      THEN
         CALL cxios_set_axis_long_name(axis_hdl%daddr, long_name_, len(long_name_))
      END IF
      IF (PRESENT(unit_))           THEN
         CALL cxios_set_axis_unit(axis_hdl%daddr, unit_, len(unit_))
      END IF
      IF (PRESENT(size_))           THEN
         CALL cxios_set_axis_size(axis_hdl%daddr, size_)
      END IF
      IF (PRESENT(value_))         THEN
         CALL cxios_set_axis_zvalue(axis_hdl%daddr, value_, size(value_, 1))
      END IF
      
   END SUBROUTINE xios(set_axis_attr_hdl_)

   
   SUBROUTINE xios(set_axisgroup_attr)(axisgroup_id, name, standard_name, long_name, unit, size, value)
      IMPLICIT NONE
      TYPE(txios(axisgroup))                                :: axisgroup_hdl
      CHARACTER(len = *)                        , INTENT(IN) :: axisgroup_id
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit
      INTEGER                         , OPTIONAL, INTENT(IN) :: size
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value(:)

      CALL xios(get_axisgroup_handle)(axisgroup_id,axisgroup_hdl)
      CALL xios(set_axisgroup_attr_hdl_)(axisgroup_hdl, name, standard_name, long_name, unit, size, value)

   END SUBROUTINE xios(set_axisgroup_attr)
   

   SUBROUTINE xios(set_axisgroup_attr_hdl)(axisgroup_hdl, name, standard_name, long_name, unit, size, value)
      IMPLICIT NONE
      TYPE(txios(axisgroup))                   , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit
      INTEGER                         , OPTIONAL, INTENT(IN) :: size
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value(:)

      CALL xios(set_axisgroup_attr_hdl_)(axisgroup_hdl, name, standard_name, long_name, unit, size, value)      

   END SUBROUTINE xios(set_axisgroup_attr_hdl)

      
   SUBROUTINE xios(set_axisgroup_attr_hdl_)(axisgroup_hdl, name_, standard_name_, long_name_, unit_, size_, value_)
      IMPLICIT NONE
      TYPE(txios(axisgroup))                   , INTENT(IN) :: axisgroup_hdl
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: standard_name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: long_name_
      CHARACTER(len = *)              , OPTIONAL, INTENT(IN) :: unit_
      INTEGER                         , OPTIONAL, INTENT(IN) :: size_
      REAL(kind=8), dimension(*), OPTIONAL, INTENT(IN) :: value_(:)
      
      IF (PRESENT(name_))           THEN
         CALL cxios_set_axisgroup_name(axisgroup_hdl%daddr, name_, len(name_))
      END IF
      IF (PRESENT(standard_name_))  THEN
         CALL cxios_set_axisgroup_standard_name(axisgroup_hdl%daddr, standard_name_, len(standard_name_))
      END IF
      IF (PRESENT(long_name_))      THEN
         CALL cxios_set_axisgroup_long_name(axisgroup_hdl%daddr, long_name_, len(long_name_))
      END IF
      IF (PRESENT(unit_))           THEN
         CALL cxios_set_axisgroup_unit(axisgroup_hdl%daddr, unit_, len(unit_))
      END IF
      IF (PRESENT(size_))           THEN
         CALL cxios_set_axisgroup_size(axisgroup_hdl%daddr, size_)
      END IF
      IF (PRESENT(value_))         THEN
         CALL cxios_set_axisgroup_zvalue(axisgroup_hdl%daddr, value_, size(value_, 1))
      END IF
   END SUBROUTINE xios(set_axisgroup_attr_hdl_)
   

   SUBROUTINE xios(get_axis_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *), INTENT(IN) :: idt      
      TYPE(txios(axis)) , INTENT(OUT):: ret
      CALL cxios_axis_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE xios(get_axis_handle)
   
   SUBROUTINE xios(get_axisgroup_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)    , INTENT(IN) :: idt      
      TYPE(txios(axisgroup)), INTENT(OUT):: ret

      CALL cxios_axisgroup_handle_create(ret%daddr, idt, len(idt))            

   END SUBROUTINE xios(get_axisgroup_handle)

   LOGICAL FUNCTION xios(is_valid_axis)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      
      CALL cxios_axis_valid_id(val, idt, len(idt))
      xios(is_valid_axis) = val

   END FUNCTION  xios(is_valid_axis)

   LOGICAL FUNCTION xios(is_valid_axisgroup)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_axisgroup_valid_id(val, idt, len(idt))
      xios(is_valid_axisgroup) = val

   END FUNCTION  xios(is_valid_axisgroup)

END MODULE IAXIS
