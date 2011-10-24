#include "xios_fortran_prefix.hpp"

MODULE ICONTEXT
   USE, INTRINSIC :: ISO_C_BINDING
   USE CONTEXT_INTERFACE
   USE IDATE

   TYPE XContextHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XContextHandle
    
   TYPE txios(context)
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE txios(context)
      
   !----------------------------------------------------------------------------
   INTERFACE set_context_attributes
      MODULE PROCEDURE set_context_attributes_id,set_context_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
   SUBROUTINE xios(set_context_attr)( context_id, calendar_type, start_date, output_dir)
      IMPLICIT NONE
      CHARACTER(len = *)            , INTENT(IN) :: context_id
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir
         
      CALL xios(set_context_attr_)( context_id, calendar_type, start_date, output_dir)
   END SUBROUTINE xios(set_context_attr)


   SUBROUTINE xios(set_context_attr_)( context_id, calendar_type_, start_date_, output_dir_)
      IMPLICIT NONE
      CHARACTER(len = *)            , INTENT(IN) :: context_id
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir_
      TYPE(txios(context))                      :: context_hdl
         
      CALL xios(get_context_handle)(context_id,context_hdl)
      CALL xios(set_context_attr_hdl_)( context_hdl, calendar_type_, start_date_, output_dir_)
   END SUBROUTINE xios(set_context_attr_)


   SUBROUTINE xios(set_context_attr_hdl)( context_hdl, calendar_type, start_date, output_dir)
      IMPLICIT NONE
      TYPE(txios(context))          , INTENT(IN) :: context_hdl
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir   
        
      CALL  xios(set_context_attr_hdl_)( context_hdl, calendar_type, start_date, output_dir)  

   END SUBROUTINE xios(set_context_attr_hdl)

   SUBROUTINE xios(set_context_attr_hdl_)( context_hdl, calendar_type_, start_date_, output_dir_)
      IMPLICIT NONE
      TYPE(txios(context))          , INTENT(IN) :: context_hdl
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir_   
         
      IF (PRESENT(calendar_type_)) THEN
         CALL cxios_set_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      END IF
      IF (PRESENT(start_date_))    THEN
         CALL cxios_set_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      END IF
      IF (PRESENT(output_dir_))    THEN
         CALL cxios_set_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      END IF
   END SUBROUTINE xios(set_context_attr_hdl_)



   SUBROUTINE xios(get_context_handle)(idt,ret)
      IMPLICIT NONE
      CHARACTER(len = *)  , INTENT(IN)  :: idt      
      TYPE(txios(context)), INTENT(OUT):: ret

      CALL cxios_context_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE xios(get_context_handle)
   
   SUBROUTINE xios(set_current_context)(context, withswap)
      IMPLICIT NONE

      TYPE(txios(context))          , INTENT(IN) :: context
      LOGICAL             , OPTIONAL, INTENT(IN) :: withswap
      LOGICAL (kind = 1)                       :: wswap

      IF (PRESENT(withswap)) THEN
         wswap = withswap
      ELSE
         wswap = .FALSE.
      END IF
      CALL cxios_context_set_current(context%daddr, wswap)

   END SUBROUTINE xios(set_current_context)
   
   
!   SUBROUTINE context_create(context_hdl, context_id, calendar_type, init_date)
!      TYPE(XContextHandle)          , INTENT(OUT) :: context_hdl
!      CHARACTER(len = *)            , INTENT(IN)  :: context_id
!      INTEGER                       , INTENT(IN)  :: calendar_type
!      TYPE(XDate)         , OPTIONAL, INTENT(IN)  :: init_date
!     IF (PRESENT(init_date)) THEN
!         CALL cxios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
!                                  init_date%year, init_date%month, init_date%day, &
!                                  init_date%hour, init_date%minute, init_date%second)
!      ELSE
!         CALL cxios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
!                                 0, 1, 1, 0, 0, 0)
!      END IF
!   END SUBROUTINE context_create

   LOGICAL FUNCTION xios(is_valid_context)(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val

      CALL cxios_context_valid_id(val, idt, len(idt));
      xios(is_valid_context) = val

   END FUNCTION  xios(is_valid_context)




















!!!!!!!! ancienne interface

   SUBROUTINE set_context_attributes_id( context_id, calendar_type_, start_date_, output_dir_)
      IMPLICIT NONE
      TYPE(XContextHandle)                       :: context_hdl
      CHARACTER(len = *)            , INTENT(IN) :: context_id
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir_
         
      CALL context_handle_create(context_hdl, context_id)
      CALL set_context_attributes_hdl( context_hdl, calendar_type_, start_date_, output_dir_)
   END SUBROUTINE set_context_attributes_id

   SUBROUTINE set_context_attributes_hdl( context_hdl, calendar_type_, start_date_, output_dir_)
      IMPLICIT NONE
      TYPE(XContextHandle)          , INTENT(IN) :: context_hdl
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: calendar_type_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: start_date_
      CHARACTER(len = *)  , OPTIONAL, INTENT(IN) :: output_dir_   
         
      IF (PRESENT(calendar_type_)) THEN
         CALL cxios_set_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      END IF
      IF (PRESENT(start_date_))    THEN
         CALL cxios_set_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      END IF
      IF (PRESENT(output_dir_))    THEN
         CALL cxios_set_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      END IF
   END SUBROUTINE set_context_attributes_hdl

   SUBROUTINE context_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XContextHandle), INTENT(OUT):: ret
      CHARACTER(len = *)  , INTENT(IN) :: idt      
      CALL cxios_context_handle_create(ret%daddr, idt, len(idt))            
   END SUBROUTINE context_handle_create
   
   SUBROUTINE context_set_current(context, withswap)
      TYPE(XContextHandle)          , INTENT(IN) :: context
      LOGICAL             , OPTIONAL, INTENT(IN) :: withswap
      LOGICAL (kind = 1)                       :: wswap
      IF (PRESENT(withswap)) THEN
         wswap = withswap
      ELSE
         wswap = .FALSE.
      END IF
      CALL cxios_context_set_current(context%daddr, wswap)
   END SUBROUTINE context_set_current
   
   SUBROUTINE context_create(context_hdl, context_id, calendar_type, init_date)
      TYPE(XContextHandle)          , INTENT(OUT) :: context_hdl
      CHARACTER(len = *)            , INTENT(IN)  :: context_id
      INTEGER                       , INTENT(IN)  :: calendar_type
      TYPE(XDate)         , OPTIONAL, INTENT(IN)  :: init_date
      IF (PRESENT(init_date)) THEN
         CALL cxios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
                                  init_date%year, init_date%month, init_date%day, &
                                  init_date%hour, init_date%minute, init_date%second)
      ELSE
         CALL cxios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
                                 0, 1, 1, 0, 0, 0)
      END IF
   END SUBROUTINE context_create

   LOGICAL FUNCTION context_valid_id(idt)
      IMPLICIT NONE
      CHARACTER(len  = *)    , INTENT(IN) :: idt
      LOGICAL  (kind = 1)                 :: val
      CALL cxios_context_valid_id(val, idt, len(idt));
      context_valid_id = val
   END FUNCTION  context_valid_id


   
END MODULE ICONTEXT
