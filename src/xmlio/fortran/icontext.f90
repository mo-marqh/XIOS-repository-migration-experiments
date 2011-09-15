MODULE ICONTEXT
   USE, INTRINSIC :: ISO_C_BINDING
   USE CONTEXT_INTERFACE
   USE IDATE
   
   TYPE XContextHandle
      INTEGER(kind = C_INTPTR_T) :: daddr
   END TYPE XContextHandle
      
   !----------------------------------------------------------------------------
   INTERFACE set_context_attributes
      MODULE PROCEDURE set_context_attributes_id,set_context_attributes_hdl
   END INTERFACE  
   !----------------------------------------------------------------------------
   
   CONTAINS ! Fonctions disponibles pour les utilisateurs.
   
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
         CALL xios_set_context_calendar_type(context_hdl%daddr, calendar_type_, len(calendar_type_))
      END IF
      IF (PRESENT(start_date_))    THEN
         CALL xios_set_context_start_date(context_hdl%daddr, start_date_, len(start_date_))
      END IF
      IF (PRESENT(output_dir_))    THEN
         CALL xios_set_context_output_dir(context_hdl%daddr, output_dir_, len(output_dir_))
      END IF
   END SUBROUTINE set_context_attributes_hdl

   SUBROUTINE context_handle_create(ret, idt)
      IMPLICIT NONE
      TYPE(XContextHandle), INTENT(OUT):: ret
      CHARACTER(len = *)  , INTENT(IN) :: idt      
      CALL xios_context_handle_create(ret%daddr, idt, len(idt))            
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
      CALL xios_context_set_current(context%daddr, wswap)
   END SUBROUTINE context_set_current
   
   SUBROUTINE context_create(context_hdl, context_id, calendar_type, init_date)
      TYPE(XContextHandle)          , INTENT(OUT) :: context_hdl
      CHARACTER(len = *)            , INTENT(IN)  :: context_id
      INTEGER                       , INTENT(IN)  :: calendar_type
      TYPE(XDate)         , OPTIONAL, INTENT(IN)  :: init_date
      IF (PRESENT(init_date)) THEN
         CALL xios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
                                  init_date%year, init_date%month, init_date%day, &
                                  init_date%hour, init_date%minute, init_date%second)
      ELSE
         CALL xios_context_create(context_hdl%daddr, context_id, len(context_id), calendar_type, &
                                 0, 1, 1, 0, 0, 0)
      END IF
   END SUBROUTINE context_create
   
END MODULE ICONTEXT
