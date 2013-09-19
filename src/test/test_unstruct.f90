PROGRAM test_client

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size
  INTEGER :: ierr
  
  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_time)      :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100 
  INTEGER,PARAMETER :: llm=5 
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  LOGICAL :: ok
  
  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION,DIMENSION(4,ni_glo,nj_glo) :: bounds_lon_glo,bounds_lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:),field_A(:,:,:), lonvalue(:) ;
  DOUBLE PRECISION,ALLOCATABLE :: bounds_lon(:,:,:),bounds_lat(:,:,:) ;
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n

  CALL xios_initialize(id,return_comm=comm)
! CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(comm,rank,ierr)
  CALL MPI_COMM_SIZE(comm,size,ierr)
  
  CALL init_wait
  
  
  DO j=1,nj_glo
    DO i=1,ni_glo
      lon_glo(i,j)=(i-1)+(j-1)*ni_glo
      lat_glo(i,j)=1000+(i-1)+(j-1)*ni_glo
      bounds_lat_glo(1,i,j)=lat_glo(i,j)-0.5
      bounds_lat_glo(2,i,j)=lat_glo(i,j)-0.5
      bounds_lat_glo(3,i,j)=lat_glo(i,j)+0.5
      bounds_lat_glo(4,i,j)=lat_glo(i,j)+0.5
      bounds_lon_glo(1,i,j)=lon_glo(i,j)-0.5
      bounds_lon_glo(2,i,j)=lon_glo(i,j)-0.5
      bounds_lon_glo(3,i,j)=lon_glo(i,j)+0.5
      bounds_lon_glo(4,i,j)=lon_glo(i,j)+0.5
      DO l=1,llm
        field_A_glo(i,j,l)=(i-1)+(j-1)*ni_glo+10000*l
      ENDDO
    ENDDO
  ENDDO
  ni=ni_glo ; ibegin=1

  jbegin=1
  DO n=0,size-1
    nj=nj_glo/size
    IF (n<MOD(nj_glo,size)) nj=nj+1
    IF (n==rank) exit 
    jbegin=jbegin+nj
  ENDDO
  
  iend=ibegin+ni-1 ; jend=jbegin+nj-1

  ALLOCATE(lon(ni,nj),lat(ni,nj),field_A(ni,nj,llm),lonvalue(ni*nj))
  ALLOCATE(bounds_lon(4,ni,nj),bounds_lat(4,ni,nj))
  lon(:,:)=lon_glo(ibegin:iend,jbegin:jend)
  lat(:,:)=lat_glo(ibegin:iend,jbegin:jend)
  bounds_lon(:,:,:)=bounds_lon_glo(:,ibegin:iend,jbegin:jend)
  bounds_lat(:,:,:)=bounds_lat_glo(:,ibegin:iend,jbegin:jend)
  field_A(1:ni,1:nj,:)=field_A_glo(ibegin:iend,jbegin:jend,:)
 

  CALL xios_context_initialize("test",comm)
  CALL xios_get_handle("test",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)
  
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
!  CALL xios_set_context_attr("test",start_date="01/01/2000 - 00:00:00")
  CALL xios_set_context_attr("test",calendar_type="Gregorian") 
  CALL xios_set_axis_attr("axis_A",size=llm ,value=lval) ;
!  CALL xios_set_domain_attr("domain_A",ni_glo=1, nj_glo=nj_glo*nj_glo, ibegin=1, ni=1,jbegin=(jbegin-1)*ni_glo+1,nj=nj*ni_glo)
  CALL xios_set_domain_attr("domain_A",ni_glo=ni_glo*nj_glo, ibegin=(jbegin-1)*ni_glo+1, ni=nj*ni_glo)
  !CALL xios_set_domain_attr("domain_A",zoom_ni=3,zoom_ibegin=3,zoom_nj=3,zoom_jbegin=6)
  CALL xios_set_domain_attr("domain_A",data_dim=1,type='unstructured')
  CALL xios_set_domain_attr("domain_A",lonvalue=RESHAPE(lon,(/ni*nj/)),latvalue=RESHAPE(lat,(/ni*nj/)))
  CALL xios_set_domain_attr("domain_A", bounds_lon=RESHAPE(bounds_lon,(/4,ni*nj/)) )
  CALL xios_set_domain_attr("domain_A",bounds_lat=RESHAPE(bounds_lat,(/4,ni*nj/)) )
  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)
  
  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B")
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_B")
  
  CALL xios_get_handle("output",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl,"field_C")
  CALL xios_set_attr(field_hdl,field_ref="field_A",name="field_C")
    
 
    dtime%second=3600
    CALL xios_set_timestep(dtime) 
    
!    ni=0 ; lonvalue(:)=0
!    CALL xios_get_domain_attr("domain_A",ni=ni,lonvalue=lonvalue)
    
!    print *,"ni",ni
!    print *,"lonvalue",lonvalue ;

    CALL xios_is_defined_field_attr("field_A",enabled=ok)
    PRINT *,"field_A : attribute enabled is defined ? ",ok
    IF (ok) THEN
      CALL xios_get_field_attr("field_A",enabled=ok)
      PRINT *,"field_A : attribute enabled = ",ok
    ENDIF
    CALL xios_is_defined_field_attr("field_C",enabled=ok)
    PRINT *,"field_C : attribute enabled is defined ? ",ok
    IF (ok) THEN
      CALL xios_get_field_attr("field_C",enabled=ok)
      PRINT *,"field_C : attribute enabled = ",ok
    ENDIF
      
    CALL xios_solve_inheritance()
    CALL xios_is_defined_field_attr("field_C",enabled=ok)
    PRINT *,"field_C : attribute enabled is defined ? ",ok
    IF (ok) THEN
      CALL xios_get_field_attr("field_C",enabled=ok)
      PRINT *,"field_C : attribute enabled = ",ok
    ENDIF
    CALL xios_close_context_definition()
    
    PRINT*,"field field_A is active ? ",xios_field_is_active("field_A")
    DO ts=1,24*10
      field_A=field_A+100000
      CALL xios_update_calendar(ts)
      CALL xios_send_field("field_A",RESHAPE(field_A,(/ni*nj,llm/)))
      if (ts==12) CALL xios_send_field("field_once",RESHAPE(field_A,(/ni*nj,llm/)))
!      field_A=field_A+100000
!     CALL wait_us(5000) ;
    ENDDO
  
    CALL xios_context_finalize()
    CALL xios_finalize()
  
!  CALL MPI_FINALIZE(ierr)
  
END PROGRAM test_client


  

  
