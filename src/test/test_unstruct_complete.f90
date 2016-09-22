PROGRAM test_unstruct_complete

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: mpi_rank
  INTEGER :: mpi_size
  INTEGER :: ierr

  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_duration) :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER, PARAMETER :: nlon=60
  INTEGER, PARAMETER :: nlat=30
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100
  INTEGER,PARAMETER :: llm=1
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  TYPE(xios_domain) :: domain_hdl
  LOGICAL :: ok

  DOUBLE PRECISION,ALLOCATABLE :: lon_glo(:),lat_glo(:)
  DOUBLE PRECISION,ALLOCATABLE :: bounds_lon_glo(:,:),bounds_lat_glo(:,:)
  DOUBLE PRECISION,ALLOCATABLE :: field_A_glo(:,:)
  INTEGER,ALLOCATABLE :: i_index_glo(:)
  INTEGER,ALLOCATABLE :: i_index(:)
  LOGICAL,ALLOCATABLE :: mask_glo(:),mask(:)
  INTEGER,ALLOCATABLE :: n_local(:),local_neighbor(:,:)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:),lat(:),field_A_srf(:,:), lonvalue(:) ;
  DOUBLE PRECISION,ALLOCATABLE :: bounds_lon(:,:),bounds_lat(:,:) ;
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n, nbMax
  INTEGER :: ncell_glo,ncell,ind
  REAL :: ilon,ilat
  DOUBLE PRECISION, PARAMETER :: Pi=3.14159265359
  INTEGER :: list_ind(nlon,nlat)
  INTEGER :: rank,j1,j2,np,ncell_x
  INTEGER :: data_n_index
  INTEGER,ALLOCATABLE :: data_i_index(:)
  DOUBLE PRECISION,ALLOCATABLE :: field_A_compressed(:,:)

  CALL xios_initialize(id,return_comm=comm)
  CALL MPI_COMM_RANK(comm,mpi_rank,ierr)
  CALL MPI_COMM_SIZE(comm,mpi_size,ierr)

  CALL init_wait

  ncell_glo=0
  DO j=1,nlat
    n =  NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
    IF (n<8) n=8
    ncell_glo=ncell_glo+n
  ENDDO

  ALLOCATE(lon_glo(ncell_glo))
  ALLOCATE(lat_glo(ncell_glo))
  ALLOCATE(bounds_lon_glo(4,ncell_glo))
  ALLOCATE(bounds_lat_glo(4,ncell_glo))
  ALLOCATE(i_index_glo(ncell_glo))
  ALLOCATE(field_A_glo(ncell_glo,llm))
  ALLOCATE(mask_glo(ncell_glo))

  ind=0
  DO j=1,nlat
    n = NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
    if (j==1) PRINT*,"--- ",n
    if (j==nlat) PRINT*,"--- ",n
    IF (n<8) n=8

    DO i=1,n
      ind=ind+1
      list_ind(i,j)=ind
      ilon=i-0.5
      ilat=j-0.5

      lat_glo(ind)= 90-(ilat*180./nlat)
      lon_glo(ind)= (ilon*360./n)


      bounds_lat_glo(1,ind)= 90-((ilat-0.5)*180./nlat)
      bounds_lon_glo(1,ind)=((ilon-0.5)*360./n)

      bounds_lat_glo(2,ind)= 90-((ilat-0.5)*180./nlat)
      bounds_lon_glo(2,ind)=((ilon+0.5)*360./n)

      bounds_lat_glo(3,ind)= 90-((ilat+0.5)*180./nlat)
      bounds_lon_glo(3,ind)=((ilon+0.5)*360./n)

      bounds_lat_glo(4,ind)= 90-((ilat+0.5)*180./nlat)
      bounds_lon_glo(4,ind)=((ilon-0.5)*360./n)

    ENDDO
  ENDDO

!  mpi_size=32
  rank=(mpi_size-1)/2
  ncell_x=sqrt(ncell_glo*1./mpi_size)

  j1=nlat/2
  DO WHILE(rank>=0)
    j2=MAX(j1-ncell_x+1,1)
    j=(j1+j2)/2
    n=NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
    np = MIN(n/ncell_x,rank+1) ;
    if (j2==1) np=rank+1

    PRINT *,"domain ",j2,j1,rank,np ;
    DO j=j2,j1
      n=NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
      IF (n<8) n=8
      DO i=1,n
        ind=list_ind(i,j)
        IF ( (i-1) < MOD(n,np)*(n/np+1)) THEN
          i_index_glo(ind) = rank - (i-1)/(n/np+1)
        ELSE
          i_index_glo(ind) = rank-(MOD(n,np)+ (i-1-MOD(n,np)*(n/np+1))/(n/np))
        ENDIF
      ENDDO
    ENDDO
    rank=rank-np
    j1=j2-1
  ENDDO

  rank=(mpi_size-1)/2+1
  ncell_x=sqrt(ncell_glo*1./mpi_size)

  j1=nlat/2+1
  DO WHILE(rank<=mpi_size-1)
    j2=MIN(j1+ncell_x-1,nlat)
    j=(j1+j2)/2
    n=NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
    np = MIN(n/ncell_x,mpi_size-rank) ;
    if (j2==nlat) np=mpi_size-rank

    PRINT *,"domain ",j2,j1,rank,np ;
    DO j=j1,j2
      n=NINT(COS(Pi/2-(j-0.5)*PI/nlat)*nlon)
      IF (n<8) n=8
      DO i=1,n
        ind=list_ind(i,j)
        IF ( (i-1) < MOD(n,np)*(n/np+1)) THEN
          i_index_glo(ind) = rank + (i-1)/(n/np+1)
        ELSE
          i_index_glo(ind) = rank+(MOD(n,np)+ (i-1-MOD(n,np)*(n/np+1))/(n/np))
        ENDIF
      ENDDO
    ENDDO
    rank=rank+np
    j1=j2+1
  ENDDO

  ncell=0
  DO ind=1,ncell_glo
    IF (i_index_glo(ind)==mpi_rank) ncell=ncell+1
  ENDDO
  ALLOCATE(i_index(ncell))
  ALLOCATE(lon(ncell))
  ALLOCATE(lat(ncell))
  ALLOCATE(bounds_lon(4,ncell))
  ALLOCATE(bounds_lat(4,ncell))
  ALLOCATE(field_A_srf(ncell,llm))
  ALLOCATE(mask(ncell))
  ALLOCATE(n_local(ncell))
  ncell=0
  data_n_index=0
  DO ind=1,ncell_glo
    IF (i_index_glo(ind)==mpi_rank) THEN
      ncell=ncell+1
      i_index(ncell)=ind-1
      lon(ncell)=lon_glo(ind)
      lat(ncell)=lat_glo(ind)
      bounds_lon(:,ncell)=bounds_lon_glo(:,ind)
      bounds_lat(:,ncell)=bounds_lat_glo(:,ind)
      field_A_srf(ncell,:)=i_index_glo(ind)
      IF (MOD(ind,8)>=0 .AND. MOD(ind,8)<2) THEN
        mask(ncell)=.FALSE.
!        data_n_index=data_n_index+1
      ELSE
        mask(ncell)=.TRUE.
        data_n_index=data_n_index+1
      ENDIF
    ENDIF
  ENDDO

  ALLOCATE(field_A_compressed(data_n_index,llm))
  ALLOCATE(data_i_index(data_n_index))
  data_n_index=0
  DO ind=1,ncell
    IF (mask(ind)) THEN
      data_n_index=data_n_index+1
      data_i_index(data_n_index)=ind-1
      field_A_compressed(data_n_index,:)=field_A_srf(ind,:)
    ENDIF
  ENDDO

  CALL xios_context_initialize("surface",comm)
  CALL xios_get_handle("surface",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)

  CALL xios_set_axis_attr("axis_srf",n_glo=llm ,value=lval) ;
  CALL xios_set_domain_attr("domain_srf", ni_glo=ncell_glo, ni=ncell, ibegin=1, i_index=i_index)
  CALL xios_set_domain_attr("domain_srf", type='unstructured', data_dim=1, data_ni=data_n_index, &
                                          data_i_index=data_i_index)
  CALL xios_set_domain_attr("domain_srf", lonvalue_1D=lon, latvalue_1D=lat)
  CALL xios_set_domain_attr("domain_srf", nvertex=4, bounds_lon_1D=bounds_lon, bounds_lat_1D=bounds_lat)
!  CALL xios_set_compute_connectivity_domain_attr("compute", n_neighbor=n_local, local_neighbor=local_neighbor)



  dtime%second=3600
  CALL xios_set_timestep(dtime)
  CALL xios_close_context_definition()

  CALL xios_field_get_domain("field_A_expand",domain_hdl)
  CALL xios_get_attr(domain_hdl,ni=ni)
  PRINT *, "Domain destination ni = ",ni, "Domain source n= ",ncell
  CALL xios_get_compute_connectivity_domain_attr("compute", n_neighbor_max=nbMax)
  ALLOCATE(local_neighbor(nbMax,ncell))
  CALL xios_get_compute_connectivity_domain_attr("compute", n_neighbor=n_local, local_neighbor=local_neighbor)
  PRINT *, "Connectivity max = ",nbMax

   DO ts=1,1
     CALL xios_update_calendar(ts)
     CALL xios_send_field("field_A_srf",field_A_compressed)
    ENDDO

    CALL xios_context_finalize()

  DEALLOCATE(lon_glo)
  DEALLOCATE(lat_glo)
  DEALLOCATE(bounds_lon_glo)
  DEALLOCATE(bounds_lat_glo)
  DEALLOCATE(i_index_glo)
  DEALLOCATE(field_A_glo)
  DEALLOCATE(mask_glo)
  DEALLOCATE(i_index)
  DEALLOCATE(lon)
  DEALLOCATE(lat)
  DEALLOCATE(bounds_lon)
  DEALLOCATE(bounds_lat)
  DEALLOCATE(field_A_srf)
  DEALLOCATE(mask)
  DEALLOCATE(field_A_compressed)
  DEALLOCATE(data_i_index)

  CALL MPI_COMM_FREE(comm, ierr)

  CALL xios_finalize()

  END PROGRAM test_unstruct_complete





