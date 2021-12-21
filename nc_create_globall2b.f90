! Name: nc_create_globall2b.f90
!
!
! Purpose: File contains subroutines to open and define netcdf file.
! 
! 
!
! Description and Algorithm details:
!
!
! Arguments:
! Name Type In/Out/Both Description
!
!
! Local variables:
! Name Type Description
!
!
! History:
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
!2013/06/11 MJ implemented code to enable writing of netcdf-4 output files.
!2014/11/19 Oliver Sus: fix in Julian days time global attribute
!2015/02/27 Oliver Sus: removal of l1closure; replaced deprecated NF90_HDF with NF90_NETCDF4
!2015/02/23 Oliver Sus: added time coverage informatio
!2015/10/26 CP: changed 4712 to 4713
!2015/12/16 OS: updated output format of lat/lon fields, avoiding inaccuracies
!2016/02/18 OS: removed cgridx/y as SR arguments, change in time reference date and attributes
!2016/02/26 OS: Added cdm_data_type, naming_authority, and keywords_vocabulary; some NetCDF variable unit/comment/name changes
!2016/03/04 OS: some minor editing of attributes
!
! $Id$
!
! Bugs:
!
!none known


SUBROUTINE nc_create_globall2b( path, nx, ny, dx, dy,slon,elon,slat,elat, &
     & ncid, dims_var, wo, &
     & cncver,ccon,cinst,l2cproc,l2cprocver,l3cproc,l3cprocver,contact,website, &
     & cprodtype, platform, csensor, cuuid, prodtime,prod_name, &
     & year, month, day, dur, grid_type, standard_name_voc,&
     & naming_authority,cdm_data_type, keywords_vocabulary,&
     & reference, history, summary, keywords, comment, project, license,cfile_version,csource )

  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !   
  ! Modifications Log:    
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------
  
  USE netcdf

  use vartypes

  IMPLICIT NONE
  
  !  INCLUDE 'netcdf.inc'
  
  ! Input
  INTEGER,INTENT(IN) :: wo!, ryr
  INTEGER(kind=lint),INTENT(IN) :: nx, ny
  INTEGER(kind=stint),INTENT(IN) ::  dx, dy
  integer (kind=stint) :: slon,elon,slat,elat
!ORG  INTEGER,INTENT(IN) :: time, nx, ny, grid, dx, dy,wo!, ryr
!   integer :: yday
  CHARACTER(LEN=*),INTENT(IN) :: path

  ! Output
  INTEGER,INTENT(OUT) :: ncid, dims_var(3)
  
  ! Local
  INTEGER :: ierr, xdim, ydim, tdim!, var1_dim, var2_dim

  INTEGER :: xid, yid, tid!, cotid,ctpid,phaseid
!   INTEGER :: yrlen(38)!, diff(2)
!  INTEGER :: i

!  INTEGER, PARAMETER :: SINGLE = 4
!  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny), time , yy, mm, dd
   integer, parameter :: R64 = selected_real_kind (15,307)
   integer, parameter :: R32 = selected_real_kind (6,37)
   integer(kind=lint) ::  i
   real(kind=sreal) :: time , yy, mm, dd
   real(kind=R64)   :: lon(nx), lat(ny), val1, val2 ! used for lon/lat calculation

!   CHARACTER(LEN=100) :: tunits
!   CHARACTER(LEN=  4) :: chryr

  integer :: clength, cposition

  integer, dimension(1) :: start,counter,stride

  CHARACTER(len= attribute_length) :: cncver,ccon,cinst, &
       & contact, website, prodtime, ctitle,  &
       & l2cproc, l3cproc,l2cprocver, l3cprocver, prod_name, year, month, day,dur, &
       & grid_type, project,cfile_version, standard_name_voc
! cproc, cprocver,instname,
  character(len=uuid_length) :: cuuid

  character(len=paramlength) :: cprodtype, platform, csensor

  character(len=inlength) :: cgridx, cgridy

  character(len=l3_outputpath_and_file_length) :: fname

  character(len=description_length) :: reference,history,summary,keywords,comment,license,csource, &
  & naming_authority, cdm_data_type, keywords_vocabulary

  real (kind=sreal) :: rgridx,rgridy
  integer (kind=stint) :: gridx,gridy

  ! End of header ----------------------------------------------------------
  
  
  !make some transformations and adjugstions
  rgridx=1.0/dx
  rgridy=1.0/dy

  write(*,*) rgridx,rgridy
  write(cgridx,'(f4.2)') rgridx
  write(cgridy,'(f4.2)') rgridy
  write(*,*) cgridx,cgridy

!   yday=1

  !ierr = NF90_CREATE(trim(adjustl(path)), NF90_CLOBBER, ncid)
  !this now creates a classical netcdf file but with the capabilities to
  !turn on chunking and compression
  !NF90_HDF5 is deprecated, use NF90_NETCDF4 instead.
!  ierr = NF90_CREATE(path, IOR(NF90_NCDF4,NF90_CLASSIC_MODEL), ncid)
  ierr = NF90_CREATE(path, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error creating file'

  ! Define the 3 dimensions: time / lat / lon

   ierr = NF90_DEF_DIM(ncid, 'lat', ny, ydim)
  IF (ierr.NE.NF90_NOERR) STOP 'create y-d'

  ierr = NF90_DEF_DIM(ncid, 'lon', nx, xdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create x-d'

  ierr = NF90_DEF_DIM(ncid, 'time', 1, tdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create t-d'


  !      ierr = NF_SET_FILL(ncid, NF_FILL,NF_NOFILL)
  !      IF (ierr.NE.NF_NOERR) stop 'error set var FillValue'
  
  !

  !
!  ierr = NF90_DEF_VAR (ncid, 'lat', NF90_FLOAT, ydim, yid) 
  ierr = NF90_DEF_VAR (ncid, 'lat', NF90_FLOAT, ydim, yid, &
  & deflate_level=compress_level_float,shuffle=shuffle_float)
  IF (ierr.NE.NF90_NOERR) STOP 'def lat'
  ierr = NF90_PUT_ATT(ncid, yid, 'long_name', 'latitude')
  IF (ierr.NE.NF90_NOERR) stop 'error def lat long_name'
  ierr = NF90_PUT_ATT(ncid, yid, 'units', 'degrees_north')
  IF (ierr.NE.NF90_NOERR) stop 'error def lat units'
!  ierr = NF90_PUT_ATT(ncid, yid, 'scale_factor', 0.001 )
!  IF (ierr.NE.NF90_NOERR) stop 'error def lat scale_factor'
!  ierr = NF90_PUT_ATT(ncid, yid, 'add_offset', 0.0 )
!  IF (ierr.NE.NF90_NOERR) stop 'error def lat offset'
  ierr = NF90_PUT_ATT(ncid, yid, 'valid_min', -90.0 )
  IF (ierr.NE.NF90_NOERR) stop 'error def lat vmin'
  ierr = NF90_PUT_ATT(ncid, yid, 'valid_max',  90.0)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def lat vmax'
  ierr = NF90_PUT_ATT(ncid, yid, 'standard_name', 'latitude')
  IF (ierr.NE.NF90_NOERR) stop 'error def lat standard_name'
  !
!  ierr = NF90_DEF_VAR (ncid, 'lon', NF90_FLOAT, xdim, xid) 
  ierr = NF90_DEF_VAR (ncid, 'lon', NF90_FLOAT, xdim, xid, &
  & deflate_level=compress_level_float,shuffle=shuffle_float)
  IF (ierr.NE.NF90_NOERR) STOP 'def lon'
  ierr = NF90_PUT_ATT(ncid, xid, 'long_name', 'longitude')
  IF (ierr.NE.NF90_NOERR) stop 'error def lon long_name'
  ierr = NF90_PUT_ATT(ncid, xid, 'units', 'degrees_east')
  IF (ierr.NE.NF90_NOERR) stop 'error def lon units'
!  ierr = NF90_PUT_ATT(ncid, xid, 'scale_factor', 0.001 )
!  IF (ierr.NE.NF90_NOERR) stop 'error def lon scale_factor'
!  ierr = NF90_PUT_ATT(ncid, xid, 'add_offset', 0.0 )
!  IF (ierr.NE.NF90_NOERR) stop 'error def lon offset'
  ierr = NF90_PUT_ATT(ncid, xid, 'valid_min', -180.0 )
  IF (ierr.NE.NF90_NOERR) stop 'error def lon vmin'
  ierr = NF90_PUT_ATT(ncid, xid, 'valid_max',  180.0)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def lon vmax'
  ierr = NF90_PUT_ATT(ncid, xid, 'standard_name','longitude')
  IF (ierr.NE.NF90_NOERR) stop 'error def lat standard_name'

  !ORG      ierr = NF90_DEF_VAR (ncid, 'time', NF_INT, 1, tdim, tid)
!   ierr = NF90_DEF_VAR (ncid, 'time', NF90_INT2, tdim, tid)
  ierr = NF90_DEF_VAR (ncid, 'time', NF90_FLOAT, tdim, tid)
  IF (ierr.NE.NF90_NOERR) STOP 'def time'
  !ORG      ierr = NF90_PUT_ATT(ncid, tid, 'long_name', 4, 'time')
!  ierr = NF90_PUT_ATT(ncid, tid, 'long_name', 'time in julian days')
  ierr = NF90_PUT_ATT(ncid, tid, 'long_name', '00:00 timestamp of the sampled day')
  IF (ierr.NE.NF90_NOERR) stop 'error def time long_name'
  ierr = NF90_PUT_ATT(ncid, tid, 'calendar', 'standard')
  IF (ierr.NE.NF90_NOERR) stop 'error def time calendar'
!   tunits='days since '//chryr//'-01-01 00:00:00' 
!   ierr = NF90_PUT_ATT(ncid, tid, 'units',TRIM(tunits))
!   IF (ierr.NE.NF90_NOERR)  stop 'error def time units'
!  ierr = NF90_PUT_ATT(ncid, tid, 'units','days since -4712-01-01 12:00:00')
  ierr = NF90_PUT_ATT(ncid, tid, 'units','days since 1970-01-01 00:00:00')
  IF (ierr.NE.NF90_NOERR)  stop 'error def time units'
  ierr = NF90_PUT_ATT(ncid, tid, 'standard_name','time')
  IF (ierr.NE.NF90_NOERR) stop 'error def time standard_name'


!hist_cot1d(hist_cot1d) ;
!        float hist_cot_bin

  ctitle='ESA Cloud CCI Retrieval Products L3U Output File'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'title',trim(adjustl(ctitle)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'project',trim(adjustl( project)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'product_version',trim(adjustl(cfile_version)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Conventions',trim(adjustl(ccon)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'standard_name_vocabulary',trim(adjustl(standard_name_voc)))
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'standard_name_vocabulary',&
!        & 'NetCDF Climate Forecast (CF) Metadata Convention version 1.5')
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'institution',trim(adjustl(cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

!sstapelb ; not compliant with data standard-------------------------------
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(cncver)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor',trim(adjustl(l2cproc)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L2_Processor_Version',trim(adjustl(l2cprocver)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L3_Processor',trim(adjustl(l3cproc)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'L3_Processor_Version',trim(adjustl(l3cprocver)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Level',trim(adjustl(cprodtype)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Name',trim(adjustl(prod_name)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_resolution', trim(adjustl(cgridx)))
!        & trim(adjustl(cgridx))//' degree')
  IF (ierr.NE.NF90_NOERR) stop 'error def geospatial_lon_resolution'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_resolution', trim(adjustl(cgridy)))
!        & trim(adjustl(cgridy))//' degree')
  IF (ierr.NE.NF90_NOERR) stop 'error def geospatial_lat_resolution'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Grid_Type',trim(adjustl(grid_type)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
! !!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Grid_Resolution','1/'//trim(adjustl(cgridx))//'x'//'1/'//trim(adjustl(cgridy)))
! !!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Closure_Flag', trim(adjustl(l1closure)))
!   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'


  !put in here the original data source
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'source',trim(adjustl(csource)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_min', float(slat) + (1./ (2.0 * dy)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_max', float(elat) - (1./ (2.0 * dy)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_min', float(slon) + (1./(2.0 * dx)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_max',float(elon) - (1./ (2.0 * dx)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

! sstapelb added attribute 21.11.2012
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_units', 'degrees_north')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_units', 'degrees_east')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_vertical_min', '0.0')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_vertical_max', '0.0')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'spatial_resolution', trim(adjustl(cgridx))//' degree')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
! sstapelb 15.11.2012
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_start',trim(adjustl(trim(adjustl(year))// &
  & trim(adjustl(month))//trim(adjustl(day))//'T000000Z')))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_end',trim(adjustl(trim(adjustl(year))// & 
  & trim(adjustl(month))//trim(adjustl(day))//'T235959Z')))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_duration',trim(adjustl(dur)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_resolution',trim(adjustl(dur)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
! ----------------


  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'platform',trim(adjustl(platform)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'sensor',trim(adjustl(csensor)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'tracking_id',trim(adjustl(cuuid)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  cposition=index(trim(adjustl(path)),'/',back=.true.)
  clength=len_trim(adjustl(path))
!  fname=trim(adjustl(path(cposition+1:clength)))
  fname=trim(adjustl(path))
!  write(*,*) cposition,clength
!  write(*,*) trim(adjustl(path))
!  write(*,*) trim(adjustl(fname))
!  write(*,*) F90MAXNCNAM
!  pause
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'id',trim(adjustl(fname(cposition+1:clength))))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'creator_email',trim(adjustl(contact)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'creator_url',trim(adjustl(website)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'date_created',trim(adjustl(prodtime)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'creator_name',trim(adjustl(cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

!!$  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
!!$       & trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day)))))
!!$  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'


  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'references',trim(adjustl(reference)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   history=trim(adjustl(prodtime))//' Product Generated from '//trim(adjustl(l2cproc))
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'history',trim(adjustl(history)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   summary="This dataset contains daily Level-3 global cloud property products &
!           & from satellite observations. Sampled onto a regular grid."
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'summary',trim(adjustl(summary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   keywords='satellite, observations, cloud properties'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'keywords',trim(adjustl(keywords)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   comment='These data were produced at ESACCI as part of the ESA CLOUD CCI project.'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'comment',trim(adjustl(comment)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!   license='data is free and open'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'license',trim(adjustl(license)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL,'cdm_data_type',trim(adjustl(cdm_data_type)))
  IF (ierr.NE.NF90_NOERR) stop 'error def cdm_data_type'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL,'keywords_vocabulary',trim(adjustl(keywords_vocabulary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def keywords vocabulary'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL,'naming_authority',trim(adjustl(naming_authority)))
  IF (ierr.NE.NF90_NOERR) stop 'error def naming convention'

  ierr = NF90_ENDDEF(ncid, h_minfree=100000)
  IF (ierr.NE.NF90_NOERR)  stop 'error enddef '
  !
  !MO RG lat(1) = -lat_offset + (1./ (2 * dy))
!  lat(1) = slat + (1./ (2 * dy))
!  DO i=2,ny
!     lat(i) = lat(i-1)+1.0/float(dy)
!     !MJ ORGlat(i) = (FLOAT(i-1))/dy - (lat_offset- 1./dy + (1./ (2 * dy)))             !create grid resolution lat
!  ENDDO

!  val1  = float(slat) + ( 1.0/(2.0*float(dy)) )
!  val2  = float(elat) - ( 1.0/(2.0*float(dy)) )
!  DO i=1,ny
!     lat(i) = nint( ( (i-1) * (val2 - val1) / (float(ny)-1.0) + val1 ) * 1000 )
!  ENDDO

  lat(1) = ( float(slat) + ( 1.0/(2.0*float(dy)) ) ) *1000.
  val1   = 1000./float(dy)
  DO i=2,ny
     lat(i) = lat(i-1) + val1
  ENDDO
  lat = lat(:) * 0.001_R64

  start(1)=1
  stride(1)=1
  counter(1)=ny
  ierr = NF90_PUT_VAR(ncid, yid,lat,start,counter,stride)!1,ny,1)   !write the lat numbers
  IF (ierr.NE.NF90_NOERR) STOP 'write lat'
  !
!  lon(1) = slon + (1./(2 * dx))
!  DO i=2,nx
!     lon(i) = lon(i-1) + 1.0/float(dx)              !create grid resolution lon
!  ENDDO

!  val1  = float(slon) + ( 1.0/(2.0*float(dx)) )
!  val2  = float(elon) - ( 1.0/(2.0*float(dx)) )
!  DO i=1,nx
!     lon(i) = nint( ( (i-1) * (val2 - val1) / (float(nx)-1.0) + val1 ) * 1000 )
!  ENDDO

  lon(1) = ( float(slon) + ( 1.0/(2.0*float(dx)) ) ) *1000.
  val1   = 1000./float(dx)
  DO i=2,nx
     lon(i) = lon(i-1) + val1
  ENDDO
  lon = lon(:) * 0.001_R64


  !MJ ORG
!!$  lon(1) = -float(lon_offset) + (1./(2 * dx))
!!$  DO i=2,nx
!!$     lon(i) = (FLOAT(i-1))/dx - ( float(lon_offset)- 1./dx + (1./ (2 * dx)))              !create grid resolution lon
!!$  ENDDO

  start(1)=1
  stride(1)=1
  counter(1)=nx
  ierr = NF90_PUT_VAR(ncid, xid, lon, start, counter, stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write lon'
  !
  IF (wo.EQ.1) THEN
     write(*,*) lat(1:10)
     write(*,*) lon(1:10)
  ENDIF

  read(year,*) yy
  read(month,*) mm
  read(day,*) dd

  time = 367.0 * yy - aint((7.0*(yy+aint((mm + 9.)/12.)))/4.) + aint((275.*mm)/9.) + dd + 1721013.5
  time = time - jul_start ! lets start from 1.1.1970 00:00:00

  ierr = NF90_PUT_VAR(ncid, tid, time)
  IF (ierr.NE.NF90_NOERR) STOP 'write time'

  dims_var(1) = xdim !1
  dims_var(2) = ydim !3
  dims_var(3) = tdim

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF

  RETURN
  
END SUBROUTINE nc_create_globall2b
