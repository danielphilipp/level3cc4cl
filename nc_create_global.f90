! Name: nc_create_global.f90
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
!2014/03/28 Cintia Carbajal Henken added if statement for FAME-C
!2014/11/19 Oliver Sus made minor editing + replaces variable daystart with
!  01, as monthly average always starts with this day; previous version used empty
!  string for daystart, causing a crush; should be properly fixed in future commit
!  + fix in Julian days time global attribute
!2015/02/27 SST added various new NetCDF histogram variables
! 2015/03/11 CP adds cloud albedo
!2015/02/23 OS added time coverage information
!2015/07/10 OS added number of processed orbits as NetCDF attribute
!2015/10/22 CP changed date form -4712 to -4713
!2015/10/22 CP big CF compliance update and comments added stemp added
!2015/12/16 OS update in time variable definition; updated output format of lat/lon fields, avoiding inaccuracies
!              renaming of histogram variable names 
!2016/02/18 OS changed histogram variable name REF to CER, change in time reference date and attributes
!2016/02/26 OS some NetCDF variable unit/comment/name changes
!2016/03/04 OS some minor editing of attributes
!2016/03/18 OS changed some units of NetCDF variables

! $Id$
!
! Bugs:
!
!none known

SUBROUTINE nc_create_global( path, nx, ny, dx, dy,slon,elon,slat,elat, &
     & ncid, dims_var, wo,hist_cot,hist_ctp,hist_cot_bin,hist_ctp_bin, dims_var_hist2d, dims_var_phase, &
     & hist_cot_1d_axis, hist_cot_1d_bin_axis,dims_var_histcot1d, &
     & hist_ctp_1d_axis, hist_ctp_1d_bin_axis,dims_var_histctp1d, &
     & hist_ctt_1d_axis, hist_ctt_1d_bin_axis,dims_var_histctt1d, &
     & hist_ref_1d_axis, hist_ref_1d_bin_axis,dims_var_histref1d, &
     & hist_cwp_1d_axis, hist_cwp_1d_bin_axis,dims_var_histcwp1d, &
     & hist_cloud_albedo1_1d_axis, hist_cloud_albedo1_1d_bin_axis,dims_var_histcloud_albedo11d, &
     & hist_cloud_albedo2_1d_axis, hist_cloud_albedo2_1d_bin_axis,dims_var_histcloud_albedo21d, &
     & cncver,ccon,cinst,l2cproc,l2cprocver,l3cproc,l3cprocver,contact,website, &
     & cprodtype, platform, csensor, cuuid, prodtime,prod_name, &
     & year, month, day, dom, dur, cgridx, cgridy, grid_type,&
     & reference, history, summary, keywords, comment, project,license,cfile_version,csource, standard_name_voc, &
     & naming_authority, cdm_data_type, keywords_vocabulary, daystart, calgo, num_of_orbits)

  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
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
  INTEGER,INTENT(OUT) :: ncid, dims_var(3),dims_var_hist2d(6), dims_var_phase(3), &
       & dims_var_histcot1d(5),dims_var_histctp1d(5),dims_var_histctt1d(5),dims_var_histref1d(5),dims_var_histcwp1d(5),dims_var_histcloud_albedo11d(5),dims_var_histcloud_albedo21d(5)

  ! Local
  INTEGER :: ierr, xdim, ydim, tdim, hist_ctpdim,hist_cotdim, hist_phasedim,hist_cloud_albedo1dim,hist_cloud_albedo2dim!,var1_dim, var2_dim,
  integer :: hist_ctpdim_bin,hist_cotdim_bin
  integer :: hist_cotdim_bin1d,hist_cotdim1d
  integer :: hist_cloud_albedo1dim_bin1d,hist_cloud_albedo1dim1d
  integer :: hist_cloud_albedo2dim_bin1d,hist_cloud_albedo2dim1d
  integer :: hist_cttdim_bin1d,hist_cttdim1d
  integer :: hist_ctpdim_bin1d,hist_ctpdim1d
  integer :: hist_refdim_bin1d,hist_refdim1d
  integer :: hist_cwpdim_bin1d,hist_cwpdim1d

  INTEGER :: xid, yid, tid, cotid,ctpid,phaseid,cotid_bin,ctpid_bin,cloud_albedo1id,cloud_albedo2id
  integer :: ctpid1d, ctpid_bin1d,cttid1d, cttid_bin1d,cotid1d, cotid_bin1d, cwpid1d, cwpid_bin1d, refid1d, refid_bin1d,stempid1d
  integer :: cloud_albedo1id1d, cloud_albedo1id_bin1d,cloud_albedo2id1d, cloud_albedo2id_bin1d
  !   INTEGER :: yrlen(38), diff(2)
!  INTEGER :: i

!  INTEGER, PARAMETER :: SINGLE = 4
!  INTEGER, PARAMETER :: DOUBLE = 8
!  REAL(KIND=SINGLE):: lon(nx), lat(ny), time , yy, mm, dd
  integer(kind=lint) :: i
  real(kind=sreal) :: time , yy, mm, dd
  integer, parameter :: R64 = selected_real_kind (15,307)
  integer, parameter :: R32 = selected_real_kind (6,37)
  real(kind=R64) :: lon(nx), lat(ny), val1!, val2 ! used for lon/lat calculation
  

  !   CHARACTER(LEN=100) :: tunits
  !   CHARACTER(LEN=  4) :: chryr

  real(kind=sreal) :: hist_cot(n_hist_cot), hist_ctp(n_hist_ctp)
  real(kind=sreal) :: hist_cot_bin(n_hist_cot-1), hist_ctp_bin(n_hist_ctp-1)

  integer(kind=stint) :: hist_phase(n_hist_phase)

  real(kind=sreal) :: hist_cot_1d_axis(n_cot_bins+1)
  real(kind=sreal) :: hist_cot_1d_bin_axis(n_cot_bins)

  real(kind=sreal) :: hist_cloud_albedo1_1d_axis(n_cloud_albedo1_bins+1)
  real(kind=sreal) :: hist_cloud_albedo1_1d_bin_axis(n_cloud_albedo1_bins)


  real(kind=sreal) :: hist_cloud_albedo2_1d_axis(n_cloud_albedo2_bins+1)
  real(kind=sreal) :: hist_cloud_albedo2_1d_bin_axis(n_cloud_albedo2_bins)

  real(kind=sreal) :: hist_ref_1d_axis(n_ref_bins+1)
  real(kind=sreal) :: hist_ref_1d_bin_axis(n_ref_bins)

  real(kind=sreal) :: hist_cwp_1d_axis(n_cwp_bins+1)
  real(kind=sreal) :: hist_cwp_1d_bin_axis(n_cwp_bins)

  real(kind=sreal) :: hist_ctp_1d_axis(n_ctp_bins+1)
  real(kind=sreal) :: hist_ctp_1d_bin_axis(n_ctp_bins)

  real(kind=sreal) :: hist_ctt_1d_axis(n_ctt_bins+1)
  real(kind=sreal) :: hist_ctt_1d_bin_axis(n_ctt_bins)

  integer :: cposition,clength
  integer(kind=lint) :: num_of_orbits

  integer, dimension(1) :: start,counter,stride

  CHARACTER(len= attribute_length) :: cncver,ccon,cinst, &
       & contact, website, prodtime, ctitle, & !cproc, cprocver, instname, &
       & l2cproc, l3cproc,l2cprocver, l3cprocver, prod_name,&
       & year, month, day, daystart, dom, dur, grid_type, project,cfile_version,standard_name_voc

  character(len=l3_outputpath_and_file_length) :: fname


  character(len=uuid_length) :: cuuid

  character(len=paramlength) :: cprodtype, platform, csensor, calgo

  character(len=inlength) :: cgridx, cgridy

  character(len=description_length) :: reference,history,summary,keywords,comment,license,csource, &
       & naming_authority, cdm_data_type, keywords_vocabulary

  real (kind=sreal) :: rgridx,rgridy
  integer (kind=stint) :: gridx,gridy

  ! End of header ----------------------------------------------------------

  ! Create new file
  if (trim(adjustl(calgo)) .eq. 'FAME-C') then 
     if(trim(adjustl(cprodtype)) .eq. 'l2b' ) then
        rgridx=0.1
        rgridy=0.1
        write(cgridx,'(f6.2)') rgridx
        write(cgridy,'(f6.2)') rgridy
     else
        rgridx=0.5
        rgridy=0.5
        write(cgridx,'(f6.2)') rgridx
        write(cgridy,'(f6.2)') rgridy
     endif
  else
     !make some transformations and adjustions
     !read(cgridx, '(I2)') gridx
     !read(cgridy, '(I2)') gridy
     rgridx=1.0/dx
     rgridy=1.0/dy

     write(cgridx,'(f5.2)') rgridx
     write(cgridy,'(f5.2)') rgridy
  endif

  if (wo .eq. 1) write(*,*) "create NetCDF file"

!   ierr = NF90_CREATE(path, NF90_CLOBBER, ncid)
  !NF90_HDF5 is deprecated, use NF90_NETCDF4 instead.
  !ierr = NF90_CREATE(path, IOR(NF90_HDF5,NF90_CLASSIC_MODEL), ncid)
  ierr = NF90_CREATE(path, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), ncid)
  IF (ierr.NE.NF90_NOERR)  stop 'error creating file'

  if (wo .eq. 1) write(*,*) "define NetCDF dimensions"

  ! Define the 3 dimensions: time / lat / lon
  ierr = NF90_DEF_DIM(ncid, 'lat', ny, ydim)
  IF (ierr.NE.NF90_NOERR) STOP 'create y-d'

  ierr = NF90_DEF_DIM(ncid, 'lon', nx, xdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create x-d'

  ! sstapelb added time atribute
  ierr = NF90_DEF_DIM(ncid, 'time', 1, tdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create t-d'

  ierr = NF90_DEF_DIM(ncid, 'hist2d_cot_bin_border', int(n_hist_cot,kind=lint), hist_cotdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create cot-d'

  ierr = NF90_DEF_DIM(ncid, 'hist2d_cot_bin_centre', int(n_hist_cot-1,kind=lint), hist_cotdim_bin)
  IF (ierr.NE.NF90_NOERR) STOP 'create cot-d_bin'



!  ierr = NF90_DEF_DIM(ncid, 'hist_cloud_albedo', int(n_hist_cloud_albedo,kind=lint), hist_cloud_albedodim)
!  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo-d'

!  ierr = NF90_DEF_DIM(ncid, 'hist_cloud_albedo_bin', int(n_hist_cloud_albedo-1,kind=lint), hist_cloud_albedodim_bin)
!  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo-d_bin'


  ierr = NF90_DEF_DIM(ncid, 'hist2d_ctp_bin_border', int(n_hist_ctp,kind=lint), hist_ctpdim)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctp-d'

  ierr = NF90_DEF_DIM(ncid, 'hist2d_ctp_bin_centre', int(n_hist_ctp-1,kind=lint), hist_ctpdim_bin)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctp-d_bin'

  ierr = NF90_DEF_DIM(ncid, 'hist_phase', int(n_hist_phase,kind=lint), hist_phasedim)
  IF (ierr.NE.NF90_NOERR) STOP 'create phase-d'

  ierr = NF90_DEF_DIM(ncid, 'hist1d_cot_bin_border', int(n_cot_bins+1,kind=lint), hist_cotdim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cot-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_cot_bin_centre', int(n_cot_bins,kind=lint), hist_cotdim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cot-d_bin1d'



  ierr = NF90_DEF_DIM(ncid, 'hist1d_cla_vis006_bin_border', int(n_cloud_albedo1_bins+1,kind=lint), hist_cloud_albedo1dim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo1-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_cla_vis006_bin_centre', int(n_cloud_albedo1_bins,kind=lint), hist_cloud_albedo1dim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo1-d_bin1d'


  ierr = NF90_DEF_DIM(ncid, 'hist1d_cla_vis008_bin_border', int(n_cloud_albedo2_bins+1,kind=lint), hist_cloud_albedo2dim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo2-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_cla_vis008_bin_centre', int(n_cloud_albedo2_bins,kind=lint), hist_cloud_albedo2dim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cloud_albedo2-d_bin1d'


  ierr = NF90_DEF_DIM(ncid, 'hist1d_cer_bin_border', int(n_ref_bins+1,kind=lint), hist_refdim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cer-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_cer_bin_centre', int(n_ref_bins,kind=lint), hist_refdim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cer-d_bin1d'

  ierr = NF90_DEF_DIM(ncid, 'hist1d_cwp_bin_border', int(n_cwp_bins+1,kind=lint), hist_cwpdim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cwp-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_cwp_bin_centre', int(n_cwp_bins,kind=lint), hist_cwpdim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create cwp-d_bin1d'

  ierr = NF90_DEF_DIM(ncid, 'hist1d_ctp_bin_border', int(n_ctp_bins+1,kind=lint), hist_ctpdim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctp-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_ctp_bin_centre', int(n_ctp_bins,kind=lint), hist_ctpdim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctp-d_bin1d'

  ierr = NF90_DEF_DIM(ncid, 'hist1d_ctt_bin_border', int(n_ctt_bins+1,kind=lint), hist_cttdim1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctt-d1d'
  ierr = NF90_DEF_DIM(ncid, 'hist1d_ctt_bin_centre', int(n_ctt_bins,kind=lint), hist_cttdim_bin1d)
  IF (ierr.NE.NF90_NOERR) STOP 'create ctt-d_bin1d'





  !      ierr = NF_SET_FILL(ncid, NF_FILL,NF_NOFILL)
  !      IF (ierr.NE.NF_NOERR) stop 'error set var FillValue'

  if (wo .eq. 1) write(*,*) "define NetCDF global variables"

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
!  ierr = NF90_PUT_ATT(ncid, yid, 'add_offset',  0.0)
!  IF (ierr.NE.NF90_NOERR) stop 'error def lat offset'
  ierr = NF90_PUT_ATT(ncid, yid, 'valid_min', -90.0 )
  IF (ierr.NE.NF90_NOERR) stop 'error def lat vmin'
  ierr = NF90_PUT_ATT(ncid, yid, 'valid_max',  90.0)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def lat vmax'
  ierr = NF90_PUT_ATT(ncid, yid, 'standard_name', 'latitude')
  IF (ierr.NE.NF90_NOERR) stop 'error def lat standard_name'

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

  !sstapelb adds time atribute
  !   ierr = NF90_DEF_VAR (ncid, 'time', 1, tdim, tid)
  ierr = NF90_DEF_VAR (ncid, 'time', NF90_FLOAT, tdim, tid)
  IF (ierr.NE.NF90_NOERR) STOP 'def time'
  ierr = NF90_PUT_ATT(ncid, tid, 'long_name', 'Time in days')
!   ierr = NF90_PUT_ATT(ncid, tid, 'long_name', 'Time in Julian days')
  IF (ierr.NE.NF90_NOERR) stop 'error def time long_name'
  ierr = NF90_PUT_ATT(ncid, tid, 'calendar', 'standard')
  IF (ierr.NE.NF90_NOERR) stop 'error def time calender'
  !   tunits='days since '//chryr//'-01-01 00:00:00' 
  !   ierr = NF90_PUT_ATT(ncid, tid, 'units',TRIM(tunits))
  ierr = NF90_PUT_ATT(ncid, tid, 'units','days since 1970-01-01 00:00:00')
!  ierr = NF90_PUT_ATT(ncid, tid, 'units','days since -4712-01-01 12:00:00')
  IF (ierr.NE.NF90_NOERR)  stop 'error def time units'
  ierr = NF90_PUT_ATT(ncid, tid, 'standard_name','time')
  IF (ierr.NE.NF90_NOERR) stop 'error def time standard_name'


  ierr = NF90_DEF_VAR (ncid, 'hist2d_cot_bin_border', NF90_FLOAT, hist_cotdim, cotid) 
  IF (ierr.NE.NF90_NOERR) STOP 'stop def cot'
  ierr = NF90_PUT_ATT(ncid, cotid, 'long_name', 'cot histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot long_name'
  ierr = NF90_PUT_ATT(ncid, cotid, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot units'
  !   ierr = NF90_PUT_ATT(ncid, cotid, 'standard_name','histogram_cot')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist2d_cot_bin_centre', NF90_FLOAT, hist_cotdim_bin, cotid_bin) 
  IF (ierr.NE.NF90_NOERR) STOP 'stop def cot_bin'
  ierr = NF90_PUT_ATT(ncid, cotid_bin, 'long_name', 'cot histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot long_name'
  ierr = NF90_PUT_ATT(ncid, cotid_bin, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot units'
  !   ierr = NF90_PUT_ATT(ncid, cotid_bin, 'standard_name','histogram_cot_bin')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot standard_name'



!  ierr = NF90_DEF_VAR (ncid, 'hist_cloud_albedo1', NF90_FLOAT, hist_cloud_albedo1dim, cloud_albedo1id) 
!  IF (ierr.NE.NF90_NOERR) STOP 'stop a def cloud_albedo1'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id, 'long_name', 'cloud_albedo1 histogram border values')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 long_name'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id, 'units', '')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 units'



!  ierr = NF90_DEF_VAR (ncid, 'hist_cloud_albedo2', NF90_FLOAT, hist_cloud_albedo2dim, cloud_albedo2id) 
!  IF (ierr.NE.NF90_NOERR) STOP 'stop  a def cloud_albedo2'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id, 'long_name', 'cloud_albedo2 histogram border values')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 long_name'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id, 'units', '')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 units'


  !   ierr = NF90_PUT_ATT(ncid, cloud_albedoid, 'standard_name','histogram_cloud_albedo')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo standard_name'  

!  ierr = NF90_DEF_VAR (ncid, 'hist_cloud_albedo_bin', NF90_FLOAT, hist_cloud_albedodim_bin, cloud_albedoid_bin) 
!  IF (ierr.NE.NF90_NOERR) STOP 'def cloud_albedo_bin'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedoid_bin, 'long_name', 'cloud_albedo histogram bins')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo long_name'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedoid_bin, 'units', '')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo units'
  !   ierr = NF90_PUT_ATT(ncid, cloud_albedoid_bin, 'standard_name','histogram_cloud_albedo_bin')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo standard_name'






  ierr = NF90_DEF_VAR (ncid, 'hist2d_ctp_bin_border', NF90_FLOAT, hist_ctpdim, ctpid) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctp'
  ierr = NF90_PUT_ATT(ncid, ctpid, 'long_name', 'ctp histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp long_name'
  ierr = NF90_PUT_ATT(ncid, ctpid, 'units', 'hPa')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp units'
  !   ierr = NF90_PUT_ATT(ncid, ctpid, 'standard_name','histogram_ctp')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp standard_name'  


  ierr = NF90_DEF_VAR (ncid, 'hist2d_ctp_bin_centre', NF90_FLOAT, hist_ctpdim_bin, ctpid_bin) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctp'
  ierr = NF90_PUT_ATT(ncid, ctpid_bin, 'long_name', 'ctp histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp long_name'
  ierr = NF90_PUT_ATT(ncid, ctpid_bin, 'units', 'hPa')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp units'
  !   ierr = NF90_PUT_ATT(ncid, ctpid_bin, 'standard_name','histogram_ctp_bin')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp standard_name'


!  ierr = NF90_DEF_VAR (ncid, 'hist_phase', NF90_INT, hist_phasedim, phaseid) 
  ierr = NF90_DEF_VAR (ncid, 'hist_phase', NF90_BYTE, hist_phasedim, phaseid)
  IF (ierr.NE.NF90_NOERR) STOP 'def phase'
  ierr = NF90_PUT_ATT(ncid, phaseid, 'long_name', 'phase histogram bins (0:liquid, 1:ice)')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_phase long_name'
  ierr = NF90_PUT_ATT(ncid, phaseid, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_phase units'
  !   ierr = NF90_PUT_ATT(ncid, phaseid, 'standard_name','histogram_phase')
  !   IF (ierr.NE.NF90_NOERR) stop 'error def hist_phase standard_name'  


  ierr = NF90_DEF_VAR (ncid, 'hist1d_cot_bin_border', NF90_FLOAT, hist_cotdim1d, cotid1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cot'
  ierr = NF90_PUT_ATT(ncid, cotid1d, 'long_name', 'cot histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot long_name'
  ierr = NF90_PUT_ATT(ncid, cotid1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot units'
!  ierr = NF90_PUT_ATT(ncid, cotid1d, 'standard_name','histogram_cot1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_cot_bin_centre', NF90_FLOAT, hist_cotdim_bin1d, cotid_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cot_bin'
  ierr = NF90_PUT_ATT(ncid, cotid_bin1d, 'long_name', 'cot histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot long_name'
  ierr = NF90_PUT_ATT(ncid, cotid_bin1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot units'
!  ierr = NF90_PUT_ATT(ncid, cotid_bin1d, 'standard_name','')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cot standard_name'  




! cloud albedo1
  ierr = NF90_DEF_VAR (ncid, 'hist1d_cla_vis006_bin_border', NF90_FLOAT, hist_cloud_albedo1dim1d, cloud_albedo1id1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'stop b def cloud_albedo1'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id1d, 'long_name', 'cloud albedo1 histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 long_name'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 units'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id1d, 'standard_name','')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_cla_vis006_bin_centre', NF90_FLOAT, hist_cloud_albedo1dim_bin1d, cloud_albedo1id_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'stop c def cloud_albedo1_bin'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id_bin1d, 'long_name', 'cloud abedo1 histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 long_name'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id_bin1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 units'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo1id_bin1d, 'standard_name','')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo1 standard_name'  





! cloud albedo2
  ierr = NF90_DEF_VAR (ncid, 'hist1d_cla_vis008_bin_border', NF90_FLOAT, hist_cloud_albedo2dim1d, cloud_albedo2id1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cloud_albedo2'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id1d, 'long_name', 'cloud albedo2 histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 long_name'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 units'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id1d, 'standard_name','histogram_cloud_albedo21d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_cla_vis008_bin_centre', NF90_FLOAT, hist_cloud_albedo2dim_bin1d, cloud_albedo2id_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cloud_albedo2_bin'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id_bin1d, 'long_name', 'cloud albedo2 histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 long_name'
  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id_bin1d, 'units', '1')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 units'
!  ierr = NF90_PUT_ATT(ncid, cloud_albedo2id_bin1d, 'standard_name','histogram_cloud_albedo2_bin1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cloud_albedo2 standard_name'  






!cer
  ierr = NF90_DEF_VAR (ncid, 'hist1d_cer_bin_border', NF90_FLOAT, hist_refdim1d,refid1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cer'
  ierr = NF90_PUT_ATT(ncid, refid1d, 'long_name', 'cer histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer long_name'
  ierr = NF90_PUT_ATT(ncid, refid1d, 'units', 'um')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer units'
!  ierr = NF90_PUT_ATT(ncid, refid1d,'standard_name','histogram_cer1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_cer_bin_centre', NF90_FLOAT, hist_refdim_bin1d, refid_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cer_bin'
  ierr = NF90_PUT_ATT(ncid, refid_bin1d, 'long_name', 'cer histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer long_name'
  ierr = NF90_PUT_ATT(ncid, refid_bin1d, 'units', 'um')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer units'
!  ierr = NF90_PUT_ATT(ncid, refid_bin1d, 'standard_name','histogram_cer_bin1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cer standard_name'  
!--

!cwp
  ierr = NF90_DEF_VAR (ncid, 'hist1d_cwp_bin_border', NF90_FLOAT, hist_cwpdim1d,cwpid1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cwp'
  ierr = NF90_PUT_ATT(ncid, cwpid1d, 'long_name', 'cwp histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp long_name'
  ierr = NF90_PUT_ATT(ncid, cwpid1d, 'units', 'g/m2')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp units'
!  ierr = NF90_PUT_ATT(ncid, cwpid1d,'standard_name','histogram_cwp1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_cwp_bin_centre', NF90_FLOAT, hist_cwpdim_bin1d, cwpid_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def cwp_bin'
  ierr = NF90_PUT_ATT(ncid, cwpid_bin1d, 'long_name', 'cwp histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp long_name'
  ierr = NF90_PUT_ATT(ncid, cwpid_bin1d, 'units', 'g/m2')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp units'
!  ierr = NF90_PUT_ATT(ncid, cwpid_bin1d, 'standard_name','histogram_cwp_bin1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_cwp standard_name'  
!--

  ierr = NF90_DEF_VAR (ncid, 'hist1d_ctp_bin_border', NF90_FLOAT, hist_ctpdim1d, ctpid1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctp'
  ierr = NF90_PUT_ATT(ncid, ctpid1d, 'long_name', 'ctp histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp long_name'
  ierr = NF90_PUT_ATT(ncid, ctpid1d, 'units', 'hPa')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp units'
!  ierr = NF90_PUT_ATT(ncid, ctpid1d, 'standard_name','histogram_ctp1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_ctp_bin_centre', NF90_FLOAT, hist_ctpdim_bin1d, ctpid_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctp_bin'
  ierr = NF90_PUT_ATT(ncid, ctpid_bin1d, 'long_name', 'ctp histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp long_name'
  ierr = NF90_PUT_ATT(ncid, ctpid_bin1d, 'units', 'hPa')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp units'
!  ierr = NF90_PUT_ATT(ncid, ctpid_bin1d, 'standard_name','histogram_ctp_bin1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctp standard_name'  



  ierr = NF90_DEF_VAR (ncid, 'hist1d_ctt_bin_border', NF90_FLOAT, hist_cttdim1d, cttid1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctt'
  ierr = NF90_PUT_ATT(ncid, cttid1d, 'long_name', 'ctt histogram bin border values')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt long_name'
  ierr = NF90_PUT_ATT(ncid, cttid1d, 'units', 'K')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt units'
!  ierr = NF90_PUT_ATT(ncid, cttid1d, 'standard_name','histogram_ctt1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt standard_name'  

  ierr = NF90_DEF_VAR (ncid, 'hist1d_ctt_bin_centre', NF90_FLOAT, hist_cttdim_bin1d, cttid_bin1d) 
  IF (ierr.NE.NF90_NOERR) STOP 'def ctt_bin'
  ierr = NF90_PUT_ATT(ncid, cttid_bin1d, 'long_name', 'ctt histogram bin centres')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt long_name'
  ierr = NF90_PUT_ATT(ncid, cttid_bin1d, 'units', 'K')
  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt units'
!  ierr = NF90_PUT_ATT(ncid, cttid_bin1d, 'standard_name','histogram_ctt_bin1d')
!  IF (ierr.NE.NF90_NOERR) stop 'error def hist_ctt standard_name'  


  ctitle='ESA Cloud CCI Retrieval Products L3 Output File'
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
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'institution',trim(adjustl(cinst)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'


  !put in here the original data source
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'source',trim(adjustl(csource)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  !put in here the number of processed orbits 
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'number_of_processed_orbits', num_of_orbits)
  IF (ierr.NE.NF90_NOERR) stop 'error def number_of_processed_orbits'

  !sstapelb ; not compliant with data standard-------------------------------
  !   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Grid_Type',trim(adjustl(grid_type)))
  !   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  !   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'NetCDF_Version',trim(adjustl(cncver)))
  !   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  !   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'closure_Flag', trim(adjustl(l1closure)))
  !   IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_resolution', trim(adjustl(cgridx)))
  !       & trim(adjustl(cgridx))//' degree')
  IF (ierr.NE.NF90_NOERR) stop 'error def geospatial_lon_resolution'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_resolution', trim(adjustl(cgridy)))
  !       & trim(adjustl(cgridy))//' degree')
  IF (ierr.NE.NF90_NOERR) stop 'error def geospatial_lat_resolution'
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
  !-------------------------------------------------------------------------------------



  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_min', float(slat) + (1./ (2.0 * dy)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_max', float(elat) - (1./ (2.0 * dy)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_min', float(slon) + (1./(2.0 * dx)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_max',float(elon) - (1./ (2.0 * dx)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!
!  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_min', -lat_offset + (1./ (2 * dy)))
!  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lat_max', (FLOAT(ny-1))/dy - (lat_offset- 1./dy + (1./ (2 * dy))))
!  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!
!  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_min', -float(lon_offset) + (1./(2 * dx)))
!  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
!  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'geospatial_lon_max', (FLOAT(nx-1))/dx - ( float(lon_offset)- 1./dx + (1./ (2 * dx))))
!  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  ! sstapelb added attribute 19.11.2012
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
  !file name
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
  !Not used could be replace by time_coverage etc.  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Product_Date',&
  !not used       & trim(adjustl(trim(adjustl(year))//trim(adjustl(month))//trim(adjustl(day)))))
  !  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'

  ! sstapelb 15.11.2012
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_start',trim(adjustl(trim(adjustl(year))// &
       & trim(adjustl(month))))//'01T000000Z')
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_end',trim(adjustl(trim(adjustl(year))// & 
       & trim(adjustl(month))//trim(adjustl(dom))//'T235959Z')))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_duration',trim(adjustl(dur)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'time_coverage_resolution',trim(adjustl(dur)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'


  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'references',trim(adjustl(reference)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'history',trim(adjustl(history)))
  !   ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'history',trim(adjustl(prodtime))//' Product Generated from '//trim(adjustl(l2cproc)))
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  !   summary=  "This dataset contains monthly Level-3 global cloud property products &
  !             & from satellite observations. Averaged onto a regular grid."
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

  !FAME-C
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'cdm_data_type',trim(adjustl(cdm_data_type)))
  IF (ierr.NE.NF90_NOERR) stop 'error def cdm_data_type'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'keywords_vocabulary',trim(adjustl(keywords_vocabulary)))
  IF (ierr.NE.NF90_NOERR) stop 'error def keywords vocabulary'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'naming_authority',trim(adjustl(naming_authority)))
  IF (ierr.NE.NF90_NOERR) stop 'error def naming authority'



  !hist_cot1d(hist_cot1d) ;
  !        float hist_cot_bin

  ierr = NF90_ENDDEF(ncid, h_minfree=100000)

  IF (ierr.NE.NF90_NOERR)  stop 'error enddef '
  !
  if (wo .eq. 1) write(*,*) "end of defining NetCDF global variables"

  if (wo .eq. 1) write(*,*) "write global variables to NetCDF file"

  if (wo .eq. 1) write(*,*) "   create grid resolution lat/lon"

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


!!$  lat(1) = -lat_offset + (1./ (2 * dy))
!!$  DO i=2,ny
!!$     lat(i) = (FLOAT(i-1))/dy - (lat_offset- 1./dy + (1./ (2 * dy)))             !create grid resolution lat
!!$  ENDDO

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


!  lon(1) = -float(lon_offset) + (1./(2 * dx))
!  DO i=2,nx
!     lon(i) = (FLOAT(i-1))/dx - ( float(lon_offset)- 1./dx + (1./ (2 * dx)))              !create grid resolution lon
!  ENDDO

  start(1)=1
  stride(1)=1
  counter(1)=nx
  ierr = NF90_PUT_VAR(ncid, xid, lon,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write lon'
  !
  IF (wo.EQ.1) THEN
     write(*,*) lat(1:10)
     write(*,*) lon(1:10)
  ENDIF

  read(year,*) yy
  read(month,*) mm

  ! set time to double precision?
  time = 367.0 * yy - aint((7.0*(yy+aint((mm + 9.)/12.)))/4.) + aint((275.*mm)/9.) + 1. + 1721013.5 !dd + 1721013.5
  time = time - jul_start ! lets start from 1.1. 1970 00:00:00

  ierr = NF90_PUT_VAR(ncid, tid, time)
  IF (ierr.NE.NF90_NOERR) STOP 'write time'

!!$  ierr = NF90_PUT_VAR(ncid, tid, yday)!,1, time, 1)
!!$  IF (ierr.NE.NF90_NOERR) STOP 'write time'

  start(1)=1
  stride(1)=1
  counter(1)=n_hist_cot
  ierr = NF90_PUT_VAR(ncid, cotid, hist_cot,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cot'

  start(1)=1
  stride(1)=1
  counter(1)=n_hist_cot-1
  ierr = NF90_PUT_VAR(ncid, cotid_bin, hist_cot_bin,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cot'


!
!  start(1)=1
!  stride(1)=1
!  counter(1)=n_hist_cloud_albedo
!  ierr = NF90_PUT_VAR(ncid, cloud_albedoid, hist_cloud_albedo,start,counter,stride)
!  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo'

!  start(1)=1
!  stride(1)=1
!  counter(1)=n_hist_cloud_albedo-1
!  ierr = NF90_PUT_VAR(ncid, cloud_albedoid_bin, hist_cloud_albedo_bin,start,counter,stride)
!  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo'

  start(1)=1
  stride(1)=1
  counter(1)=n_hist_ctp
  ierr = NF90_PUT_VAR(ncid, ctpid, hist_ctp,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctp'

  start(1)=1
  stride(1)=1
  counter(1)=n_hist_ctp-1
  ierr = NF90_PUT_VAR(ncid, ctpid_bin, hist_ctp_bin,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctp'


  start(1)=1
  stride(1)=1
  counter(1)=n_hist_phase
  hist_phase(1)=0
  hist_phase(2)=1
  ierr = NF90_PUT_VAR(ncid, phaseid, hist_phase,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctp'


  ! COT
  start(1)=1
  stride(1)=1
  counter(1)=n_cot_bins+1
  ierr = NF90_PUT_VAR(ncid, cotid1d, hist_cot_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cot'

  start(1)=1
  stride(1)=1
  counter(1)=n_cot_bins
  ierr = NF90_PUT_VAR(ncid, cotid_bin1d, hist_cot_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cot'



  ! CLOUD_ALBEDO1
  start(1)=1
  stride(1)=1
  counter(1)=n_cloud_albedo1_bins+1
  ierr = NF90_PUT_VAR(ncid, cloud_albedo1id1d, hist_cloud_albedo1_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo1'

  start(1)=1
  stride(1)=1
  counter(1)=n_cloud_albedo1_bins
  ierr = NF90_PUT_VAR(ncid, cloud_albedo1id_bin1d, hist_cloud_albedo1_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo1'




  ! CLOUD_ALBEDO2
  start(1)=1
  stride(1)=1
  counter(1)=n_cloud_albedo2_bins+1
  ierr = NF90_PUT_VAR(ncid, cloud_albedo2id1d, hist_cloud_albedo2_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo2'

  start(1)=1
  stride(1)=1
  counter(1)=n_cloud_albedo2_bins
  ierr = NF90_PUT_VAR(ncid, cloud_albedo2id_bin1d, hist_cloud_albedo2_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cloud_albedo2'






  ! CER
  start(1)=1
  stride(1)=1
  counter(1)=n_ref_bins+1
  ierr = NF90_PUT_VAR(ncid, refid1d, hist_ref_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cer'


  start(1)=1
  stride(1)=1
  counter(1)=n_ref_bins
  ierr = NF90_PUT_VAR(ncid, refid_bin1d, hist_ref_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cer'

  
  ! CWP
  start(1)=1
  stride(1)=1
  counter(1)=n_cwp_bins+1
  ierr = NF90_PUT_VAR(ncid, cwpid1d, hist_cwp_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cwp'

  start(1)=1
  stride(1)=1
  counter(1)=n_cwp_bins
  ierr = NF90_PUT_VAR(ncid, cwpid_bin1d, hist_cwp_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_cwp'

  ! CTP
  start(1)=1
  stride(1)=1
  counter(1)=n_ctp_bins+1
  ierr = NF90_PUT_VAR(ncid, ctpid1d, hist_ctp_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctp'

  start(1)=1
  stride(1)=1
  counter(1)=n_ctp_bins
  ierr = NF90_PUT_VAR(ncid, ctpid_bin1d, hist_ctp_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctp'


  ! CTT
  start(1)=1
  stride(1)=1
  counter(1)=n_ctt_bins+1
  ierr = NF90_PUT_VAR(ncid, cttid1d, hist_ctt_1d_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctt'

  start(1)=1
  stride(1)=1
  counter(1)=n_ctt_bins
  ierr = NF90_PUT_VAR(ncid, cttid_bin1d, hist_ctt_1d_bin_axis,start,counter,stride)
  IF (ierr.NE.NF90_NOERR) STOP 'write hist_ctt'


  ! sstapelb added time dim
  dims_var(1) = xdim
  dims_var(2) = ydim
  dims_var(3) = tdim

  dims_var_phase(1) = xdim
  dims_var_phase(2) = ydim 
  dims_var_phase(3) = n_hist_phase

  ! sstapelb added time dim
  dims_var_hist2d(1) = xdim !1
  dims_var_hist2d(2) = ydim !3
  dims_var_hist2d(3) = hist_cotdim_bin
  dims_var_hist2d(4) = hist_ctpdim_bin
  dims_var_hist2d(5) = hist_phasedim
  dims_var_hist2d(6) = tdim

  !   dims_var_hist2d(1) = xdim !1
  !   dims_var_hist2d(2) = ydim !3
  !   dims_var_hist2d(3) = hist_cotdim_bin
  !   dims_var_hist2d(4) = hist_ctpdim_bin
  !   dims_var_hist2d(5) = hist_phasedim

  dims_var_histcot1d(1)=xdim
  dims_var_histcot1d(2)=ydim
  dims_var_histcot1d(3)=hist_cotdim_bin1d
  dims_var_histcot1d(4)=hist_phasedim
  dims_var_histcot1d(5)=tdim


  dims_var_histcloud_albedo11d(1)=xdim
  dims_var_histcloud_albedo11d(2)=ydim
  dims_var_histcloud_albedo11d(3)=hist_cloud_albedo1dim_bin1d
  dims_var_histcloud_albedo11d(4)=hist_phasedim
  dims_var_histcloud_albedo11d(5)=tdim



  dims_var_histcloud_albedo21d(1)=xdim
  dims_var_histcloud_albedo21d(2)=ydim
  dims_var_histcloud_albedo21d(3)=hist_cloud_albedo2dim_bin1d
  dims_var_histcloud_albedo21d(4)=hist_phasedim
  dims_var_histcloud_albedo21d(5)=tdim



  dims_var_histref1d(1)=xdim
  dims_var_histref1d(2)=ydim
  dims_var_histref1d(3)=hist_refdim_bin1d
  dims_var_histref1d(4)=hist_phasedim
  dims_var_histref1d(5)=tdim

  dims_var_histcwp1d(1)=xdim
  dims_var_histcwp1d(2)=ydim
  dims_var_histcwp1d(3)=hist_cwpdim_bin1d
  dims_var_histcwp1d(4)=hist_phasedim
  dims_var_histcwp1d(5)=tdim

  dims_var_histctp1d(1)=xdim
  dims_var_histctp1d(2)=ydim
  dims_var_histctp1d(3)=hist_ctpdim_bin1d
  dims_var_histctp1d(4)=hist_phasedim
  dims_var_histctp1d(5)=tdim

  dims_var_histctt1d(1)=xdim
  dims_var_histctt1d(2)=ydim
  dims_var_histctt1d(3)=hist_cttdim_bin1d
  dims_var_histctt1d(4)=hist_phasedim
  dims_var_histctt1d(5)=tdim


  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'New file created: ',TRIM(path)
  ENDIF

  RETURN

END SUBROUTINE nc_create_global
