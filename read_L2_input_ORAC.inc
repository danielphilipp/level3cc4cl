  ! Name: read_L2_input_orac.inc
  !
  !
  ! Purpose: F90
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
  !
  !2012/02/03 Matthias Jerg original code
  !2015/03/20 CP add cloud albedo
  !2015/07/07 CP add corrected cth
  !2015/04/23 OS+SteSta added reading of solzen, relaz, stemp(_error), cld_type
  !2015/07/10 OS minor edit
  !2015/08/31 OS use variable "phase" instead of "phase_pavolonis"
  !           for cty; read phase only once
  !2015/10/27 CP add stemp
  !2015/11/03 CP add ATSR-2 capability
  !2015/12/16 OS replaced wo with verbose; now reading additional variables
  !           cloud albedo and convergence
  !2016/01/21 OS replaced ref with cer
  !2016/02/18 OS added CEE, CTT_corrected, CTP_corrected
  !2016/02/24 CP added AATSR spefic condition to remove overlaping orbits
  !2017/06/22 OS added ann phase, MODIS refl/BT, toa/boa
  !2017/06/29 OS Added cph uncertainty
  !2019/09/06 Daniel Philipp added SEVIRI support

  ! $Id$
  !
  ! Bugs:
  !
  !none known
  !fudge the number of nodes, in normal operation the L2 file is read and
  !the node dimension colapses to one entry, see also ORAC_L2B branch
  nnodes=1
  inode=nnodes

  !read more dimension information
  name='across_track'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),verbose)
  call nc_dim_length(ncid,name,dimids(1),dummyint,verbose)
  dimlength(1)=dummyint
  xdim1km=int(dimlength(1),kind=lint)

  name='along_track'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),verbose)
  call nc_dim_length(ncid,name,dimids(2),dummyint,verbose)

  !
  !if AATSR instrument then shorten the lenght of the swath to remove overlapping portion
  !

  if ( platform(1:4) == "ENV" ) then
     if ( dummyint .gt. 40240)  then
        dummyint=40240
     endif
  endif

  dimlength(2)=dummyint
  ydim1km=int(dimlength(2),kind=lint)

  allocate(lat_1km_raw_2d(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate lat_1km_raw_2d failed: ierr=', ierr
  endif
  lat_1km_raw_2d=real_fill_value
  allocate(lon_1km_raw_2d(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate lon_1km_raw_2d failed: ierr=', ierr
  endif
  lon_1km_raw_2d=real_fill_value
  allocate(sensorzenith_1km_raw_2d(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate sensorzenith_1km_raw_2d failed: ierr=', ierr
  endif
  sensorzenith_1km_raw_2d=real_fill_value
  allocate(solarzenith_1km_raw_2d(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate solarzenith_1km_raw_2d failed: ierr=', ierr
  endif
  solarzenith_1km_raw_2d=real_fill_value
  allocate(relazimuth_1km_raw_2d(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate relazimuth_1km_raw_2d failed: ierr=', ierr
  endif
  relazimuth_1km_raw_2d=real_fill_value

  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &                 'lon',lon_1km_raw_2d,lon_unit,verbose)
  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'lat',lat_1km_raw_2d,lat_unit,verbose)
  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'satellite_zenith_view_no1',sensorzenith_1km_raw_2d, &
       &          dummy_unit,verbose)
  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'solar_zenith_view_no1',solarzenith_1km_raw_2d, &
       &          dummy_unit,verbose)
  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'rel_azimuth_view_no1',relazimuth_1km_raw_2d, &
       &          dummy_unit,verbose)

  !Define L2 input structure and read then the variables
  call set_l2_input_struct_2d(l2_input_2d,xdim1km,ydim1km,nnodes,&
       & nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features,calgo,llocal)

  !   l2_input_2d%vname=varname_1km
  !   l2_input_2d%ename=''
  l2_input_2d%lon(:,:)=lon_1km_raw_2d
  l2_input_2d%lat(:,:)=lat_1km_raw_2d
  l2_input_2d%satzen(:,:)=sensorzenith_1km_raw_2d
  l2_input_2d%solzen(:,:)=solarzenith_1km_raw_2d
  l2_input_2d%relazi(:,:)=relazimuth_1km_raw_2d

  deallocate(lat_1km_raw_2d,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate lat_1km_raw_2d failed: ierr=', ierr
  endif
  deallocate(lon_1km_raw_2d,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate lon_1km_raw_2d failed: ierr=', ierr
  endif
  deallocate(sensorzenith_1km_raw_2d,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate sensorzenith_1km_raw_2d failed: ierr=', ierr
  endif
  deallocate(solarzenith_1km_raw_2d,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate solarzenith_1km_raw_2d failed: ierr=', ierr
  endif
  deallocate(relazimuth_1km_raw_2d,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate relazimuth_1km_raw_2d failed: ierr=', ierr
  endif

  allocate(l2var_dummy(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate l2var_dummy failed: ierr=', ierr
  endif
  l2var_dummy=real_fill_value

  allocate(l2var_dummy_double(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate l2var_dummy_double failed: ierr=', ierr
  endif
  l2var_dummy_double=double_fill_value

  allocate(l2var_dummy_byte(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate l2var_dummy_byte failed: ierr=', ierr
  endif
  l2var_dummy_byte=short_int_fill_value

  allocate(l2var_dummy_stint(xdim1km,ydim1km),stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'allocate l2var_dummy_stint failed: ierr=', ierr
  endif
  l2var_dummy_stint=nint_fill_value

  !variables follow:
  !time
  call nc_read_array_2d_double_orac(ncid,xdim1km,ydim1km,"time",l2var_dummy_double,verbose)
  l2_input_2d%time(:,:,inode)=l2var_dummy_double

  !ctt
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctt(:,:,inode)=l2var_dummy

  !ctt_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctt_error(:,:,inode)=l2var_dummy

  !ctt_corrected
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt_corrected',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctt_corrected(:,:,inode)=l2var_dummy

  !ctt_corrected_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt_corrected_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctt_corrected_error(:,:,inode)=l2var_dummy

  !cth
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cth(:,:,inode)=l2var_dummy

  !cth_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cth_error(:,:,inode)=l2var_dummy



  !cth_corrected
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth_corrected',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cth_corrected(:,:,inode)=l2var_dummy

  !cth_corrected_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth_corrected_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cth_corrected_error(:,:,inode)=l2var_dummy



  !ctp
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctp(:,:,inode)=l2var_dummy

  !ctp_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctp_error(:,:,inode)=l2var_dummy

  !ctp_corrected
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_corrected',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctp_corrected(:,:,inode)=l2var_dummy

  !ctp_corrected_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_corrected_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ctp_corrected_error(:,:,inode)=l2var_dummy

  !cct
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cc_total',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cct_post(:,:,inode)=l2var_dummy

  !cct_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cc_total_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cct_error(:,:,inode)=l2var_dummy

  !cct_pre

  !  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
  !       &          'cloudmask_pre',l2var_dummy,dummy_unit,verbose)
  !  l2_input_2d%cct(:,:,inode)=l2var_dummy

  !change the cct to be the post flag this is a test
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cc_total',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cct(:,:,inode)=l2var_dummy


  !cot
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cot',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cot(:,:,inode)=l2var_dummy

  !cot_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cot_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cot_error(:,:,inode)=l2var_dummy

  !cccot
  !   call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
  !        &          'cccot',l2var_dummy,dummy_unit,verbose)
  !   l2_input_2d%cccot(:,:,inode)=l2var_dummy

  !cccot_pre
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cccot_pre',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cccot_pre(:,:,inode)=l2var_dummy

  !ann_phase_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ann_phase_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ann_phase_uncertainty(:,:,inode)=l2var_dummy

  !cphcot
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cphcot',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cphcot(:,:,inode)=l2var_dummy
  
  !ref
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cer',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ref(:,:,inode)=l2var_dummy

  !ref_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cer_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%ref_error(:,:,inode)=l2var_dummy

  if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2')  ) then
     write(*,*)' read_l2_input_orac level3 processing for AATSR'
     !cloud_albedo
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_in_channel_no_2',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

     !cloud_albedo_uncertainty does not exist yet
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_uncertainty_in_channel_no_2',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo1_error(:,:,inode)=l2var_dummy


     !cloud_albedo
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_in_channel_no_3',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy

     !cloud_albedo_uncertainty does not exist yet
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_uncertainty_in_channel_no_3',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo2_error(:,:,inode)=l2var_dummy

  endif

  if ((csensor .eq. 'AVHRR') .or. (csensor .eq. 'MODIS') .or. (csensor .eq. 'SEVIRI')) then
     write(*,*)' read_l2_input_orac level3 processing for AVHRR or MODIS or SEVIRI'
     !cloud_albedo
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_in_channel_no_1',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

     !cloud_albedo_uncertainty does not exist yet
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_uncertainty_in_channel_no_1',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo1_error(:,:,inode)=l2var_dummy

     !cloud_albedo
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_in_channel_no_2',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy

     !cloud_albedo_uncertainty does not exist yet
     call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
          &          'cloud_albedo_uncertainty_in_channel_no_2',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cloud_albedo2_error(:,:,inode)=l2var_dummy

  endif

  ! cloud effective emissivity (CEE)
  if (csensor .eq. 'AVHRR') then

     cee_11um_input_string             = 'cee_in_channel_no_5'
     cee_11um_uncertainty_input_string = 'cee_uncertainty_in_channel_no_5'

  else if (csensor .eq. 'MODIS') then

     cee_11um_input_string             = 'cee_in_channel_no_31'
     cee_11um_uncertainty_input_string = 'cee_uncertainty_in_channel_no_31'

  else if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2')) then

     cee_11um_input_string             = 'cee_in_channel_no_6'
     cee_11um_uncertainty_input_string = 'cee_uncertainty_in_channel_no_6'

  else if (csensor .eq. 'SEVIRI') then
        cee_11um_input_string             = 'cee_in_channel_no_9'
        cee_11um_uncertainty_input_string = 'cee_uncertainty_in_channel_no_9'

  endif

  ! cloud effective emissivity (CEE)
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          cee_11um_input_string,l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cee(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          cee_11um_uncertainty_input_string,l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cee_error(:,:,inode)=l2var_dummy

  !cwp
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cwp',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cwp(:,:,inode)=l2var_dummy

  !cwp_uncertainty
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cwp_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cwp_error(:,:,inode)=l2var_dummy

  !stemp
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'stemp',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%stemp(:,:,inode)=l2var_dummy

  !stemp_error
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'stemp_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%stemp_error(:,:,inode)=l2var_dummy

  !qcflag
  !  call nc_read_array_2d_short_to_short_orac(ncid,xdim1km,ydim1km, &
  !       &          'qcflag',l2_input_2d%qcflag(:,:,inode),dummy_unit,verbose)
  !stapel

!MST===
  call nc_read_array_2d_short_to_short_orac(ncid,xdim1km,ydim1km, &
       &          'qcflag',l2var_dummy_stint,dummy_unit,verbose)
  l2_input_2d%qcflag(:,:,inode)=l2var_dummy_stint
!l2_input_2d%qcflag(:,:,inode)=0

!MST===
!call nc_read_array_2d_short_to_short_orac(ncid,xdim1km,ydim1km, &
!       &          'convergence',l2var_dummy_stint,dummy_unit,verbose)
!  l2_input_2d%convergence(:,:,inode)=l2var_dummy_stint
l2_input_2d%convergence(:,:,inode)=0
!====
!  !stapel

  !cty_post
  !   call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
  !        &          'phase',l2var_dummy_byte,dummy_unit,verbose)
  !   l2_input_2d%cty_post(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

  !cty_pre
  call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
       &          'phase',l2var_dummy_byte,dummy_unit,verbose)
  l2_input_2d%cty(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ann_phase_uncertainty',l2var_dummy,dummy_unit,verbose)
  l2_input_2d%cty_error(:,:,inode)=real(l2var_dummy,kind=sreal)  
  
  !cloud type
  call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
       &          'cldtype',l2var_dummy_byte,dummy_unit,verbose)
  l2_input_2d%cld_type(:,:,inode)=l2var_dummy_byte

  ! !ann_phase
  ! call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
  !      &          'ann_phase',l2var_dummy_byte,dummy_unit,verbose)
  ! l2_input_2d%ann_phase(:,:,inode)=l2var_dummy_byte
  
  !read illumination: to be used in different cloud covers
  call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
       &          'illum',l2var_dummy_byte,dummy_unit,verbose)
  l2_input_2d%illum(:,:,inode)=l2var_dummy_byte

  !read land-sea flag: to be used in different cloud covers
    call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
        &          'lsflag',l2var_dummy_byte,dummy_unit,verbose)
    l2_input_2d%lsflag(:,:,inode)=l2var_dummy_byte
             
  if (llocal) then
     call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'reflectance_in_channel_no_1',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%reflectance06(:,:,inode)=l2var_dummy

     call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'reflectance_in_channel_no_2',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%reflectance08(:,:,inode)=l2var_dummy

     if ((csensor .eq. 'AVHRR') .or. (csensor .eq. 'SEVIRI')) then
       call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'reflectance_in_channel_no_3',l2var_dummy,dummy_unit,verbose)
     else if (csensor .eq. 'MODIS') then
       call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'reflectance_in_channel_no_6',l2var_dummy,dummy_unit,verbose)
     else if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2')) then
       call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'reflectance_in_channel_no_6',l2var_dummy,dummy_unit,verbose)
     endif

     l2_input_2d%reflectance16(:,:,inode)=l2var_dummy

     if (csensor .eq. 'AVHRR') then
          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_4',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT37(:,:,inode)=l2var_dummy

          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_5',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT11(:,:,inode)=l2var_dummy

          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_6',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT12(:,:,inode)=l2var_dummy                              
     else if (csensor .eq. 'MODIS') then      
          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'brightness_temperature_in_channel_no_20',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT37(:,:,inode)=l2var_dummy

          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'brightness_temperature_in_channel_no_31',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT11(:,:,inode)=l2var_dummy

          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
          &          'brightness_temperature_in_channel_no_32',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT12(:,:,inode)=l2var_dummy
     else if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2')) then
          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_20',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT37(:,:,inode)=l2var_dummy

          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_31',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT11(:,:,inode)=l2var_dummy
 
          call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &          'brightness_temperature_in_channel_no_32',l2var_dummy,dummy_unit,verbose)
          l2_input_2d%BT12(:,:,inode)=l2var_dummy         
    else if (csensor .eq. 'SEVIRI') then
        call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &           'brightness_temperature_in_channel_no_4',l2var_dummy,dummy_unit,verbose)    
        l2_input_2d%BT37(:,:,inode)=l2var_dummy

        call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &           'brightness_temperature_in_channel_no_9',l2var_dummy,dummy_unit,verbose)
        l2_input_2d%BT11(:,:,inode)=l2var_dummy

        call nc_read_array_2d_short_orac(ncid_secondary,xdim1km,ydim1km, &
                    &           'brightness_temperature_in_channel_no_10',l2var_dummy,dummy_unit,verbose)
        l2_input_2d%BT12(:,:,inode)=l2var_dummy

     endif

  endif

  if (proc_toa) then
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'toa_swup',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%toa_swup(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'toa_swdn',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%toa_swdn(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'toa_swup_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%toa_swup_clr(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'toa_lwup',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%toa_lwup(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'toa_lwup_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%toa_lwup_clr(:,:,inode)=l2var_dummy

     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_swup',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_swup(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_swup_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_swup_clr(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_lwup',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_lwup(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_lwup_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_lwup_clr(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_swdn',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_swdn(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_swdn_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_swdn_clr(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_lwdn',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_lwdn(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_lwdn_clr',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_lwdn_clr(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_par_dif',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_par_dif(:,:,inode)=l2var_dummy
     call nc_read_array_2d_float_orac(ncid_toa,xdim1km,ydim1km, &
          'boa_par_tot',l2var_dummy,dummy_unit,verbose)
     l2_input_2d%boa_par_tot(:,:,inode)=l2var_dummy
  endif

  deallocate(l2var_dummy,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2var_dummy failed: ierr=', ierr
  endif
  deallocate(l2var_dummy_double,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2var_dummy_double failed: ierr=', ierr
  endif
  deallocate(l2var_dummy_byte,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2var_dummy_byte failed: ierr=', ierr
  endif
  deallocate(l2var_dummy_stint,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2var_dummy_stint failed: ierr=', ierr
  endif
