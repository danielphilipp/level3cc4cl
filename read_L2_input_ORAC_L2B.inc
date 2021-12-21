  ! Name: read_l2_input_orac_l2b.inc
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
  !2015-03-20 CP add cloud albedo
  !2015-07-07 CP add corrected cloud top height
  !2015-11-03 CP add surface temeparture
  !2015-11-03 CP add Atsr-2 capability
  !2015-12-16 OS replaced wo with verbose
  !2016-01-21 OS replaced ref with cer
  !2016-02-18 OS added CTP_corrected, CEE
  !2019-09-06 Daniel Philipp added SEVIRI support
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  !read more dimension information
  
  !we have ascending and descending node
  if (csensor .eq. 'SEVIRI') then
    nnodes = 1
  else
    nnodes=2
  endif

  name='lon'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),verbose)
  call nc_dim_length(ncid,name,dimids(1),dummyint,verbose)
  dimlength(1)=dummyint
  xdim1km=int(dimlength(1),kind=lint)

  name='lat'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),verbose)
  call nc_dim_length(ncid,name,dimids(2),dummyint,verbose)
  dimlength(2)=dummyint
  ydim1km=int(dimlength(2),kind=lint)

  allocate(lat_1km_raw_1d(ydim1km))
  lat_1km_raw_1d=real_fill_value
  allocate(lon_1km_raw_1d(xdim1km))
  lon_1km_raw_1d=real_fill_value

  !we have a regular lat-lon grid
  call nc_read_array_1d_float_orac(ncid,xdim1km, &
       &                 'lon',lon_1km_raw_1d,lon_unit,verbose)
  call nc_read_array_1d_float_orac(ncid,ydim1km, &
       &          'lat',lat_1km_raw_1d,lat_unit,verbose)

  !Define L2 input structure and read then the variables
  if(ind .eq. 1 ) call set_l2_input_struct_2d(l2_input_2d,xdim1km,ydim1km,nnodes,&
       & nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features)              

  !  l2_input_2d%vname=varname_1km
  !  l2_input_2d%ename=''

  !generate 2D coordinates from the regular lat-lon axes
  do idim=1,xdim1km
     l2_input_2d%lat(idim,:)=lat_1km_raw_1d(:)
  enddo
  do jdim=1,ydim1km
     l2_input_2d%lon(:,jdim)=lon_1km_raw_1d(:)
  enddo

  deallocate(lat_1km_raw_1d)
  deallocate(lon_1km_raw_1d)

  allocate(l2var_dummy(xdim1km,ydim1km))
  l2var_dummy=real_fill_value

  allocate(l2var_dummy_double(xdim1km,ydim1km))
  l2var_dummy_double=double_fill_value

  allocate(l2var_dummy_stint(xdim1km,ydim1km))
  l2var_dummy_stint=nint_fill_value

  allocate(l2var_dummy_byte(xdim1km,ydim1km))
  l2var_dummy_byte=short_int_fill_value

  !loop over the nodes inside the L2b (aka L3U) file

  do inode=1,nnodes

     !set the suffixes of the variable names  
     if (csensor .eq. 'SEVIRI') then
        node_suffix=''
     else
        if(inode .eq.1) node_suffix='_asc'
        if(inode .eq.2) node_suffix='_desc'
     endif

     !variables follow:

     !time
     call nc_read_array_2d_double_orac(ncid,xdim1km,ydim1km,'time'//trim(adjustl(node_suffix)),l2var_dummy_double,verbose)
     l2_input_2d%time(:,:,inode)=l2var_dummy_double

     !ctt    
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctt'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%ctt(:,:,inode)=l2var_dummy

     !ctt_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctt_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%ctt_error(:,:,inode)=l2var_dummy

     !ctt_corrected    
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctt_corrected'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%ctt_corrected(:,:,inode)=l2var_dummy

     !ctt_corrected_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctt_corrected_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%ctt_corrected_error(:,:,inode)=l2var_dummy


     !stemp    
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'stemp'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%stemp(:,:,inode)=l2var_dummy

     !stemp_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'stemp_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%stemp_error(:,:,inode)=l2var_dummy




     !cth
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cth'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%cth(:,:,inode)=l2var_dummy

     !cth_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cth_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%cth_error(:,:,inode)=l2var_dummy



     !cth_corrected
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cth_corrected'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%cth_corrected(:,:,inode)=l2var_dummy

     !cth_corrected_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cth_corrected_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)            
     l2_input_2d%cth_corrected_error(:,:,inode)=l2var_dummy


     !ctp
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctp'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ctp(:,:,inode)=l2var_dummy

     !ctp_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctp_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ctp_error(:,:,inode)=l2var_dummy

     !ctp_corrected
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctp_corrected'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ctp_corrected(:,:,inode)=l2var_dummy

     !ctp_corrected_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ctp_corrected_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ctp_corrected_error(:,:,inode)=l2var_dummy

     !cct
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
          &          'cc_mask'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%cct(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

     !cct_pre
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
          &          'cc_mask_pre'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%cct_pre(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

     !cot
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cot'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cot(:,:,inode)=l2var_dummy

     !cot_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cot_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cot_error(:,:,inode)=l2var_dummy

     !cccot
     !      call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
     !           &          'cccot'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     !      l2_input_2d%cccot(:,:,inode)=l2var_dummy

     !cccot_pre
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cccot_pre'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cccot_pre(:,:,inode)=l2var_dummy

     !ann_phase_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'ann_phase_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ann_phase_uncertainty(:,:,inode)=l2var_dummy
     
     !cphcot
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cphcot'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cphcot(:,:,inode)=l2var_dummy
     
     !ref
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cer'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ref(:,:,inode)=l2var_dummy

     !ref_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cer_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%ref_error(:,:,inode)=l2var_dummy



!      if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2') ) then

!         !cloud_albedo1
!         call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
!              &          'cloud_albedo_in_channel_no_2'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
!         l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

!         !cloud_albedo2
!         call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
!              &          'cloud_albedo_in_channel_no_3'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
!         l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy
!      endif



     if ((csensor .eq. 'AATSR') .or. (csensor .eq. 'ATSR2')) then

        !cloud_albedo1
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
             &          'cloud_albedo_in_channel_no_2'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

        !cloud_albedo2
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
             &          'cloud_albedo_in_channel_no_3'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy
     endif


     if ((csensor .eq. 'AVHRR') .or. (csensor .eq. 'MODIS')) then

        !cloud_albedo1
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
             &          'cloud_albedo_in_channel_no_1'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

        !cloud_albedo2
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
             &          'cloud_albedo_in_channel_no_2'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy
     endif

     if (csensor .eq. 'SEVIRI') then
        !cloud_albedo1
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
            &            'cloud_albedo_in_channel_no_1'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo1(:,:,inode)=l2var_dummy

        !cloud_albedo2
        call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
            &            'cloud_albedo_in_channel_no_2'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
        l2_input_2d%cloud_albedo2(:,:,inode)=l2var_dummy
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
        cee_11um_uncertainty_input_string = 'cee_uncertainty_in_channel_no_6'

     endif

     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          cee_11um_input_string,l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cee(:,:,inode)=l2var_dummy
     
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          cee_11um_uncertainty_input_string,l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cee_error(:,:,inode)=l2var_dummy

     !cwp
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cwp'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cwp(:,:,inode)=l2var_dummy

     !cwp_uncertainty
     call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
          &          'cwp_uncertainty'//trim(adjustl(node_suffix)),l2var_dummy,dummy_unit,verbose)
     l2_input_2d%cwp_error(:,:,inode)=l2var_dummy

     !qcflag
     call nc_read_array_2d_short_to_short_orac(ncid,xdim1km,ydim1km, &
          &          'qcflag'//trim(adjustl(node_suffix)),l2var_dummy_stint,dummy_unit,verbose)
     l2_input_2d%qcflag(:,:,inode)=l2var_dummy_stint  

     !cty
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
          &          'cph'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%cty(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

     !cty_pre
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
          &          'cph_pre'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%cty_pre(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

     !ann_phase
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
          &          'ann_phase'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%ann_phase(:,:,inode)=real(l2var_dummy_byte,kind=sreal)
     
     !read illumination: to be used in different cloud covers call
     call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km,  &
          &          'illum'//trim(adjustl(node_suffix)),l2var_dummy_byte,dummy_unit,verbose)
     l2_input_2d%illum(:,:,inode)=l2var_dummy_byte

  enddo

  deallocate(l2var_dummy)
  deallocate(l2var_dummy_double)
  deallocate(l2var_dummy_byte)
  deallocate(l2var_dummy_stint)
