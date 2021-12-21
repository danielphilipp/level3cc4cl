  ! Name: read_L2_input_FAME-C.inc
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
  ! 2015/12/17 OS added file header; replaced wo with verbose
  ! 2016/02/18 OS addedd CTP_corrected

  ! $Id$
  !
  ! Bugs:
  !
  !none known


  !fudge the number of nodes, in normal operation the L2 file is read and 
  !the node dimension colapses to one entry, see also ORAC_L2B branch
  nnodes=1
  inode=nnodes

  name='across_track'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(1),verbose)
  call nc_dim_length(ncid,name,dimids(1),dummyint,verbose)
  dimlength(1)=dummyint
  xdim1km=int(dimlength(1),kind=lint)

  name='along_track'
  call nc_dim_id(ncid,trim(adjustl(name)),dimids(2),verbose)
  call nc_dim_length(ncid,name,dimids(2),dummyint,verbose)
  dimlength(2)=dummyint
  ydim1km=int(dimlength(2),kind=lint)

  allocate(lat_1km_raw_2d(xdim1km,ydim1km))
  lat_1km_raw_2d=real_fill_value
  allocate(lon_1km_raw_2d(xdim1km,ydim1km))
  lon_1km_raw_2d=real_fill_value
  allocate(sensorzenith_1km_raw_2d(xdim1km,ydim1km))
  sensorzenith_1km_raw_2d=real_fill_value

  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &                 'lon',lon_1km_raw_2d,lon_unit,verbose)

  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'lat',lat_1km_raw_2d,lat_unit,verbose)


  call nc_read_array_2d_float_orac(ncid,xdim1km,ydim1km, &
       &          'satellite_zenith_view_aatsr_nadir',sensorzenith_1km_raw_2d,dummy_unit,verbose)

  !Define L2 input structure and read then the variables
  call set_l2_input_struct_2d(l2_input_2d,xdim1km,ydim1km,nnodes,&
       & nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features)

!  l2_input_2d%vname=varname_1km
!  l2_input_2d%ename=''!varname_errors_1km
  l2_input_2d%lon=lon_1km_raw_2d
  l2_input_2d%lat=lat_1km_raw_2d
  l2_input_2d%satzen=sensorzenith_1km_raw_2d

  deallocate(lat_1km_raw_2d)
  deallocate(lon_1km_raw_2d)
  deallocate(sensorzenith_1km_raw_2d)

  allocate(l2var_dummy(xdim1km,ydim1km))
  l2var_dummy=real_fill_value

  allocate(l2var_dummy_double(xdim1km,ydim1km))
  l2var_dummy_double=double_fill_value

  allocate(l2var_dummy_byte(xdim1km,ydim1km))
  l2var_dummy_byte=short_int_fill_value

  allocate(l2var_dummy_stint(xdim1km,ydim1km))
  l2var_dummy_stint=nint_fill_value

  !variables follow:
  !time
  call nc_read_array_2d_double_orac(ncid,xdim1km,ydim1km,"time",l2var_dummy_double,verbose)
  l2_input_2d%time(:,:,inode)=l2var_dummy_double

  !ctt           
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctt(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctt_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctt_error(:,:,inode)=l2var_dummy


  !cth
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cth(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cth_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cth_error(:,:,inode)=l2var_dummy

  !ctp
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctp(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctp_error(:,:,inode)=l2var_dummy

  !ctp_corrected
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_corrected',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctp_corrected(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ctp_corrected_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ctp_corrected_error(:,:,inode)=l2var_dummy

  !cct
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cc_total',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cct(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cc_total_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cct_error(:,:,inode)=l2var_dummy

  !cot
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cot',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cot(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cot_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cot_error(:,:,inode)=l2var_dummy


  !ref
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ref',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ref(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'ref_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%ref_error(:,:,inode)=l2var_dummy


  !changed the scaling factor not type of number in FAME-C
  !cwp
  !              call nc_read_array_2d_long_orac(ncid,xdim1km,ydim1km, &
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cwp',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cwp(:,:,inode)=l2var_dummy

  !              call nc_read_array_2d_long_orac(ncid,xdim1km,ydim1km, &
  call nc_read_array_2d_short_orac(ncid,xdim1km,ydim1km, &
       &          'cwp_uncertainty',l2var_dummy,dummy_unit,verbose)            
  l2_input_2d%cwp_error(:,:,inode)=l2var_dummy

  call nc_read_array_2d_short_to_short_orac(ncid,xdim1km,ydim1km, &
       &          'qcflag',l2var_dummy_stint,dummy_unit,verbose)            
  l2_input_2d%qcflag(:,:,inode)=l2var_dummy_stint


  !cty
  call nc_read_array_2d_byte_orac(ncid,xdim1km,ydim1km, &
       &          'phase',l2var_dummy_byte,dummy_unit,verbose)            
  l2_input_2d%cty(:,:,inode)=real(l2var_dummy_byte,kind=sreal)

  deallocate(l2var_dummy)
  deallocate(l2var_dummy_double)
  deallocate(l2var_dummy_byte)
  deallocate(l2var_dummy_stint)
