! Name: read_L2_input_L3d.inc
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
  !2015-10-27 CP add stemp
  !2015-12-16 OS replaced wo with verbose
  !2016-02-18 OS added CEE, CTP_corrected, CTT_corrected
  !2019-09-06 Daniel Philipp added SEVIRI support
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  !reading of the l3 daily means
  
  do idim=1,ndim

     if(idim .eq. 1 ) name=trim(adjustl(cdim1))
     if(idim .eq. 2 ) name=trim(adjustl(cdim2))

     call nc_dim_id(ncid,name,dimids(idim),verbose)
     call nc_dim_length(ncid,dname(idim),dimids(idim),dummyint,verbose)
     dimlength(idim)=dummyint
     if(idim .eq. 1 ) ydim1km=int(dimlength(1),kind=lint)
     if(idim .eq. 2 ) xdim1km=int(dimlength(2),kind=lint)

  enddo

  allocate(lat_1km_raw_2d(xdim1km,ydim1km))
  lat_1km_raw_2d=real_fill_value
  allocate(lon_1km_raw_2d(xdim1km,ydim1km))
  lon_1km_raw_2d=real_fill_value

  call nc_read_lon_grid_float(ncid,xdim1km,ydim1km, &
       &                 'lon',lon_1km_raw_2d,lon_unit,verbose)

  call nc_read_lat_grid_float(ncid,xdim1km,ydim1km, &
       &                 'lat',lat_1km_raw_2d,lat_unit,verbose)

  if(ind .eq.1) then

     call set_l3d_macro_input_struct(l3d_macro_input,xdim1km,ydim1km,lastfile,calgo)
     call set_l3d_micro_input_struct(l3d_micro_input,xdim1km,ydim1km,lastfile)

  endif

  allocate(l2var_dummy(xdim1km,ydim1km))
  l2var_dummy=real_fill_value

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
    cee_11um_input_string              = 'cee_in_channel_no_9'
    cee_11um_uncertainty_input_string  = 'cee_uncertainty_in_channel_no_9'

  endif


  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 cee_11um_input_string,l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%cee_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'ctt',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%ctt_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'ctt_corrected',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%ctt_corrected_mean(:,:,ind)=l2var_dummy


  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'stemp',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%stemp_mean(:,:,ind)=l2var_dummy


  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cth',l2var_dummy,dummy_unit,verbose)

  l3d_macro_input%cth_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cth_corrected',l2var_dummy,dummy_unit,verbose)

  l3d_macro_input%cth_corrected_mean(:,:,ind)=l2var_dummy
 
  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cc_total',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%cct_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'ctp',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%ctp_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'ctp_corrected',l2var_dummy,dummy_unit,verbose)
  l3d_macro_input%ctp_corrected_mean(:,:,ind)=l2var_dummy

  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cot',l2var_dummy,dummy_unit,verbose)
  l3d_micro_input%cot_mean(:,:,ind)=l2var_dummy
  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'ref',l2var_dummy,dummy_unit,verbose)
  l3d_micro_input%ref_mean(:,:,ind)=l2var_dummy
  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cwp',l2var_dummy,dummy_unit,verbose)
  l3d_micro_input%cwp_mean(:,:,ind)=l2var_dummy
  call nc_read_array_2d_floatl3d(ncid,xdim1km,ydim1km, &
       &                 'cty',l2var_dummy,dummy_unit,verbose)
  l3d_micro_input%cty_mean(:,:,ind)=l2var_dummy

  !perform averaging
  do idim=1,xdim1km
     do jdim=1,ydim1km

        !macrophysical variables
        if(l3d_macro_input%ctt_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_macro_input%cth_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_macro_input%ctp_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_macro_input%cct_mean(idim,jdim,ind) .ge. filter_micro) then

           l3d_macro%ctt_mean(idim,jdim)=l3d_macro%ctt_mean(idim,jdim)+&
                & l3d_macro_input%ctt_mean(idim,jdim,ind)

           l3d_macro%ctt_corrected_mean(idim,jdim)=l3d_macro%ctt_corrected_mean(idim,jdim)+&
                & l3d_macro_input%ctt_corrected_mean(idim,jdim,ind)

           l3d_macro%stemp_mean(idim,jdim)=l3d_macro%stemp_mean(idim,jdim)+&
                & l3d_macro_input%stemp_mean(idim,jdim,ind)


           l3d_macro%cth_mean(idim,jdim)=l3d_macro%cth_mean(idim,jdim)+&
                & l3d_macro_input%cth_mean(idim,jdim,ind)

           l3d_macro%cth_corrected_mean(idim,jdim)=l3d_macro%cth_corrected_mean(idim,jdim)+&
                & l3d_macro_input%cth_corrected_mean(idim,jdim,ind)

           l3d_macro%ctp_mean(idim,jdim)=l3d_macro%ctp_mean(idim,jdim)+&
                & l3d_macro_input%ctp_mean(idim,jdim,ind)

           l3d_macro%ctp_corrected_mean(idim,jdim)=l3d_macro%ctp_corrected_mean(idim,jdim)+&
                & l3d_macro_input%ctp_corrected_mean(idim,jdim,ind)

           l3d_macro%cct_mean(idim,jdim)=l3d_macro%cct_mean(idim,jdim)+&
                & l3d_macro_input%cct_mean(idim,jdim,ind)

           l3d_macro%points(idim,jdim)=l3d_macro%points(idim,jdim)+1

        endif

        !microphysical variables
        if(l3d_micro_input%cot_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_micro_input%ref_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_micro_input%cwp_mean(idim,jdim,ind) .ge. filter_micro .and. &
             & l3d_micro_input%cty_mean(idim,jdim,ind) .gt. filter_micro ) then

           l3d_micro%cot_mean(idim,jdim)=l3d_micro%cot_mean(idim,jdim)+&
                & l3d_micro_input%cot_mean(idim,jdim,ind)

           l3d_micro%cloud_albedo1_mean(idim,jdim)=l3d_micro%cloud_albedo1_mean(idim,jdim)+&
                & l3d_micro_input%cloud_albedo1_mean(idim,jdim,ind)
           l3d_micro%cloud_albedo2_mean(idim,jdim)=l3d_micro%cloud_albedo2_mean(idim,jdim)+&
                & l3d_micro_input%cloud_albedo2_mean(idim,jdim,ind)



           l3d_micro%ref_mean(idim,jdim)=l3d_micro%ref_mean(idim,jdim)+&
                & l3d_micro_input%ref_mean(idim,jdim,ind)
           l3d_micro%cwp_mean(idim,jdim)=l3d_micro%cwp_mean(idim,jdim)+&
                & l3d_micro_input%cwp_mean(idim,jdim,ind)
           l3d_micro%cty_mean(idim,jdim)=l3d_micro%cty_mean(idim,jdim)+&
                & l3d_micro_input%cty_mean(idim,jdim,ind)
           l3d_micro%points(idim,jdim)=l3d_micro%points(idim,jdim)+1

        endif

     enddo
  enddo

  deallocate(lat_1km_raw_2d)
  deallocate(lon_1km_raw_2d)
  deallocate(l2var_dummy)
