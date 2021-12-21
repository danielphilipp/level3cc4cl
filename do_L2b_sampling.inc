  ! Name: do_L2b_sampling.inc.inc
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
  !2015/04/23 OS+SteSta: code sourced out to include file
  !2015/07/05 CP added corrected cloud top height
  !2015/07/10 OS+SteSta: added cct_error
  !2015/10/27 CP: added STEMP
  !2015/12/16 OS: changes in qcflag required updates on tests on
  !               convergence and maximum cost; reactivated calculation of time
  !2016/02/18 OS: added CTP_corrected, CTT_corrected, CEE
  !2016/02/26 OS: bug fix in sampling constraints on cost and convergence
  !2016/02/26 OS: only sample CEE and CEE_unc if not fill_value
  !2016/03/04 OS: minor editing
  !2016/03/18 OS: float variables now scaled and written as 16 bit integers to NetCDF file
  !2017/06/22 OS: added ann phase, toa/boa, MODIS refl/BT
  !2017/06/29 OS: Adjusted scale factor for cph unc
  !2018/01/02 MST: Including fix for toa_swup fluxes in twilight conditions based on reflectance data
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known

 !look if this is a good pixel:
  if(l2_input_2d%cct(idim,jdim,inode) .ge. 0.0 .and. & !valid masking info
       & l2_input_2d%satzen(idim,jdim) .ge. 0.0) then

     ! MODIS Europe reflectances/BTs
     if (llocal) then

!        if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint .and. l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
!           l2_input_2d%reflectance06(idim,jdim,inode) .gt. 0. .and. l2_input_2d%reflectance06(idim,jdim,inode) .lt. 1.) then
!              !approximate TOA_SWUP fluxes in cloudy twi-light conditions as the cloud properties derived in these conditions are just the FG values, thus not reliable
!              l2_input_2d%toa_swup(idim,jdim,inode)=l2_input_2d%reflectance06(idim,jdim,inode)*100.*16. !*9.07416+114.467  !*15.
!        endif

        if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint .and. l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
           l2_input_2d%reflectance06(idim,jdim,inode) .gt. 0. .and. l2_input_2d%reflectance06(idim,jdim,inode) .lt. 1.) then
              !approximate TOA_SWUP fluxes in cloudy twi-light conditions as the cloud properties derived in these conditions are just the FG values, thus not reliable
              tmp_mst=l2_input_2d%toa_swup(idim,jdim,inode)
              l2_input_2d%toa_swup(idim,jdim,inode)=l2_input_2d%reflectance06(idim,jdim,inode)*100.*16. !*9.07416+114.467  !*15.
              l2_input_2d%boa_swdn(idim,jdim,inode)=l2_input_2d%boa_swdn(idim,jdim,inode)*min(5.,max(0.2,tmp_mst/l2_input_2d%toa_swup(idim,jdim,inode)))
        endif
                                                                                                                                                                          

        if (lasc) then

           if (l2_input_2d%satzen(idim,jdim) .lt. l2b_macro_micro_asc%satzen(lon_i,lat_j)) then

              if (l2_input_2d%reflectance06(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%reflectance06(lon_i,lat_j)=((l2_input_2d%reflectance06(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_asc%reflectance06(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%reflectance08(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%reflectance08(lon_i,lat_j)=((l2_input_2d%reflectance08(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_asc%reflectance08(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%reflectance16(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%reflectance16(lon_i,lat_j)=((l2_input_2d%reflectance16(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_asc%reflectance16(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT37(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%BT37(lon_i,lat_j)=((l2_input_2d%BT37(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_asc%BT37(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT11(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%BT11(lon_i,lat_j)=((l2_input_2d%BT11(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_asc%BT11(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT12(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_asc%BT12(lon_i,lat_j)=((l2_input_2d%BT12(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_asc%BT12(lon_i,lat_j)=real_fill_value
              endif

           endif ! satzen

        else ! if ldesc

           if (l2_input_2d%satzen(idim,jdim) .lt. l2b_macro_micro_desc%satzen(lon_i,lat_j)) then

              if (l2_input_2d%reflectance06(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%reflectance06(lon_i,lat_j)=((l2_input_2d%reflectance06(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_desc%reflectance06(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%reflectance08(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%reflectance08(lon_i,lat_j)=((l2_input_2d%reflectance08(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_desc%reflectance08(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%reflectance16(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%reflectance16(lon_i,lat_j)=((l2_input_2d%reflectance16(idim,jdim,inode)-offset_zero)/ scale_10000)
              else
                 l2b_macro_micro_desc%reflectance16(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT37(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%BT37(lon_i,lat_j)=((l2_input_2d%BT37(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_desc%BT37(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT11(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%BT11(lon_i,lat_j)=((l2_input_2d%BT11(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_desc%BT11(lon_i,lat_j)=real_fill_value
              endif
              if (l2_input_2d%BT12(idim,jdim,inode) .ne. real_fill_value ) then
                 l2b_macro_micro_desc%BT12(lon_i,lat_j)=((l2_input_2d%BT12(idim,jdim,inode)-offset_temp)/ scale_100)
              else
                 l2b_macro_micro_desc%BT12(lon_i,lat_j)=real_fill_value
              endif

           endif ! satzen

        endif ! if lasc

     endif ! MODIS Europe reflectances/BTs

     ! if this pixel is cloudy pick it only if l2 algo has converged with cost within bounds, otherwise try next pixel
!     if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5  .and. & ! cloudy
!          &   btest(l2_input_2d%qcflag(idim,jdim,inode),0) .or. & !convergence NOT reached
!          &   btest(l2_input_2d%qcflag(idim,jdim,inode),1) )  cycle   !cost NOT within bounds
                         
!     !if this is a daytime pixel and there is NO valid phase (clear,liq,ice= 0 1 2) then go to next pixel
!     if( l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .and. l2_input_2d%cty(idim,jdim,inode) .lt. filter_micro ) cycle

     !pixel is part of ascending node
     if(lasc) then

        if (l2_input_2d%satzen(idim,jdim) .lt. l2b_macro_micro_asc%satzen(lon_i,lat_j)) then

           if (l2_input_2d%cct_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cct_error(lon_i,lat_j)=((l2_input_2d%cct_error(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%cct_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%time(idim,jdim,inode) .ne. double_fill_value ) then
              l2b_macro_micro_asc%time(lon_i,lat_j)=nint(((l2_input_2d%time(idim,jdim,inode)-time_start) / time_scale ), lint)
           else
              l2b_macro_micro_asc%time(lon_i,lat_j)=long_int_fill_value
           endif
           if (l2_input_2d%satzen(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_asc%satzen(lon_i,lat_j)=l2_input_2d%satzen(idim,jdim)
           else
              l2b_macro_micro_asc%satzen(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%solzen(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_asc%solzen(lon_i,lat_j)=((l2_input_2d%solzen(idim,jdim) - offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%solzen(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%relazi(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_asc%relazi(lon_i,lat_j)=((l2_input_2d%relazi(idim,jdim) - offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%relazi(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif

         if(l2_input_2d%cct(idim,jdim,inode) .gt. 0.5) then

         if( .not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. & !convergence reached
           & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) ) then   !cost within bounds

           if (l2_input_2d%cth(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cth(lon_i,lat_j)=((l2_input_2d%cth(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_asc%cth(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cth_corrected(lon_i,lat_j)=((l2_input_2d%cth_corrected(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_asc%cth_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cth_error(lon_i,lat_j)=((l2_input_2d%cth_error(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_asc%cth_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cth_corrected_error(lon_i,lat_j)=((l2_input_2d%cth_corrected_error(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_asc%cth_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cot(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cot(lon_i,lat_j)=((l2_input_2d%cot(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%cot(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cot_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cot_error(lon_i,lat_j)=((l2_input_2d%cot_error(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%cot_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo1(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cloud_albedo1(lon_i,lat_j)=((l2_input_2d%cloud_albedo1(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_asc%cloud_albedo1(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo1_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cloud_albedo1_error(lon_i,lat_j)=((l2_input_2d%cloud_albedo1_error(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_asc%cloud_albedo1_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo2(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cloud_albedo2(lon_i,lat_j)=((l2_input_2d%cloud_albedo2(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_asc%cloud_albedo2(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo2_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cloud_albedo2_error(lon_i,lat_j)=((l2_input_2d%cloud_albedo2_error(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_asc%cloud_albedo2_error(lon_i,lat_j)=real_fill_value
           endif
!           if (l2_input_2d%cccot_pre(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_asc%cccot_pre(lon_i,lat_j)=((l2_input_2d%cccot_pre(idim,jdim,inode)- offset_zero) / scale_10000) 
!           else
!              l2b_macro_micro_asc%cccot_pre(lon_i,lat_j)=real_fill_value
!           endif
           if (l2_input_2d%ann_phase_uncertainty(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ann_phase_uncertainty(lon_i,lat_j)=((l2_input_2d%ann_phase_uncertainty(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%ann_phase_uncertainty(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cphcot(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%cphcot(lon_i,lat_j)=((l2_input_2d%cphcot(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_asc%cphcot(lon_i,lat_j)=real_fill_value
           endif                   
           if (l2_input_2d%ref(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ref(lon_i,lat_j)=((l2_input_2d%ref(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%ref(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%ref_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ref_error(lon_i,lat_j)=((l2_input_2d%ref_error(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_asc%ref_error(lon_i,lat_j)=real_fill_value
           endif
           ! ctp
           if (l2_input_2d%ctp(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctp(lon_i,lat_j)=((l2_input_2d%ctp(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_asc%ctp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctp_corrected(lon_i,lat_j)=((l2_input_2d%ctp_corrected(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_asc%ctp_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctp_error(lon_i,lat_j)=((l2_input_2d%ctp_error(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_asc%ctp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctp_corrected_error(lon_i,lat_j)=((l2_input_2d%ctp_corrected_error(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_asc%ctp_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           ! ctt , stemp
           if (l2_input_2d%ctt(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctt(lon_i,lat_j)=((l2_input_2d%ctt(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_asc%ctt(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctt_corrected(lon_i,lat_j)=((l2_input_2d%ctt_corrected(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_asc%ctt_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctt_error(lon_i,lat_j)=((l2_input_2d%ctt_error(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_asc%ctt_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_asc%ctt_corrected_error(lon_i,lat_j)=((l2_input_2d%ctt_corrected_error(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_asc%ctt_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
!           if (l2_input_2d%stemp(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_asc%stemp(lon_i,lat_j)=((l2_input_2d%stemp(idim,jdim,inode)-offset_temp)/ scale_100)
!           else
!              l2b_macro_micro_asc%stemp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
!           endif
!           if (l2_input_2d%stemp_error(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_asc%stemp_error(lon_i,lat_j)=((l2_input_2d%stemp_error(idim,jdim,inode)-offset_temp)/ scale_100)
!           else
!              l2b_macro_micro_asc%stemp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
!           endif
           if ((l2_input_2d%cee(idim,jdim,inode) .ne. real_fill_value) &
                .and. (l2_input_2d%cee_error(idim,jdim,inode) .ne. real_fill_value)) then
              l2b_macro_micro_asc%cee(lon_i,lat_j)=((l2_input_2d%cee(idim,jdim,inode)- offset_zero) / scale_10000)
              l2b_macro_micro_asc%cee_error(lon_i,lat_j)=min(((l2_input_2d%cee_error(idim,jdim,inode)- offset_zero) / scale_10000), 1. / scale_10000)
           else
              l2b_macro_micro_asc%cee(lon_i,lat_j)=real_fill_value
              l2b_macro_micro_asc%cee_error(lon_i,lat_j)=real_fill_value
           endif

           !no scaling
           l2b_macro_micro_asc%cwp(lon_i,lat_j)=l2_input_2d%cwp(idim,jdim,inode)
           l2b_macro_micro_asc%cwp_error(lon_i,lat_j)=l2_input_2d%cwp_error(idim,jdim,inode)
           !no scaling necessary
!           l2b_macro_micro_asc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
!           l2b_macro_micro_asc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
!           l2b_macro_micro_asc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
!           l2b_macro_micro_asc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)
!           l2b_macro_micro_asc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)

        endif ! qcflag
         
         endif ! if cloudy

         !the following are sampled in clear-sky and cloudy conditions

          l2b_macro_micro_asc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
          l2b_macro_micro_asc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
          l2b_macro_micro_asc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
          l2b_macro_micro_asc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)
          l2b_macro_micro_asc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)
                                            
           l2b_macro_micro_asc%illum(lon_i,lat_j)=l2_input_2d%illum(idim,jdim,inode)
          if (l2_input_2d%cccot_pre(idim,jdim,inode) .ne. real_fill_value ) then
             l2b_macro_micro_asc%cccot_pre(lon_i,lat_j)=((l2_input_2d%cccot_pre(idim,jdim,inode)- offset_zero) / scale_10000)
          else
             l2b_macro_micro_asc%cccot_pre(lon_i,lat_j)=real_fill_value
          endif
                                                             
          if (l2_input_2d%stemp(idim,jdim,inode) .ne. real_fill_value ) then
             l2b_macro_micro_asc%stemp(lon_i,lat_j)=((l2_input_2d%stemp(idim,jdim,inode)-offset_temp)/ scale_100)
          else
             l2b_macro_micro_asc%stemp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
          endif
          if (l2_input_2d%stemp_error(idim,jdim,inode) .ne. real_fill_value ) then
             l2b_macro_micro_asc%stemp_error(lon_i,lat_j)=((l2_input_2d%stemp_error(idim,jdim,inode)-offset_temp)/ scale_100)
          else
             l2b_macro_micro_asc%stemp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
          endif
                                                                                                                         

           if ((l2_input_2d%toa_lwup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_lwup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swdn(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_lwup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_lwdn_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn(idim,jdim,inode) .ne. real_fill_value)) then
               
              l2b_macro_micro_asc%toa_lwup(lon_i,lat_j)=((l2_input_2d%toa_lwup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%toa_lwup_clr(lon_i,lat_j)=((l2_input_2d%toa_lwup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%toa_swup(lon_i,lat_j)=((l2_input_2d%toa_swup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%toa_swup_clr(lon_i,lat_j)=((l2_input_2d%toa_swup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%toa_swdn(lon_i,lat_j)=((l2_input_2d%toa_swdn(idim,jdim,inode)- offset_zero) / 0.1)                                                   

              l2b_macro_micro_asc%boa_lwup(lon_i,lat_j)=((l2_input_2d%boa_lwup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_lwup_clr(lon_i,lat_j)=((l2_input_2d%boa_lwup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_swup(lon_i,lat_j)=((l2_input_2d%boa_swup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_swup_clr(lon_i,lat_j)=((l2_input_2d%boa_swup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_lwdn(lon_i,lat_j)=((l2_input_2d%boa_lwdn(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_lwdn_clr(lon_i,lat_j)=((l2_input_2d%boa_lwdn_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_swdn(lon_i,lat_j)=((l2_input_2d%boa_swdn(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_swdn_clr(lon_i,lat_j)=((l2_input_2d%boa_swdn_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_par_dif(lon_i,lat_j)=((l2_input_2d%boa_par_dif(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_asc%boa_par_tot(lon_i,lat_j)=((l2_input_2d%boa_par_tot(idim,jdim,inode)- offset_zero) / 0.1)
                      
           endif

!           !no scaling
!           l2b_macro_micro_asc%cwp(lon_i,lat_j)=l2_input_2d%cwp(idim,jdim,inode)
!           l2b_macro_micro_asc%cwp_error(lon_i,lat_j)=l2_input_2d%cwp_error(idim,jdim,inode)
!           !no scaling necessary
!           l2b_macro_micro_asc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
!           l2b_macro_micro_asc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
!           l2b_macro_micro_asc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
!           l2b_macro_micro_asc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)
!           l2b_macro_micro_asc%illum(lon_i,lat_j)=l2_input_2d%illum(idim,jdim,inode)
!           l2b_macro_micro_asc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)
        endif
        !increment counter to keep track of number of pixels which were considered for the sample
        l2b_macro_micro_asc%counter(lon_i,lat_j)=l2b_macro_micro_asc%counter(lon_i,lat_j)+1
        !descending
     else
        if (l2_input_2d%satzen(idim,jdim) .lt. l2b_macro_micro_desc%satzen(lon_i,lat_j)) then
           if (l2_input_2d%cct_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cct_error(lon_i,lat_j)=((l2_input_2d%cct_error(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%cct_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%time(idim,jdim,inode) .ne. double_fill_value ) then
              l2b_macro_micro_desc%time(lon_i,lat_j)=nint(((l2_input_2d%time(idim,jdim,inode)-time_start) / time_scale ), lint)
           else
              l2b_macro_micro_desc%time(lon_i,lat_j)=long_int_fill_value
           endif
           if (l2_input_2d%satzen(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_desc%satzen(lon_i,lat_j)=l2_input_2d%satzen(idim,jdim)
           else
              l2b_macro_micro_desc%satzen(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%solzen(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_desc%solzen(lon_i,lat_j)=((l2_input_2d%solzen(idim,jdim) - offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%solzen(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%relazi(idim,jdim) .ne. real_fill_value ) then
              l2b_macro_micro_desc%relazi(lon_i,lat_j)=((l2_input_2d%relazi(idim,jdim) - offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%relazi(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif

         if(l2_input_2d%cct(idim,jdim,inode) .gt. 0.5) then

          if( .not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. & !convergence reached
             & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) ) then   !cost within bounds

           if (l2_input_2d%cth(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cth(lon_i,lat_j)=((l2_input_2d%cth(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_desc%cth(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cth_corrected(lon_i,lat_j)=((l2_input_2d%cth_corrected(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_desc%cth_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cth_error(lon_i,lat_j)=((l2_input_2d%cth_error(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_desc%cth_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cth_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cth_corrected_error(lon_i,lat_j)=((l2_input_2d%cth_corrected_error(idim,jdim,inode) - offset_zero) / scale_1000)
           else
              l2b_macro_micro_desc%cth_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%cot(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cot(lon_i,lat_j)=((l2_input_2d%cot(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%cot(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cot_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cot_error(lon_i,lat_j)=((l2_input_2d%cot_error(idim,jdim,inode) - offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%cot_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo1(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cloud_albedo1(lon_i,lat_j)=((l2_input_2d%cloud_albedo1(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cloud_albedo1(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo1_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cloud_albedo1_error(lon_i,lat_j)=((l2_input_2d%cloud_albedo1_error(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cloud_albedo1_error(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo2(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cloud_albedo2(lon_i,lat_j)=((l2_input_2d%cloud_albedo2(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cloud_albedo2(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cloud_albedo2_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cloud_albedo2_error(lon_i,lat_j)=((l2_input_2d%cloud_albedo2_error(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cloud_albedo2_error(lon_i,lat_j)=real_fill_value
           endif
!           if (l2_input_2d%cccot_pre(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_desc%cccot_pre(lon_i,lat_j)=((l2_input_2d%cccot_pre(idim,jdim,inode)- offset_zero) / scale_10000)
!           else
!              l2b_macro_micro_desc%cccot_pre(lon_i,lat_j)=real_fill_value
!           endif
           if (l2_input_2d%ann_phase_uncertainty(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ann_phase_uncertainty(lon_i,lat_j)=((l2_input_2d%ann_phase_uncertainty(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%ann_phase_uncertainty(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%cphcot(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cphcot(lon_i,lat_j)=((l2_input_2d%cphcot(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cphcot(lon_i,lat_j)=real_fill_value
           endif           
           if (l2_input_2d%ref(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ref(lon_i,lat_j)=((l2_input_2d%ref(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%ref(lon_i,lat_j)=real_fill_value
           endif
           if (l2_input_2d%ref_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ref_error(lon_i,lat_j)=((l2_input_2d%ref_error(idim,jdim,inode)- offset_zero) / scale_100)
           else
              l2b_macro_micro_desc%ref_error(lon_i,lat_j)=real_fill_value
           endif
           ! ctp
           if (l2_input_2d%ctp(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctp(lon_i,lat_j)=((l2_input_2d%ctp(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_desc%ctp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctp_corrected(lon_i,lat_j)=((l2_input_2d%ctp_corrected(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_desc%ctp_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctp_error(lon_i,lat_j)=((l2_input_2d%ctp_error(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_desc%ctp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctp_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctp_corrected_error(lon_i,lat_j)=((l2_input_2d%ctp_corrected_error(idim,jdim,inode)-offset_ctp)/ scale_ctp)
           else
              l2b_macro_micro_desc%ctp_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           ! ctt , stemp
           if (l2_input_2d%ctt(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctt(lon_i,lat_j)=((l2_input_2d%ctt(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%ctt(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_corrected(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctt_corrected(lon_i,lat_j)=((l2_input_2d%ctt_corrected(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%ctt_corrected(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctt_error(lon_i,lat_j)=((l2_input_2d%ctt_error(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%ctt_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%ctt_corrected_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%ctt_corrected_error(lon_i,lat_j)=((l2_input_2d%ctt_corrected_error(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%ctt_corrected_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
!           if (l2_input_2d%stemp(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_desc%stemp(lon_i,lat_j)=((l2_input_2d%stemp(idim,jdim,inode)-offset_temp)/ scale_100)
!           else
!              l2b_macro_micro_desc%stemp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
!           endif
!           if (l2_input_2d%stemp_error(idim,jdim,inode) .ne. real_fill_value ) then
!              l2b_macro_micro_desc%stemp_error(lon_i,lat_j)=((l2_input_2d%stemp_error(idim,jdim,inode)-offset_temp)/ scale_100)
!           else
!              l2b_macro_micro_desc%stemp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
!           endif
           if ((l2_input_2d%cee(idim,jdim,inode) .ne. real_fill_value) &
                .and. (l2_input_2d%cee_error(idim,jdim,inode) .ne. real_fill_value)) then
              l2b_macro_micro_desc%cee(lon_i,lat_j)=((l2_input_2d%cee(idim,jdim,inode)- offset_zero) / scale_10000)
              l2b_macro_micro_desc%cee_error(lon_i,lat_j)=min(((l2_input_2d%cee_error(idim,jdim,inode)- offset_zero) / scale_10000), 1. / scale_10000)
           else
              l2b_macro_micro_desc%cee(lon_i,lat_j)=real_fill_value
              l2b_macro_micro_desc%cee_error(lon_i,lat_j)=real_fill_value
           endif

           !no scaling
           l2b_macro_micro_desc%cwp(lon_i,lat_j)=l2_input_2d%cwp(idim,jdim,inode)
           l2b_macro_micro_desc%cwp_error(lon_i,lat_j)=l2_input_2d%cwp_error(idim,jdim,inode)
           !no scaling necessary
!           l2b_macro_micro_desc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
!           l2b_macro_micro_desc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
!           l2b_macro_micro_desc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
!           l2b_macro_micro_desc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)
!           l2b_macro_micro_desc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)

          endif !qcflag        

         endif ! if cloudy

         !the following a sampled in clear-sky and cloudy conditions

           l2b_macro_micro_desc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
           l2b_macro_micro_desc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
           l2b_macro_micro_desc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
           l2b_macro_micro_desc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)
           l2b_macro_micro_desc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)
                                            
           l2b_macro_micro_desc%illum(lon_i,lat_j)=l2_input_2d%illum(idim,jdim,inode)
           if (l2_input_2d%cccot_pre(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%cccot_pre(lon_i,lat_j)=((l2_input_2d%cccot_pre(idim,jdim,inode)- offset_zero) / scale_10000)
           else
              l2b_macro_micro_desc%cccot_pre(lon_i,lat_j)=real_fill_value
           endif
                                                                        
           if (l2_input_2d%stemp(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%stemp(lon_i,lat_j)=((l2_input_2d%stemp(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%stemp(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif
           if (l2_input_2d%stemp_error(idim,jdim,inode) .ne. real_fill_value ) then
              l2b_macro_micro_desc%stemp_error(lon_i,lat_j)=((l2_input_2d%stemp_error(idim,jdim,inode)-offset_temp)/ scale_100)
           else
              l2b_macro_micro_desc%stemp_error(lon_i,lat_j)=real(stint_fill_value, kind=sreal)
           endif

           if ((l2_input_2d%toa_lwup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_lwup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%toa_swdn(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_lwup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swup(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_lwdn_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn_clr(idim,jdim,inode) .ne. real_fill_value) &
               .and. (l2_input_2d%boa_swdn(idim,jdim,inode) .ne. real_fill_value)) then
               
              l2b_macro_micro_desc%toa_lwup(lon_i,lat_j)=((l2_input_2d%toa_lwup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%toa_lwup_clr(lon_i,lat_j)=((l2_input_2d%toa_lwup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%toa_swup(lon_i,lat_j)=((l2_input_2d%toa_swup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%toa_swup_clr(lon_i,lat_j)=((l2_input_2d%toa_swup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%toa_swdn(lon_i,lat_j)=((l2_input_2d%toa_swdn(idim,jdim,inode)- offset_zero) / 0.1)

              l2b_macro_micro_desc%boa_lwup(lon_i,lat_j)=((l2_input_2d%boa_lwup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_lwup_clr(lon_i,lat_j)=((l2_input_2d%boa_lwup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_swup(lon_i,lat_j)=((l2_input_2d%boa_swup(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_swup_clr(lon_i,lat_j)=((l2_input_2d%boa_swup_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_lwdn(lon_i,lat_j)=((l2_input_2d%boa_lwdn(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_lwdn_clr(lon_i,lat_j)=((l2_input_2d%boa_lwdn_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_swdn(lon_i,lat_j)=((l2_input_2d%boa_swdn(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_swdn_clr(lon_i,lat_j)=((l2_input_2d%boa_swdn_clr(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_par_dif(lon_i,lat_j)=((l2_input_2d%boa_par_dif(idim,jdim,inode)- offset_zero) / 0.1)
              l2b_macro_micro_desc%boa_par_tot(lon_i,lat_j)=((l2_input_2d%boa_par_tot(idim,jdim,inode)- offset_zero) / 0.1)
                                                                                                                                           
           endif

!           !no scaling
!           l2b_macro_micro_desc%cwp(lon_i,lat_j)=l2_input_2d%cwp(idim,jdim,inode)
!           l2b_macro_micro_desc%cwp_error(lon_i,lat_j)=l2_input_2d%cwp_error(idim,jdim,inode)
!           !no scaling necessary
!           l2b_macro_micro_desc%qcflag(lon_i,lat_j)=l2_input_2d%qcflag(idim,jdim,inode)
!           l2b_macro_micro_desc%cty(lon_i,lat_j)=nint(l2_input_2d%cty(idim,jdim,inode),kind=sint)
!           l2b_macro_micro_desc%cld_type(lon_i,lat_j)=l2_input_2d%cld_type(idim,jdim,inode)
!           l2b_macro_micro_desc%ann_phase(lon_i,lat_j)=l2_input_2d%ann_phase(idim,jdim,inode)           
!           l2b_macro_micro_desc%illum(lon_i,lat_j)=l2_input_2d%illum(idim,jdim,inode)
!           l2b_macro_micro_desc%cct(lon_i,lat_j)=nint(l2_input_2d%cct(idim,jdim,inode),kind=sint)
        endif

        !increment counter to keep track of number of pixels which were considered for the sample
        l2b_macro_micro_desc%counter(lon_i,lat_j)=l2b_macro_micro_desc%counter(lon_i,lat_j)+1

     endif
  endif
