  ! Name: L3abc_averaging.inc
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
  !2015/04/23 OS: some minor edits
  !2015-03-30 CP added uncertainty information
  !2015-07-07 CP added corrected cloud top height
  !2015/07/10 OS: removed some commented code; calculate new uncertainty and
  !               cph_day
  !2015/08/31 OS: some indentation changes
  !2015/10/31 CP: add stemp
  !2015/12/16 OS: updated use of some pointer variables;
  !               activated calculation of cloud albedo statistics;
  !               added cot/ref/albedo uncertainties (prop and corr)
  !2015/02/18 OS: added variables CEE, CTT_corrected, CTP_corrected
  !2015/02/26 OS: added counter for CEE
  !2017/06/22 OS: added stemp_clear, toa/boa
  !2017/06/29 OS: Added cph alias cty
  !
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known

  l3_macro%cct_points_clear  = l3_macro%points - l3_macro%cct_points_raw

  !call omp_set_num_threads(2)
  !OMP PARALLEL DO  PRIVATE(idim,jdim)
  do idim=1,ncols
     do jdim=1,nrows

        !First produce cloud fractions

        !write(*,*)'l3abc' ,l3_macro%cct_points_raw(idim,jdim), l3_macro%cct_points_clear_raw(idim,jdim),min_num_values
        if (proc_toa) then

!           l3_macro%boa_swdn_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_swdn_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swdn_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swup_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swup_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_swup_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_swup_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_par_dif_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_par_tot_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_lwdn_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_lwdn_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_lwup_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_lwup_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_lwup_mean(idim,jdim)=real_fill_value
!           l3_macro%boa_lwup_clr_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swup_hig_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_lwup_hig_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swup_mid_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_lwup_mid_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_swup_low_mean(idim,jdim)=real_fill_value
!           l3_macro%toa_lwup_low_mean(idim,jdim)=real_fill_value
             
        if(l3_macro%rad_points_sw(idim,jdim) .gt. 0) then
              l3_macro%boa_swdn_mean(idim,jdim)=&
                   & l3_macro%boa_swdn_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%boa_swdn_clr_mean(idim,jdim)=&
                   & l3_macro%boa_swdn_clr_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%toa_swdn_mean(idim,jdim)=&
                   & l3_macro%toa_swdn_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%toa_swup_mean(idim,jdim)=&
                   & l3_macro%toa_swup_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%toa_swup_clr_mean(idim,jdim)=&
                   & l3_macro%toa_swup_clr_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%boa_swup_mean(idim,jdim)=&
                   & l3_macro%boa_swup_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%boa_swup_clr_mean(idim,jdim)=&
                   & l3_macro%boa_swup_clr_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%boa_par_dif_mean(idim,jdim)=&
                   & l3_macro%boa_par_dif_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
              l3_macro%boa_par_tot_mean(idim,jdim)=&
                   & l3_macro%boa_par_tot_mean(idim,jdim)/l3_macro%rad_points_sw(idim,jdim)
           elseif(l3_macro%rad_points_sw(idim,jdim) .eq. 0 .and. l3_macro%rad_points_lw(idim,jdim) .gt. 0) then
              l3_macro%boa_swdn_mean(idim,jdim)=0.
              l3_macro%boa_swdn_clr_mean(idim,jdim)=0.
              l3_macro%toa_swdn_mean(idim,jdim)=0.
              l3_macro%toa_swup_mean(idim,jdim)=0.
              l3_macro%toa_swup_clr_mean(idim,jdim)=0.
              l3_macro%boa_swup_mean(idim,jdim)=0.
              l3_macro%boa_swup_clr_mean(idim,jdim)=0.
              l3_macro%boa_par_dif_mean(idim,jdim)=0.
              l3_macro%boa_par_tot_mean(idim,jdim)=0.
           else
              l3_macro%boa_swdn_mean(idim,jdim)=real_fill_value
              l3_macro%boa_swdn_clr_mean(idim,jdim)=real_fill_value
              l3_macro%toa_swdn_mean(idim,jdim)=real_fill_value
              l3_macro%toa_swup_mean(idim,jdim)=real_fill_value
              l3_macro%toa_swup_clr_mean(idim,jdim)=real_fill_value
              l3_macro%boa_swup_mean(idim,jdim)=real_fill_value
              l3_macro%boa_swup_clr_mean(idim,jdim)=real_fill_value
              l3_macro%boa_par_dif_mean(idim,jdim)=real_fill_value
              l3_macro%boa_par_tot_mean(idim,jdim)=real_fill_value
           endif
                   

           if(l3_macro%rad_points_lw(idim,jdim) .gt. 0) then
              l3_macro%toa_lwup_mean(idim,jdim)=&
                   & l3_macro%toa_lwup_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
              l3_macro%toa_lwup_clr_mean(idim,jdim)=&
                   & l3_macro%toa_lwup_clr_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
              l3_macro%boa_lwup_mean(idim,jdim)=&
                   & l3_macro%boa_lwup_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
              l3_macro%boa_lwup_clr_mean(idim,jdim)=&
                   & l3_macro%boa_lwup_clr_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
              l3_macro%boa_lwdn_mean(idim,jdim)=&
                   & l3_macro%boa_lwdn_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
              l3_macro%boa_lwdn_clr_mean(idim,jdim)=&
                   & l3_macro%boa_lwdn_clr_mean(idim,jdim)/l3_macro%rad_points_lw(idim,jdim)
           elseif(l3_macro%rad_points_lw(idim,jdim) .eq. 0 .and. l3_macro%points(idim,jdim) .gt. 0) then
              l3_macro%boa_lwdn_mean(idim,jdim)=0.
              l3_macro%boa_lwdn_clr_mean(idim,jdim)=0.
              l3_macro%toa_lwup_mean(idim,jdim)=0.
              l3_macro%toa_lwup_clr_mean(idim,jdim)=0.
              l3_macro%boa_lwup_mean(idim,jdim)=0.
              l3_macro%boa_lwup_clr_mean(idim,jdim)=0.
           else
              l3_macro%boa_lwdn_mean(idim,jdim)=real_fill_value
              l3_macro%boa_lwdn_clr_mean(idim,jdim)=real_fill_value
              l3_macro%toa_lwup_mean(idim,jdim)=real_fill_value
              l3_macro%toa_lwup_clr_mean(idim,jdim)=real_fill_value
              l3_macro%boa_lwup_mean(idim,jdim)=real_fill_value
              l3_macro%boa_lwup_clr_mean(idim,jdim)=real_fill_value                                                                         
           endif

           if(l3_macro%rad_points_sw_hig(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_hig_mean(idim,jdim)=l3_macro%toa_swup_hig_mean(idim,jdim)/l3_macro%rad_points_sw_hig(idim,jdim)
           elseif(l3_macro%rad_points_sw_hig(idim,jdim) .eq. 0 .and. l3_macro%rad_points_lw_hig(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_hig_mean(idim,jdim)=0.
           else
             l3_macro%toa_swup_hig_mean(idim,jdim)=real_fill_value
           endif
           if(l3_macro%rad_points_lw_hig(idim,jdim) .gt. 0) then
             l3_macro%toa_lwup_hig_mean(idim,jdim)=l3_macro%toa_lwup_hig_mean(idim,jdim)/l3_macro%rad_points_lw_hig(idim,jdim)
           else
             l3_macro%toa_lwup_hig_mean(idim,jdim)=real_fill_value
           endif

           if(l3_macro%rad_points_sw_mid(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_mid_mean(idim,jdim)=l3_macro%toa_swup_mid_mean(idim,jdim)/l3_macro%rad_points_sw_mid(idim,jdim)
           elseif(l3_macro%rad_points_sw_mid(idim,jdim) .eq. 0 .and. l3_macro%rad_points_lw_mid(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_mid_mean(idim,jdim)=0.
           else
             l3_macro%toa_swup_mid_mean(idim,jdim)=real_fill_value
           endif
           if(l3_macro%rad_points_lw_mid(idim,jdim) .gt. 0) then
             l3_macro%toa_lwup_mid_mean(idim,jdim)=l3_macro%toa_lwup_mid_mean(idim,jdim)/l3_macro%rad_points_lw_mid(idim,jdim)
           else
             l3_macro%toa_lwup_mid_mean(idim,jdim)=real_fill_value
           endif

           if(l3_macro%rad_points_sw_low(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_low_mean(idim,jdim)=l3_macro%toa_swup_low_mean(idim,jdim)/l3_macro%rad_points_sw_low(idim,jdim)
           elseif(l3_macro%rad_points_sw_low(idim,jdim) .eq. 0 .and. l3_macro%rad_points_lw_low(idim,jdim) .gt. 0) then
             l3_macro%toa_swup_low_mean(idim,jdim)=0.
           else
             l3_macro%toa_swup_low_mean(idim,jdim)=real_fill_value
           endif
           if(l3_macro%rad_points_lw_low(idim,jdim) .gt. 0) then
             l3_macro%toa_lwup_low_mean(idim,jdim)=l3_macro%toa_lwup_low_mean(idim,jdim)/l3_macro%rad_points_lw_low(idim,jdim)
           else
             l3_macro%toa_lwup_low_mean(idim,jdim)=real_fill_value
           endif
                                                                                                                     
         endif

        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value)
        if(l3_macro%cct_points_raw(idim,jdim) .ge. min_num_values .or. &
             & l3_macro%cct_points_clear(idim,jdim) .ge. min_num_values) then

           !cct do this because of new uncertainty
           call compute_stats_stage2(l3_macro%cct_mean(idim,jdim),l3_macro%cct_std(idim,jdim),&
                & l3_macro%cct_mean_error(idim,jdim), l3_macro%cct_std_error(idim,jdim),&
                & l3_macro%cct_prop_uncertainty(idim,jdim), l3_macro%cct_correlated_uncertainty(idim,jdim),&
                & l3_macro%cct_points_raw(idim,jdim)+l3_macro%cct_points_clear(idim,jdim))

        elseif(l3_macro%cct_points_raw(idim,jdim) .lt. min_num_values .and.&
             & l3_macro%cct_points_clear(idim,jdim) .lt. min_num_values) then

           l3_macro%cct_mean(idim,jdim)=real_fill_value
           l3_macro%cct_std(idim,jdim)=real_fill_value
           l3_macro%cct_mean_error(idim,jdim)=real_fill_value
           l3_macro%cct_std_error(idim,jdim)=real_fill_value
           l3_macro%cct_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%cct_correlated_uncertainty(idim,jdim)=real_fill_value

        endif
        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value) during day
        if(l3_macro%cct_points_day(idim,jdim) .ge. min_num_values .or. &
             & l3_micro%cct_points_clear(idim,jdim) .ge. min_num_values) then

           l3_macro%cct_day(idim,jdim)=real(l3_macro%cct_points_day(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points_day(idim,jdim)+l3_micro%cct_points_clear(idim,jdim)), kind=sreal)
           if( l3_macro%cct_day(idim,jdim) .gt. 1.0 ) l3_macro%cct_day(idim,jdim)=1.0-dither3

           !this invalidates the cells where never a "day" pixel was encountered (artic night)
        elseif(l3_macro%cct_points_day(idim,jdim) .lt. min_num_values .and. &
             & l3_micro%cct_points_clear(idim,jdim) .lt. min_num_values) then

           l3_macro%cct_day(idim,jdim)=real_fill_value

        endif
        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value) during twilight
        if(l3_macro%cct_points_twl(idim,jdim) .ge. min_num_values .or. &
             & l3_macro%cct_points_clear_twl(idim,jdim) .ge. min_num_values) then

           l3_macro%cct_twl(idim,jdim)=real(l3_macro%cct_points_twl(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points_twl(idim,jdim)+l3_macro%cct_points_clear_twl(idim,jdim)), kind=sreal)
           if( l3_macro%cct_twl(idim,jdim) .gt. 1.0 ) l3_macro%cct_twl(idim,jdim)=1.0-dither3

        elseif(l3_macro%cct_points_twl(idim,jdim) .lt. min_num_values .and. &
             & l3_macro%cct_points_clear_twl(idim,jdim) .lt. min_num_values) then

           l3_macro%cct_twl(idim,jdim)=real_fill_value

        endif

        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value) during night
        if(l3_macro%cct_points_night(idim,jdim) .ge. min_num_values .or. &
             & l3_macro%cct_points_clear_night(idim,jdim) .ge. min_num_values) then

           l3_macro%cct_night(idim,jdim)=real(l3_macro%cct_points_night(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points_night(idim,jdim)+l3_macro%cct_points_clear_night(idim,jdim)), kind=sreal)
           if( l3_macro%cct_night(idim,jdim) .gt. 1.0 ) l3_macro%cct_night(idim,jdim)=1.0-dither3

        elseif(l3_macro%cct_points_night(idim,jdim) .lt. min_num_values .and. &
             & l3_macro%cct_points_clear_night(idim,jdim) .lt. min_num_values) then

           l3_macro%cct_night(idim,jdim)=real_fill_value

        endif
        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value)
        if(l3_macro%cct_points(idim,jdim) .ge. min_num_values .or. l3_macro%cct_points_clear(idim,jdim) .ge. min_num_values) then

           l3_macro%cct_mean_low(idim,jdim)=real(l3_macro%cct_low_points(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points(idim,jdim)+l3_macro%cct_points_clear(idim,jdim)), kind=sreal)
           if( l3_macro%cct_mean_low(idim,jdim) .gt. 1.0 ) l3_macro%cct_mean_low(idim,jdim)=1.0-dither3

           l3_macro%cct_mean_middle(idim,jdim)=real(l3_macro%cct_middle_points(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points(idim,jdim)+l3_macro%cct_points_clear(idim,jdim)), kind=sreal)
           if( l3_macro%cct_mean_middle(idim,jdim) .gt. 1.0 ) l3_macro%cct_mean_middle(idim,jdim)=1.0-dither3

           l3_macro%cct_mean_high(idim,jdim)=real(l3_macro%cct_high_points(idim,jdim), kind=sreal)/&
                & real((l3_macro%cct_points(idim,jdim)+l3_macro%cct_points_clear(idim,jdim)), kind=sreal)
           if( l3_macro%cct_mean_high(idim,jdim) .gt. 1.0 ) l3_macro%cct_mean_high(idim,jdim)=1.0-dither3

           !this invalidates the cells where there are no clear and no cloudy pixels ever encountered
        elseif(l3_macro%cct_points(idim,jdim) .lt. min_num_values .and. l3_macro%cct_points_clear(idim,jdim) .lt. min_num_values) then

           l3_macro%cct_mean_low(idim,jdim)=real_fill_value
           l3_macro%cct_mean_middle(idim,jdim)=real_fill_value
           l3_macro%cct_mean_high(idim,jdim)=real_fill_value

        endif

        if (l3_macro%stemp_clear_points(idim,jdim) .ge. min_num_values ) then
           !stemp clear
           call compute_stats_stage2(l3_macro%stemp_clear_mean(idim,jdim),l3_macro%stemp_clear_std(idim,jdim),&
                & l3_macro%stemp_clear_mean_error(idim,jdim),l3_macro%stemp_clear_std_error(idim,jdim),&
                & l3_macro%stemp_clear_prop_uncertainty(idim,jdim),l3_macro%stemp_clear_correlated_uncertainty(idim,jdim),&
                & l3_macro%stemp_clear_points(idim,jdim))
        else
           l3_macro%stemp_clear_mean(idim,jdim)=real_fill_value
           l3_macro%stemp_clear_std(idim,jdim)=real_fill_value
           l3_macro%stemp_clear_mean_error(idim,jdim)=real_fill_value
           l3_macro%stemp_clear_std_error(idim,jdim)=real_fill_value
           l3_macro%stemp_clear_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%stemp_clear_correlated_uncertainty(idim,jdim)=real_fill_value
        endif

        !if there have been pixels registered as "cloudy", average the properties
        if(l3_macro%cloudy_points(idim,jdim) .ge. min_num_values ) then
           !cph

           if ( (real(l3_macro%cloudy_points_cph_liq(idim,jdim),kind=sreal)+real(l3_macro%cloudy_points_cph_ice(idim,jdim),kind=sreal)) .gt. 0. ) then

              call compute_stats_stage2(l3_macro%cty_mean(idim,jdim),l3_macro%cty_std(idim,jdim),&
                   & l3_macro%cty_mean_error(idim,jdim),l3_macro%cty_std_error(idim,jdim),&
                   & l3_macro%cty_prop_uncertainty(idim,jdim),l3_macro%cty_correlated_uncertainty(idim,jdim),&
                   & l3_macro%cloudy_points_cph_liq(idim,jdim)+l3_macro%cloudy_points_cph_ice(idim,jdim))
              
              ! l3_macro%cty_mean(idim,jdim)=real(l3_macro%cloudy_points_cph_liq(idim,jdim),kind=sreal)/&
              !      & (real(l3_macro%cloudy_points_cph_liq(idim,jdim),kind=sreal)+real(l3_macro%cloudy_points_cph_ice(idim,jdim),kind=sreal))
           endif

           !cph_std
           ! if ( (real((l3_macro%cloudy_points_cph_liq(idim,jdim)+l3_macro%cloudy_points_cph_ice(idim,jdim)), kind=dreal)-1) .gt. 0. ) then
           !    if ( l3_macro%cloudy_points_cph_liq(idim,jdim) .gt. 0 .and. l3_macro%cloudy_points_cph_ice(idim,jdim) .gt. 0) then
           !       l3_macro%cty_std(idim,jdim)=&
           !            & sqrt((1./(real((l3_macro%cloudy_points_cph_liq(idim,jdim)+l3_macro%cloudy_points_cph_ice(idim,jdim)),kind=dreal)-1)) * &
           !            & ( real(l3_macro%cloudy_points_cph_liq(idim,jdim),kind=dreal) - (real(l3_macro%cloudy_points_cph_liq(idim,jdim), kind=dreal))**2./&
           !            &    real((l3_macro%cloudy_points_cph_liq(idim,jdim)+l3_macro%cloudy_points_cph_ice(idim,jdim)),kind=dreal)  ) )
           !    else
           !       l3_macro%cty_std(idim,jdim)=0.
           !    endif
           ! endif
           !remedian
           !            call compute_remedian_stage2_macro(l3_macro,idim,jdim)

           !logarithmic average of ctp
           l3_macro%ctp_log_mean(idim,jdim)=&
                & exp(l3_macro%ctp_log_mean(idim,jdim)/l3_macro%cloudy_points(idim,jdim))
           !divide sum of varible with number of values =>average

           !cee
           if (l3_macro%cee_points(idim,jdim) .gt. 0) then
              call compute_stats_stage2(l3_macro%cee_mean(idim,jdim),l3_macro%cee_std(idim,jdim),&
                   & l3_macro%cee_mean_error(idim,jdim),l3_macro%cee_std_error(idim,jdim),&
                   & l3_macro%cee_prop_uncertainty(idim,jdim),l3_macro%cee_correlated_uncertainty(idim,jdim),&
                   & l3_macro%cee_points(idim,jdim))
              ! & l3_macro%cloudy_points(idim,jdim))
           endif

           !cth
           call compute_stats_stage2(l3_macro%cth_mean(idim,jdim),l3_macro%cth_std(idim,jdim),&
                & l3_macro%cth_mean_error(idim,jdim),l3_macro%cth_std_error(idim,jdim),&
                & l3_macro%cth_prop_uncertainty(idim,jdim),l3_macro%cth_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%cth_v1(idim,jdim),l3_macro%cth_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%cth_mean_error_sq(idim,jdim))

           !cth_corrected
           call compute_stats_stage2(l3_macro%cth_corrected_mean(idim,jdim),l3_macro%cth_corrected_std(idim,jdim),&
                & l3_macro%cth_corrected_mean_error(idim,jdim),l3_macro%cth_corrected_std_error(idim,jdim),&
                & l3_macro%cth_corrected_prop_uncertainty(idim,jdim),l3_macro%cth_corrected_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%cth_corrected_v1(idim,jdim),l3_macro%cth_corrected_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%cth_corrected_mean_error_sq(idim,jdim))

           !ctt
           call compute_stats_stage2(l3_macro%ctt_mean(idim,jdim),l3_macro%ctt_std(idim,jdim),&
                & l3_macro%ctt_mean_error(idim,jdim),l3_macro%ctt_std_error(idim,jdim),&
                & l3_macro%ctt_prop_uncertainty(idim,jdim),l3_macro%ctt_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%ctt_v1(idim,jdim),l3_macro%ctt_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%ctt_mean_error_sq(idim,jdim))

           !ctt_corrected
           call compute_stats_stage2(l3_macro%ctt_corrected_mean(idim,jdim),l3_macro%ctt_corrected_std(idim,jdim),&
                & l3_macro%ctt_corrected_mean_error(idim,jdim),l3_macro%ctt_corrected_std_error(idim,jdim),&
                & l3_macro%ctt_corrected_prop_uncertainty(idim,jdim),l3_macro%ctt_corrected_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%ctt_corrected_v1(idim,jdim),l3_macro%ctt_corrected_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%ctt_corrected_mean_error_sq(idim,jdim))

           !stemp
           call compute_stats_stage2(l3_macro%stemp_mean(idim,jdim),l3_macro%stemp_std(idim,jdim),&
                & l3_macro%stemp_mean_error(idim,jdim),l3_macro%stemp_std_error(idim,jdim),&
                & l3_macro%stemp_prop_uncertainty(idim,jdim),l3_macro%stemp_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%stemp_v1(idim,jdim),l3_macro%stemp_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%stemp_mean_error_sq(idim,jdim))

           !ctp
           call compute_stats_stage2(l3_macro%ctp_mean(idim,jdim),l3_macro%ctp_std(idim,jdim),&
                & l3_macro%ctp_mean_error(idim,jdim),l3_macro%ctp_std_error(idim,jdim),&
                & l3_macro%ctp_prop_uncertainty(idim,jdim),l3_macro%ctp_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%ctp_v1(idim,jdim),l3_macro%ctp_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%ctp_mean_error_sq(idim,jdim))

           !ctp_corrected
           call compute_stats_stage2(l3_macro%ctp_corrected_mean(idim,jdim),l3_macro%ctp_corrected_std(idim,jdim),&
                & l3_macro%ctp_corrected_mean_error(idim,jdim),l3_macro%ctp_corrected_std_error(idim,jdim),&
                & l3_macro%ctp_corrected_prop_uncertainty(idim,jdim),l3_macro%ctp_corrected_correlated_uncertainty(idim,jdim),&
                                !                 & l3_macro%ctp_corrected_v1(idim,jdim),l3_macro%ctp_corrected_v2(idim,jdim),l3_macro%cloudy_points(idim,jdim),&
                & l3_macro%cloudy_points(idim,jdim))!,l3_macro%ctp_corrected_mean_error_sq(idim,jdim))


           !invalidate the cell if there have not been enough cloudy pixels
        else

           l3_macro%cee_mean(idim,jdim)=real_fill_value
           l3_macro%cee_std(idim,jdim)=real_fill_value
           l3_macro%cee_mean_error(idim,jdim)=real_fill_value
           l3_macro%cee_std_error(idim,jdim)=real_fill_value
           l3_macro%cee_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%cee_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%ctp_mean(idim,jdim)=real_fill_value
           l3_macro%ctp_std(idim,jdim)=real_fill_value
           l3_macro%ctp_mean_error(idim,jdim)=real_fill_value
           l3_macro%ctp_std_error(idim,jdim)=real_fill_value
           l3_macro%ctp_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%ctp_correlated_uncertainty(idim,jdim)=real_fill_value
           l3_macro%ctp_log_mean(idim,jdim)=real_fill_value

           l3_macro%ctp_corrected_mean(idim,jdim)=real_fill_value
           l3_macro%ctp_corrected_std(idim,jdim)=real_fill_value
           l3_macro%ctp_corrected_mean_error(idim,jdim)=real_fill_value
           l3_macro%ctp_corrected_std_error(idim,jdim)=real_fill_value
           l3_macro%ctp_corrected_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%ctp_corrected_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%cth_mean(idim,jdim)=real_fill_value
           l3_macro%cth_std(idim,jdim)=real_fill_value
           l3_macro%cth_mean_error(idim,jdim)=real_fill_value
           l3_macro%cth_std_error(idim,jdim)=real_fill_value
           l3_macro%cth_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%cth_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%cth_corrected_mean(idim,jdim)=real_fill_value
           l3_macro%cth_corrected_std(idim,jdim)=real_fill_value
           l3_macro%cth_corrected_mean_error(idim,jdim)=real_fill_value
           l3_macro%cth_corrected_std_error(idim,jdim)=real_fill_value
           l3_macro%cth_corrected_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%cth_corrected_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%ctt_mean(idim,jdim)=real_fill_value
           l3_macro%ctt_std(idim,jdim)=real_fill_value
           l3_macro%ctt_mean_error(idim,jdim)=real_fill_value
           l3_macro%ctt_std_error(idim,jdim)=real_fill_value
           l3_macro%ctt_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%ctt_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%ctt_corrected_mean(idim,jdim)=real_fill_value
           l3_macro%ctt_corrected_std(idim,jdim)=real_fill_value
           l3_macro%ctt_corrected_mean_error(idim,jdim)=real_fill_value
           l3_macro%ctt_corrected_std_error(idim,jdim)=real_fill_value
           l3_macro%ctt_corrected_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%ctt_corrected_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%stemp_mean(idim,jdim)=real_fill_value
           l3_macro%stemp_std(idim,jdim)=real_fill_value
           l3_macro%stemp_mean_error(idim,jdim)=real_fill_value
           l3_macro%stemp_std_error(idim,jdim)=real_fill_value
           l3_macro%stemp_prop_uncertainty(idim,jdim)=real_fill_value
           l3_macro%stemp_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_macro%cty_mean(idim,jdim)=real_fill_value
           l3_macro%cty_std(idim,jdim)=real_fill_value

        endif
        !micro properties statistics

        !if there are clear or cloudy pixels (cover of "0" and "1" is valid value)
        !calculate cloud area fraction
        !         if(l3_micro%cct_points(idim,jdim) .ge. min_num_values .or. l3_micro%cct_points_clear(idim,jdim) .ge. min_num_values) then

        !            l3_micro%cct_mean(idim,jdim)=real(l3_micro%cct_points(idim,jdim), kind=sreal)/&
        !                 & real((l3_micro%cct_points(idim,jdim)+l3_micro%cct_points_clear(idim,jdim)), kind=sreal)
        !            if( l3_micro%cct_mean(idim,jdim) .gt. 1.0 ) l3_micro%cct_mean(idim,jdim)=1.0-dither3

        !            ! stapel new stddev approach
        !            if ( (real((l3_micro%cct_points(idim,jdim)+l3_micro%cct_points_clear(idim,jdim)), kind=dreal)-1) .gt. 0. ) then
        !               if ( l3_micro%cct_points_clear(idim,jdim) .gt. 0 .and. l3_micro%cct_points(idim,jdim) .gt. 0) then
        !                  l3_micro%cct_std(idim,jdim)=&
        !                       & sqrt( ( 1./(real((l3_micro%cct_points(idim,jdim)+l3_micro%cct_points_clear(idim,jdim)), kind=dreal)-1)) * &
        !                       & ( real(l3_micro%cct_points(idim,jdim), kind=dreal) - (real(l3_micro%cct_points(idim,jdim), kind=dreal))**2./&
        !                       & real((l3_micro%cct_points(idim,jdim)+l3_micro%cct_points_clear(idim,jdim)), kind=dreal)  ) )
        !               else
        !                  l3_micro%cct_std(idim,jdim)=0.
        !               endif
        !            endif
        !            !this invalidates the cells where there are no clear and no cloudy pixels ever encountered
        !         elseif(l3_micro%cct_points(idim,jdim) .lt. min_num_values .and. l3_micro%cct_points_clear(idim,jdim) .lt. min_num_values) then

        !            l3_micro%cct_mean(idim,jdim)=real_fill_value
        !            l3_micro%cct_std(idim,jdim)=real_fill_value

        !         endif

        !if there have been pixels registered as "cloudy", average the properties
        if(l3_micro%cloudy_points(idim,jdim) .ge. min_num_values ) then

           !            !remedian
           !            call compute_remedian_stage2_micro(l3_micro,idim,jdim)
           !cot
           call compute_stats_stage2(l3_micro%cot_mean(idim,jdim),l3_micro%cot_std(idim,jdim),&
                & l3_micro%cot_mean_error(idim,jdim),l3_micro%cot_std_error(idim,jdim),&
                & l3_micro%cot_prop_uncertainty(idim,jdim),l3_micro%cot_correlated_uncertainty(idim,jdim),&
                                !                                                 & l3_micro%cot_v1(idim,jdim),l3_micro%cot_v2(idim,jdim),l3_micro%cloudy_points(idim,jdim),&
                & l3_micro%cloudy_points(idim,jdim))!,l3_micro%cot_mean_error_sq(idim,jdim))

           !logarithmic average of cot
           l3_micro%cot_log_mean(idim,jdim)=&
                & exp(l3_micro%cot_log_mean(idim,jdim)/l3_micro%cloudy_points(idim,jdim))


           !ref
           call compute_stats_stage2(l3_micro%ref_mean(idim,jdim),l3_micro%ref_std(idim,jdim),&
                & l3_micro%ref_mean_error(idim,jdim),l3_micro%ref_std_error(idim,jdim),&
                & l3_micro%ref_prop_uncertainty(idim,jdim),l3_micro%ref_correlated_uncertainty(idim,jdim),&
                                !                                                & l3_micro%ref_v1(idim,jdim),l3_micro%ref_v2(idim,jdim),l3_micro%cloudy_points(idim,jdim),&
                & l3_micro%cloudy_points(idim,jdim))!,l3_micro%ref_mean_error_sq(idim,jdim))

           !cloud_albedo1
           call compute_stats_stage2(l3_micro%cloud_albedo1_mean(idim,jdim),l3_micro%cloud_albedo1_std(idim,jdim),&
                & l3_micro%cloud_albedo1_mean_error(idim,jdim),l3_micro%cloud_albedo1_std_error(idim,jdim),&
                & l3_micro%cloud_albedo1_prop_uncertainty(idim,jdim),l3_micro%cloud_albedo1_correlated_uncertainty(idim,jdim),&
                                !                 & l3_micro%cloud_albedo1_v1(idim,jdim),l3_micro%cloud_albedo1_v2(idim,jdim),l3_micro%cloudy_points(idim,jdim),&
                & l3_micro%cloudy_points(idim,jdim))!,l3_micro%cloud_albedo1_mean_error_sq(idim,jdim))

           !cloud_albedo2
           call compute_stats_stage2(l3_micro%cloud_albedo2_mean(idim,jdim),l3_micro%cloud_albedo2_std(idim,jdim),&
                & l3_micro%cloud_albedo2_mean_error(idim,jdim),l3_micro%cloud_albedo2_std_error(idim,jdim),&
                & l3_micro%cloud_albedo2_prop_uncertainty(idim,jdim),l3_micro%cloud_albedo2_correlated_uncertainty(idim,jdim),&
                                !                 & l3_micro%cloud_albedo2_v1(idim,jdim),l3_micro%cloud_albedo2_v2(idim,jdim),l3_micro%cloudy_points(idim,jdim),&
                & l3_micro%cloudy_points(idim,jdim))!,l3_micro%cloud_albedo2_mean_error_sq(idim,jdim))

           !cwp
           call compute_stats_stage2(l3_micro%cwp_mean(idim,jdim),l3_micro%cwp_std(idim,jdim),&
                & l3_micro%cwp_mean_error(idim,jdim),l3_micro%cwp_std_error(idim,jdim),&
                & l3_micro%cwp_prop_uncertainty(idim,jdim),l3_micro%cwp_correlated_uncertainty(idim,jdim),&
                                !                 & l3_micro%cwp_v1(idim,jdim),l3_micro%cwp_v2(idim,jdim),l3_micro%cloudy_points(idim,jdim),&
                & l3_micro%cloudy_points(idim,jdim))!,l3_micro%cwp_mean_error_sq(idim,jdim))

           !lwp
           call compute_stats_stage2(l3_micro%cwp_mean_liq(idim,jdim),l3_micro%cwp_std_liq(idim,jdim),&
                & l3_micro%cwp_mean_error_liq(idim,jdim),l3_micro%cwp_std_error_liq(idim,jdim),&
                & l3_micro%cwp_prop_uncertainty_liq(idim,jdim),l3_micro%cwp_correlated_uncertainty_liq(idim,jdim),&
                                !                 & l3_micro%cwp_v1_liq(idim,jdim),l3_micro%cwp_v2_liq(idim,jdim),l3_micro%cloudy_points_liq(idim,jdim),&
                & l3_micro%cloudy_points_liq(idim,jdim))!,l3_micro%cwp_mean_error_liq_sq(idim,jdim))

           !iwp
           call compute_stats_stage2(l3_micro%cwp_mean_ice(idim,jdim),l3_micro%cwp_std_ice(idim,jdim),&
                & l3_micro%cwp_mean_error_ice(idim,jdim),l3_micro%cwp_std_error_ice(idim,jdim),&
                & l3_micro%cwp_prop_uncertainty_ice(idim,jdim),l3_micro%cwp_correlated_uncertainty_ice(idim,jdim),&
                                !                 & l3_micro%cwp_v1_ice(idim,jdim),l3_micro%cwp_v2_ice(idim,jdim),l3_micro%cloudy_points_ice(idim,jdim),&
                & l3_micro%cloudy_points_ice(idim,jdim))!,l3_micro%cwp_mean_error_ice_sq(idim,jdim))

           !phase: This is essentially a liquid phase cloud fraction:  (liq. clouds)/(total clouds)
           !as the same selection criteria for the "cty_mean" counter is applied as for "cct_points"
           !Thus: 1.0-cty_mean=ice cloud fraction should hold
           !cph_day
           if ( (real(l3_micro%cloudy_points_liq(idim,jdim),kind=sreal)+real(l3_micro%cloudy_points_ice(idim,jdim),kind=sreal)) .gt. 0. ) then
              ! l3_micro%cty_mean(idim,jdim)=real(l3_micro%cloudy_points_liq(idim,jdim),kind=sreal)/&
              !      & (real(l3_micro%cloudy_points_liq(idim,jdim),kind=sreal)+real(l3_micro%cloudy_points_ice(idim,jdim),kind=sreal))
              call compute_stats_stage2(l3_micro%cty_mean(idim,jdim),l3_micro%cty_std(idim,jdim),&
                   & l3_micro%cty_mean_error(idim,jdim),l3_micro%cty_std_error(idim,jdim),&
                   & l3_micro%cty_prop_uncertainty(idim,jdim),l3_micro%cty_correlated_uncertainty(idim,jdim),&
                   & l3_micro%cloudy_points_liq(idim,jdim)+l3_micro%cloudy_points_ice(idim,jdim))
           endif

           !cph_day_std
           ! if ( (real((l3_micro%cloudy_points_liq(idim,jdim)+l3_micro%cloudy_points_ice(idim,jdim)), kind=dreal)-1) .gt. 0. ) then
           !    if ( l3_micro%cloudy_points_liq(idim,jdim) .gt. 0 .and. l3_micro%cloudy_points_ice(idim,jdim) .gt. 0) then
           !       l3_micro%cty_std(idim,jdim)=&
           !            &  sqrt((1./(real((l3_micro%cloudy_points_liq(idim,jdim)+l3_micro%cloudy_points_ice(idim,jdim)),kind=dreal)-1)) * &
           !            & ( real(l3_micro%cloudy_points_liq(idim,jdim),kind=dreal) - (real(l3_micro%cloudy_points_liq(idim,jdim), kind=dreal))**2./&
           !            &   real((l3_micro%cloudy_points_liq(idim,jdim)+l3_micro%cloudy_points_ice(idim,jdim)),kind=dreal)  ) )
           !    else
           !       l3_micro%cty_std(idim,jdim)=0.
           !    endif
           ! endif

           !ref liquid
           call compute_stats_stage2(l3_micro%ref_mean_liq(idim,jdim),l3_micro%ref_std_liq(idim,jdim),&
                & l3_micro%ref_mean_error_liq(idim,jdim),l3_micro%ref_std_error_liq(idim,jdim),&
                & l3_micro%ref_prop_uncertainty_liq(idim,jdim),l3_micro%ref_correlated_uncertainty_liq(idim,jdim),&
                & l3_micro%cloudy_points_liq(idim,jdim))

           !ref ice
           call compute_stats_stage2(l3_micro%ref_mean_ice(idim,jdim),l3_micro%ref_std_ice(idim,jdim),&
                & l3_micro%ref_mean_error_ice(idim,jdim),l3_micro%ref_std_error_ice(idim,jdim),&
                & l3_micro%ref_prop_uncertainty_ice(idim,jdim),l3_micro%ref_correlated_uncertainty_ice(idim,jdim),&
                                !                 & l3_micro%ref_v1_ice(idim,jdim),l3_micro%ref_v2_ice(idim,jdim),l3_micro%cloudy_points_ice(idim,jdim),&
                & l3_micro%cloudy_points_ice(idim,jdim))!,l3_micro%ref_mean_error_ice_sq(idim,jdim))

           !cloud_albedo liquid
           call compute_stats_stage2(l3_micro%cloud_albedo1_mean_liq(idim,jdim),l3_micro%cloud_albedo1_std_liq(idim,jdim),&
                & l3_micro%cloud_albedo1_mean_error_liq(idim,jdim),l3_micro%cloud_albedo1_std_error_liq(idim,jdim),&
                & l3_micro%cloud_albedo1_prop_uncertainty_liq(idim,jdim),l3_micro%cloud_albedo1_correlated_uncertainty_liq(idim,jdim),&
                                ! & l3_micro%cloud_albedo1_v1_liq(idim,jdim),l3_micro%cloud_albedo1_v2_liq(idim,jdim),l3_micro%cloudy_points_liq(idim,jdim),&
                & l3_micro%cloudy_points_liq(idim,jdim))!,l3_micro%cloud_albedo1_mean_error_liq_sq(idim,jdim))

           !cloud_albedo1 ice
           call compute_stats_stage2(l3_micro%cloud_albedo1_mean_ice(idim,jdim),l3_micro%cloud_albedo1_std_ice(idim,jdim),&
                & l3_micro%cloud_albedo1_mean_error_ice(idim,jdim),l3_micro%cloud_albedo1_std_error_ice(idim,jdim),&
                & l3_micro%cloud_albedo1_prop_uncertainty_ice(idim,jdim),l3_micro%cloud_albedo1_correlated_uncertainty_ice(idim,jdim),&
                                !& l3_micro%cloud_albedo1_v1_ice(idim,jdim),l3_micro%cloud_albedo1_v2_ice(idim,jdim),l3_micro%cloudy_points_ice(idim,jdim),&
                & l3_micro%cloudy_points_ice(idim,jdim))!,l3_micro%cloud_albedo1_mean_error_ice_sq(idim,jdim))

           !cloud_albedo2 liquid
           call compute_stats_stage2(l3_micro%cloud_albedo2_mean_liq(idim,jdim),l3_micro%cloud_albedo2_std_liq(idim,jdim),&
                & l3_micro%cloud_albedo2_mean_error_liq(idim,jdim),l3_micro%cloud_albedo2_std_error_liq(idim,jdim),&
                & l3_micro%cloud_albedo2_prop_uncertainty_liq(idim,jdim),l3_micro%cloud_albedo2_correlated_uncertainty_liq(idim,jdim),&
                                !& l3_micro%cloud_albedo2_v1_liq(idim,jdim),l3_micro%cloud_albedo2_v2_liq(idim,jdim),l3_micro%cloudy_points_liq(idim,jdim),&
                & l3_micro%cloudy_points_liq(idim,jdim))!,l3_micro%cloud_albedo2_mean_error_liq_sq(idim,jdim))

           !           !cloud_albedo2 ice
           call compute_stats_stage2(l3_micro%cloud_albedo2_mean_ice(idim,jdim),l3_micro%cloud_albedo2_std_ice(idim,jdim),&
                & l3_micro%cloud_albedo2_mean_error_ice(idim,jdim),l3_micro%cloud_albedo2_std_error_ice(idim,jdim),&
                & l3_micro%cloud_albedo2_prop_uncertainty_ice(idim,jdim),l3_micro%cloud_albedo2_correlated_uncertainty_ice(idim,jdim),&
                                !& l3_micro%cloud_albedo2_v1_ice(idim,jdim),l3_micro%cloud_albedo2_v2_ice(idim,jdim),l3_micro%cloudy_points_ice(idim,jdim),&
                & l3_micro%cloudy_points_ice(idim,jdim))!,l3_micro%cloud_albedo2_mean_error_ice_sq(idim,jdim))

           !cot liquid
           call compute_stats_stage2(l3_micro%cot_mean_liq(idim,jdim),l3_micro%cot_std_liq(idim,jdim),&
                & l3_micro%cot_mean_error_liq(idim,jdim),l3_micro%cot_std_error_liq(idim,jdim),&
                & l3_micro%cot_prop_uncertainty_liq(idim,jdim),l3_micro%cot_correlated_uncertainty_liq(idim,jdim),&
                                !                                                 & l3_micro%cot_v1_liq(idim,jdim),l3_micro%cot_v2_liq(idim,jdim),l3_micro%cloudy_points_liq(idim,jdim),&
                & l3_micro%cloudy_points_liq(idim,jdim))!,l3_micro%cot_mean_error_ice_sq(idim,jdim))

           !cot ice
           call compute_stats_stage2(l3_micro%cot_mean_ice(idim,jdim),l3_micro%cot_std_ice(idim,jdim),&
                & l3_micro%cot_mean_error_ice(idim,jdim),l3_micro%cot_std_error_ice(idim,jdim),&
                & l3_micro%cot_prop_uncertainty_ice(idim,jdim),l3_micro%cot_correlated_uncertainty_ice(idim,jdim),&
                                !                                                 & l3_micro%cot_v1_ice(idim,jdim),l3_micro%cot_v2_ice(idim,jdim),l3_micro%cloudy_points_ice(idim,jdim),&
                & l3_micro%cloudy_points_ice(idim,jdim))!,l3_micro%cot_mean_error_ice_sq(idim,jdim))

           !invalidate the cell if there have not been enough cloudy pixels
        else
           l3_micro%cot_mean(idim,jdim)=real_fill_value
           !            l3_micro%cot_mean_error_sq(idim,jdim)=real_fill_value
           l3_micro%cot_log_mean(idim,jdim)=real_fill_value
           l3_micro%cot_std(idim,jdim)=real_fill_value
           l3_micro%cot_mean_error(idim,jdim)=real_fill_value
           l3_micro%cot_std_error(idim,jdim)=real_fill_value
           l3_micro%cot_prop_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cot_correlated_uncertainty(idim,jdim)=real_fill_value
           l3_micro%ref_mean(idim,jdim)=real_fill_value
           !            l3_micro%ref_mean_error_sq(idim,jdim)=real_fill_value
           l3_micro%ref_std(idim,jdim)=real_fill_value
           l3_micro%ref_mean_error(idim,jdim)=real_fill_value
           l3_micro%ref_std_error(idim,jdim)=real_fill_value
           l3_micro%ref_prop_uncertainty(idim,jdim)=real_fill_value
           l3_micro%ref_correlated_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_mean(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo1_mean_error_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_mean_error(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std_error(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_prop_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_correlated_uncertainty(idim,jdim)=real_fill_value

           l3_micro%cloud_albedo2_mean(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo2_mean_error_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_mean_error(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std_error(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_prop_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_correlated_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cwp_mean(idim,jdim)=real_fill_value
           !            l3_micro%cwp_mean_error_sq(idim,jdim)=real_fill_value
           l3_micro%cwp_std(idim,jdim)=real_fill_value
           l3_micro%cwp_mean_error(idim,jdim)=real_fill_value
           l3_micro%cwp_std_error(idim,jdim)=real_fill_value
           l3_micro%cwp_prop_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cwp_correlated_uncertainty(idim,jdim)=real_fill_value
           l3_micro%cwp_mean_liq(idim,jdim)=real_fill_value
           !            l3_micro%cwp_mean_error_liq_sq(idim,jdim)=real_fill_value
           l3_micro%cwp_std_liq(idim,jdim)=real_fill_value
           l3_micro%cwp_mean_error_liq(idim,jdim)=real_fill_value
           l3_micro%cwp_std_error_liq(idim,jdim)=real_fill_value
           l3_micro%cwp_prop_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cwp_correlated_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cwp_mean_ice(idim,jdim)=real_fill_value
           !            l3_micro%cwp_mean_error_ice_sq(idim,jdim)=real_fill_value
           l3_micro%cwp_std_ice(idim,jdim)=real_fill_value
           l3_micro%cwp_mean_error_ice(idim,jdim)=real_fill_value
           l3_micro%cwp_std_error_ice(idim,jdim)=real_fill_value
           l3_micro%cwp_prop_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%cwp_correlated_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%ref_mean_liq(idim,jdim)=real_fill_value
           !            l3_micro%ref_mean_error_liq_sq(idim,jdim)=real_fill_value
           l3_micro%ref_std_liq(idim,jdim)=real_fill_value
           l3_micro%ref_mean_error_liq(idim,jdim)=real_fill_value
           l3_micro%ref_std_error_liq(idim,jdim)=real_fill_value
           l3_micro%ref_prop_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%ref_correlated_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%ref_mean_ice(idim,jdim)=real_fill_value
           !            l3_micro%ref_mean_error_ice_sq(idim,jdim)=real_fill_value
           l3_micro%ref_std_ice(idim,jdim)=real_fill_value
           l3_micro%ref_mean_error_ice(idim,jdim)=real_fill_value
           l3_micro%ref_std_error_ice(idim,jdim)=real_fill_value
           l3_micro%ref_prop_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%ref_correlated_uncertainty_ice(idim,jdim)=real_fill_value

           l3_micro%cloud_albedo1_mean_liq(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo1_mean_error_liq_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_mean_error_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std_error_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_prop_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_correlated_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_mean_ice(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo1_mean_error_ice_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_mean_error_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_std_error_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_prop_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo1_correlated_uncertainty_ice(idim,jdim)=real_fill_value

           l3_micro%cloud_albedo2_mean_liq(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo2_mean_error_liq_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_mean_error_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std_error_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_prop_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_correlated_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_mean_ice(idim,jdim)=real_fill_value
           !            l3_micro%cloud_albedo2_mean_error_ice_sq(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_mean_error_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_std_error_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_prop_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%cloud_albedo2_correlated_uncertainty_ice(idim,jdim)=real_fill_value

           l3_micro%cot_mean_liq(idim,jdim)=real_fill_value
           !            l3_micro%cot_mean_error_liq_sq(idim,jdim)=real_fill_value
           l3_micro%cot_std_liq(idim,jdim)=real_fill_value
           l3_micro%cot_mean_error_liq(idim,jdim)=real_fill_value
           l3_micro%cot_std_error_liq(idim,jdim)=real_fill_value
           l3_micro%cot_prop_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cot_correlated_uncertainty_liq(idim,jdim)=real_fill_value
           l3_micro%cot_mean_ice(idim,jdim)=real_fill_value
           !            l3_micro%cot_mean_error_ice_sq(idim,jdim)=real_fill_value
           l3_micro%cot_std_ice(idim,jdim)=real_fill_value
           l3_micro%cot_mean_error_ice(idim,jdim)=real_fill_value
           l3_micro%cot_std_error_ice(idim,jdim)=real_fill_value
           l3_micro%cot_prop_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%cot_correlated_uncertainty_ice(idim,jdim)=real_fill_value
           l3_micro%cty_mean(idim,jdim)=real_fill_value
           l3_micro%cty_std(idim,jdim)=real_fill_value

           !            l3_micro%cot_median(idim,jdim)=real_fill_value
           !            l3_micro%ref_median(idim,jdim)=real_fill_value
           !            l3_micro%cwp_median(idim,jdim)=real_fill_value
        endif
        !loops over global grid close
     enddo
  enddo
  !OMP END PARALLEL DO
