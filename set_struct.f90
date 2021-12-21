! Name: set_struct.f90
!
!
! Purpose: File contains several subroutines to allocate and initialize structures and user defined variable types.
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
!2013/04/12 Cintia Carbajal Henken added variables ice/liq for cot and ref
!2014/03/28 Cintia Carbajal Henken added cth2 with if statements for FAME-C
!2014/11/19 OS added variables cccot_pre, cty_pre, cty_post, cct, cct_pre, cct_post,
!  cty_pre_error, cct_error, cct_pre_error cct_post_points_raw, cct_post_points_clear_raw,
!  cct_post_raw
!2015/02/27 OS and SST added l3_macro%cloudy_points, %cty_mean/std (i.e. cloud type),
!  histogram_ref_phase, histogram_cwp_phase removal of weighted means/standard deviations,
!  added error prints when deallocation fails; further removal of various superfluous variables/clean up,
!  histogram bins now follow clara-a2 and claas definition (i.e. now higher resolved), change in number
!  of command arguments (removed l2tol3configpath, l2tol3configfile, l1closure); grid resolution now
!  only dependent on call arguments
!2015/03/20 CP add cloud albedo
!2015/04/23 OS added (de)allocation of solzen, relazi, stemp(_error), cld_type; removed orig_neighbour
!2015/07/07 CP add corrected cloud top height
!2015/07/10 OS added cty_day, reactivated cct_error
!2015/08/31 OS changed type of time fill value
!2015/07/27 CP add stemp
!2015/12/16 OS removed duplicate stemp; added L2 variable convergence
!2016/01/21 OS added allsky IWP and LWP
!2016/02/18 OS added variables CTP_corrected, CTT_corrected, CEE
!2016/02/23 OS changed time fill value
!2016/02/26 OS added counter for CEE
!2016/03/18 OS changed fill values for variables to be scaled
!2017/06/22 OS added stemp_clear/cloudy, toa/boa, ann phase, MODIS refl/BT
!2017/06/29 OS added cty, which stands for cph (it should not)
!
! $Id$
!
! Bugs:
!
!none known

!-------------------------------------------
!-------------------------------------------
subroutine set_l3_2d(l3_struct_2d,nl3x,nl3y,n_bins,nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features)
  !-------------------------------------------
  !-------------------------------------------
  !   integer(kind=lint), dimension(n_methods, n_variables, n_features,nl3x,nl3y) :: l3_n_points
  !   real(kind=sreal), dimension(n_methods,n_variables,n_features,nl3x,nl3y) :: l3_vector
  !   real(kind=sreal), dimension(n_methods,n_variables,n_bins+2,nl3x,nl3y) :: l3_histogram
  !   real(kind=sreal), dimension(n_methods,n_variables,n_bins_power,nl3x,nl3y) :: l3_power_spec
  !   real(kind=sreal), dimension(n_methods,nl3x,nl3y) :: l3_f_para

  !   real(kind=sreal), dimension(n_methods,n_variables,n_error_features,nl3x,nl3y) :: l3_errors

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y, n_bins

  integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features

  type(l3_output_struct_2d) :: l3_struct_2d

  allocate(l3_struct_2d%l3_n_points(nl3x,nl3y))
  l3_struct_2d%l3_n_points=0!long_int_fill_value

  !  allocate(l3_struct_2d%l3_sensor_azimuth(nl3x,nl3y))
  !  l3_struct_2d%l3_sensor_azimuth=long_int_fill_value

  allocate(l3_struct_2d%l3_vector(nl3x,nl3y,nl2vars_1km,n_val_plus_error, n_features))
  l3_struct_2d%l3_vector=filter_micro

  allocate(l3_struct_2d%l3_histogram_cot_ctp_phase(nl3x,nl3y,n_hist_cot-1,n_hist_ctp-1,n_hist_phase))
  l3_struct_2d%l3_histogram_cot_ctp_phase=long_int_zero

  allocate(l3_struct_2d%l3_histogram_cot_phase(nl3x,nl3y,n_cot_bins,n_hist_phase))
  l3_struct_2d%l3_histogram_cot_phase=long_int_zero

  !  allocate(l3_struct_2d%l3_median_cot(nl3x,nl3y,n_hist_phase))
  !  l3_struct_2d%l3_median_cot=filter_micro

  allocate(l3_struct_2d%l3_median(nl3x,nl3y,nl2vars_1km))
  l3_struct_2d%l3_median=filter_micro

  allocate(l3_struct_2d%l3_ctp_log(nl3x,nl3y))
  l3_struct_2d%l3_ctp_log=filter_micro

!!$  allocate(l3_struct_2d%l3_power_spec(n_methods,n_variables,n_bins_power,nl3x,nl3y))
!!$  l3_struct_2d%l3_power_spec=real_fill_value

  !  allocate(l3_struct_2d%l3_f_para(nl3x,nl3y))
  !  l3_struct_2d%l3_f_para=real_fill_value

  !obsolete  allocate(l3_struct_2d%l3_errors(n_variables,n_error_features,nl3x,nl3y))
  !obsolete  l3_struct_2d%l3_errors=real_fill_value

end subroutine set_l3_2d

subroutine set_l3_rad_struct(l3_rad,nl3x,nl3y)

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y

  type(l3_rad_struct) :: l3_rad

  allocate(l3_rad%denominator(nl3x,nl3y))
  l3_rad%denominator=long_int_zero

  allocate(l3_rad%rad_mean(nl3x,nl3y))
  l3_rad%rad_mean=filter_micro

  allocate(l3_rad%tau_consistent(nl3x,nl3y))
  l3_rad%tau_consistent=filter_micro

  allocate(l3_rad%tau_compare(nl3x,nl3y))
  l3_rad%tau_compare=filter_micro

  allocate(l3_rad%ppab(nl3x,nl3y))
  l3_rad%ppab=filter_micro

  allocate(l3_rad%reduction_factor(nl3x,nl3y))
  l3_rad%reduction_factor=filter_micro

  allocate(l3_rad%variance_para(nl3x,nl3y))
  l3_rad%variance_para=filter_micro

end subroutine set_l3_rad_struct

!-------------------------------------------
!-------------------------------------------
! subroutine set_l3_macro_struct(l3_macro,nl3x,nl3y,n_bins,nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features,calgo)
subroutine set_l3_macro_struct(l3_macro,nl3x,nl3y,calgo)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y, n_bins

  !   integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features

  type(l3_macro_struct) :: l3_macro

  character (len=paramlength) :: calgo

  allocate(l3_macro%points(nl3x,nl3y))
  l3_macro%points=0!long_int_fill_value
  allocate(l3_macro%cloudy_points(nl3x,nl3y))
  l3_macro%cloudy_points=0!long_int_fill_value
  allocate(l3_macro%clear_points(nl3x,nl3y))
  l3_macro%clear_points=0!long_int_fill_value
  !cty points
  allocate(l3_macro%cloudy_points_cph_liq(nl3x,nl3y))
  l3_macro%cloudy_points_cph_liq=0!long_int_fill_value

  allocate(l3_macro%cloudy_points_cph_ice(nl3x,nl3y))
  l3_macro%cloudy_points_cph_ice=0!long_int_fill_value

  !   allocate(l3_macro%remedian_counter(nl3x,nl3y,n_remedian_exponent))
  !   l3_macro%remedian_counter=0

  !cty
  allocate(l3_macro%cty_mean(nl3x,nl3y))
  l3_macro%cty_mean=filter_micro
  allocate(l3_macro%cty_std(nl3x,nl3y))
  l3_macro%cty_std=filter_micro
  allocate(l3_macro%cty_mean_error(nl3x,nl3y))
  l3_macro%cty_mean_error=filter_micro
  allocate(l3_macro%cty_std_error(nl3x,nl3y))
  l3_macro%cty_std_error=filter_micro
  allocate(l3_macro%cty_prop_uncertainty(nl3x,nl3y))
  l3_macro%cty_prop_uncertainty=filter_micro
  allocate(l3_macro%cty_correlated_uncertainty(nl3x,nl3y))
  l3_macro%cty_correlated_uncertainty=filter_micro

  !ctp
  allocate(l3_macro%ctp_mean(nl3x,nl3y))
  l3_macro%ctp_mean=filter_micro
  allocate(l3_macro%ctp_mean_error(nl3x,nl3y))
  l3_macro%ctp_mean_error=filter_micro
  allocate(l3_macro%ctp_std(nl3x,nl3y))
  l3_macro%ctp_std=filter_micro
  allocate(l3_macro%ctp_std_error(nl3x,nl3y))
  l3_macro%ctp_std_error=filter_micro
  allocate(l3_macro%ctp_log_mean(nl3x,nl3y))
  l3_macro%ctp_log_mean=filter_micro
  allocate(l3_macro%ctp_prop_uncertainty(nl3x,nl3y))
  l3_macro%ctp_prop_uncertainty=filter_micro
  allocate(l3_macro%ctp_correlated_uncertainty(nl3x,nl3y))
  l3_macro%ctp_correlated_uncertainty=filter_micro

  !ctp_corrected
  allocate(l3_macro%ctp_corrected_mean(nl3x,nl3y))
  l3_macro%ctp_corrected_mean=filter_micro
  allocate(l3_macro%ctp_corrected_mean_error(nl3x,nl3y))
  l3_macro%ctp_corrected_mean_error=filter_micro
  allocate(l3_macro%ctp_corrected_std(nl3x,nl3y))
  l3_macro%ctp_corrected_std=filter_micro
  allocate(l3_macro%ctp_corrected_std_error(nl3x,nl3y))
  l3_macro%ctp_corrected_std_error=filter_micro
  allocate(l3_macro%ctp_corrected_prop_uncertainty(nl3x,nl3y))
  l3_macro%ctp_corrected_prop_uncertainty=filter_micro
  allocate(l3_macro%ctp_corrected_correlated_uncertainty(nl3x,nl3y))
  l3_macro%ctp_corrected_correlated_uncertainty=filter_micro

  !cth
  allocate(l3_macro%cth_mean(nl3x,nl3y))
  l3_macro%cth_mean=filter_micro
  allocate(l3_macro%cth_mean_error(nl3x,nl3y))
  l3_macro%cth_mean_error=filter_micro
  allocate(l3_macro%cth_std(nl3x,nl3y))
  l3_macro%cth_std=filter_micro
  allocate(l3_macro%cth_std_error(nl3x,nl3y))
  l3_macro%cth_std_error=filter_micro
  !   allocate(l3_macro%cth_mean_error_sq(nl3x,nl3y))
  !   l3_macro%cth_mean_error_sq=filter_micro

  allocate(l3_macro%cth_prop_uncertainty(nl3x,nl3y))
  l3_macro%cth_prop_uncertainty=filter_micro
  allocate(l3_macro%cth_correlated_uncertainty(nl3x,nl3y))
  l3_macro%cth_correlated_uncertainty=filter_micro
  !   allocate(l3_macro%cth_v1(nl3x,nl3y))
  !   l3_macro%cth_v1=filter_micro
  !   allocate(l3_macro%cth_v2(nl3x,nl3y))
  !   l3_macro%cth_v2=filter_micro

  !   allocate(l3_macro%cth_median(nl3x,nl3y))
  !   l3_macro%cth_median=filter_micro
  !   allocate(l3_macro%cth_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_macro%cth_md=filter_micro




  !cth_corrected
  allocate(l3_macro%cth_corrected_mean(nl3x,nl3y))
  l3_macro%cth_corrected_mean=filter_micro
  allocate(l3_macro%cth_corrected_mean_error(nl3x,nl3y))
  l3_macro%cth_corrected_mean_error=filter_micro
  allocate(l3_macro%cth_corrected_std(nl3x,nl3y))
  l3_macro%cth_corrected_std=filter_micro
  allocate(l3_macro%cth_corrected_std_error(nl3x,nl3y))
  l3_macro%cth_corrected_std_error=filter_micro
  !   allocate(l3_macro%cth_corrected_mean_error_sq(nl3x,nl3y))
  !   l3_macro%cth_corrected_mean_error_sq=filter_micro

  allocate(l3_macro%cth_corrected_prop_uncertainty(nl3x,nl3y))
  l3_macro%cth_corrected_prop_uncertainty=filter_micro
  allocate(l3_macro%cth_corrected_correlated_uncertainty(nl3x,nl3y))
  l3_macro%cth_corrected_correlated_uncertainty=filter_micro
  !   allocate(l3_macro%cth_corrected_v1(nl3x,nl3y))
  !   l3_macro%cth_corrected_v1=filter_micro
  !   allocate(l3_macro%cth_corrected_v2(nl3x,nl3y))
  !   l3_macro%cth_corrected_v2=filter_micro

  !   allocate(l3_macro%cth_corrected_median(nl3x,nl3y))
  !   l3_macro%cth_corrected_median=filter_micro
  !   allocate(l3_macro%cth_corrected_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_macro%cth_corrected_md=filter_micro








  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l3_macro%cth2_mean(nl3x,nl3y))
     l3_macro%cth2_mean=filter_micro
     !     allocate(l3_macro%cth2_mean_error_sq(nl3x,nl3y))
     !     l3_macro%cth2_mean_error_sq=filter_micro
     allocate(l3_macro%cth2_std(nl3x,nl3y))
     l3_macro%cth2_std=filter_micro

     allocate(l3_macro%cth2_prop_uncertainty(nl3x,nl3y))
     l3_macro%cth2_prop_uncertainty=filter_micro
     allocate(l3_macro%cth2_correlated_uncertainty(nl3x,nl3y))
     l3_macro%cth2_correlated_uncertainty=filter_micro
     allocate(l3_macro%cth2_v1(nl3x,nl3y))
     l3_macro%cth2_v1=filter_micro
     allocate(l3_macro%cth2_v2(nl3x,nl3y))
     l3_macro%cth2_v2=filter_micro

     allocate(l3_macro%cth2_median(nl3x,nl3y))
     l3_macro%cth2_median=filter_micro
     allocate(l3_macro%cth2_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
     l3_macro%cth2_md=filter_micro
     allocate(l3_macro%cth2_mean_error(nl3x,nl3y))
     l3_macro%cth2_mean_error=filter_micro
     allocate(l3_macro%cth2_std_error(nl3x,nl3y))
     l3_macro%cth2_std_error=filter_micro
  endif

  !ctt
  allocate(l3_macro%ctt_mean(nl3x,nl3y))
  l3_macro%ctt_mean=filter_micro
  allocate(l3_macro%ctt_mean_error(nl3x,nl3y))
  l3_macro%ctt_mean_error=filter_micro
  allocate(l3_macro%ctt_std(nl3x,nl3y))
  l3_macro%ctt_std=filter_micro
  allocate(l3_macro%ctt_std_error(nl3x,nl3y))
  l3_macro%ctt_std_error=filter_micro

  allocate(l3_macro%ctt_prop_uncertainty(nl3x,nl3y))
  l3_macro%ctt_prop_uncertainty=filter_micro
  allocate(l3_macro%ctt_correlated_uncertainty(nl3x,nl3y))
  l3_macro%ctt_correlated_uncertainty=filter_micro

  !ctt_corrected
  allocate(l3_macro%ctt_corrected_mean(nl3x,nl3y))
  l3_macro%ctt_corrected_mean=filter_micro
  allocate(l3_macro%ctt_corrected_mean_error(nl3x,nl3y))
  l3_macro%ctt_corrected_mean_error=filter_micro
  allocate(l3_macro%ctt_corrected_std(nl3x,nl3y))
  l3_macro%ctt_corrected_std=filter_micro
  allocate(l3_macro%ctt_corrected_std_error(nl3x,nl3y))
  l3_macro%ctt_corrected_std_error=filter_micro

  allocate(l3_macro%ctt_corrected_prop_uncertainty(nl3x,nl3y))
  l3_macro%ctt_corrected_prop_uncertainty=filter_micro
  allocate(l3_macro%ctt_corrected_correlated_uncertainty(nl3x,nl3y))
  l3_macro%ctt_corrected_correlated_uncertainty=filter_micro


  !stemp
  allocate(l3_macro%stemp_mean(nl3x,nl3y))
  l3_macro%stemp_mean=filter_micro
  allocate(l3_macro%stemp_mean_error(nl3x,nl3y))
  l3_macro%stemp_mean_error=filter_micro
  allocate(l3_macro%stemp_std(nl3x,nl3y))
  l3_macro%stemp_std=filter_micro
  allocate(l3_macro%stemp_std_error(nl3x,nl3y))
  l3_macro%stemp_std_error=filter_micro

  allocate(l3_macro%stemp_prop_uncertainty(nl3x,nl3y))
  l3_macro%stemp_prop_uncertainty=filter_micro
  allocate(l3_macro%stemp_correlated_uncertainty(nl3x,nl3y))
  l3_macro%stemp_correlated_uncertainty=filter_micro

  !stemp clear
  allocate(l3_macro%stemp_clear_mean(nl3x,nl3y))
  l3_macro%stemp_clear_mean=filter_micro
  allocate(l3_macro%stemp_clear_mean_error(nl3x,nl3y))
  l3_macro%stemp_clear_mean_error=filter_micro
  allocate(l3_macro%stemp_clear_std(nl3x,nl3y))
  l3_macro%stemp_clear_std=filter_micro
  allocate(l3_macro%stemp_clear_std_error(nl3x,nl3y))
  l3_macro%stemp_clear_std_error=filter_micro

  allocate(l3_macro%stemp_clear_prop_uncertainty(nl3x,nl3y))
  l3_macro%stemp_clear_prop_uncertainty=filter_micro
  allocate(l3_macro%stemp_clear_correlated_uncertainty(nl3x,nl3y))
  l3_macro%stemp_clear_correlated_uncertainty=filter_micro

  !toa
  allocate(l3_macro%toa_swup_mean(nl3x,nl3y))
  l3_macro%toa_swup_mean=filter_micro
  allocate(l3_macro%toa_swdn_mean(nl3x,nl3y))
  l3_macro%toa_swdn_mean=filter_micro
  allocate(l3_macro%toa_swup_clr_mean(nl3x,nl3y))
  l3_macro%toa_swup_clr_mean=filter_micro
  allocate(l3_macro%toa_lwup_mean(nl3x,nl3y))
  l3_macro%toa_lwup_mean=filter_micro
  allocate(l3_macro%toa_lwup_clr_mean(nl3x,nl3y))
  l3_macro%toa_lwup_clr_mean=filter_micro

  allocate(l3_macro%toa_swup_low_mean(nl3x,nl3y))
  l3_macro%toa_swup_low_mean=filter_micro
  allocate(l3_macro%toa_swup_mid_mean(nl3x,nl3y))
  l3_macro%toa_swup_mid_mean=filter_micro
  allocate(l3_macro%toa_swup_hig_mean(nl3x,nl3y))
  l3_macro%toa_swup_hig_mean=filter_micro
  allocate(l3_macro%toa_lwup_low_mean(nl3x,nl3y))
  l3_macro%toa_lwup_low_mean=filter_micro
  allocate(l3_macro%toa_lwup_mid_mean(nl3x,nl3y))
  l3_macro%toa_lwup_mid_mean=filter_micro
  allocate(l3_macro%toa_lwup_hig_mean(nl3x,nl3y))
  l3_macro%toa_lwup_hig_mean=filter_micro

  !boa
  allocate(l3_macro%boa_swup_mean(nl3x,nl3y))
  l3_macro%boa_swup_mean=filter_micro
  allocate(l3_macro%boa_swup_clr_mean(nl3x,nl3y))
  l3_macro%boa_swup_clr_mean=filter_micro
  allocate(l3_macro%boa_lwup_mean(nl3x,nl3y))
  l3_macro%boa_lwup_mean=filter_micro
  allocate(l3_macro%boa_lwup_clr_mean(nl3x,nl3y))
  l3_macro%boa_lwup_clr_mean=filter_micro
  allocate(l3_macro%boa_swdn_mean(nl3x,nl3y))
  l3_macro%boa_swdn_mean=filter_micro
  allocate(l3_macro%boa_swdn_clr_mean(nl3x,nl3y))
  l3_macro%boa_swdn_clr_mean=filter_micro
  allocate(l3_macro%boa_lwdn_mean(nl3x,nl3y))
  l3_macro%boa_lwdn_mean=filter_micro
  allocate(l3_macro%boa_lwdn_clr_mean(nl3x,nl3y))
  l3_macro%boa_lwdn_clr_mean=filter_micro
  allocate(l3_macro%boa_par_dif_mean(nl3x,nl3y))
  l3_macro%boa_par_dif_mean=filter_micro
  allocate(l3_macro%boa_par_tot_mean(nl3x,nl3y))
  l3_macro%boa_par_tot_mean=filter_micro

  !cct
  allocate(l3_macro%cct_mean(nl3x,nl3y))
  l3_macro%cct_mean=filter_micro
  allocate(l3_macro%cct_std(nl3x,nl3y))
  l3_macro%cct_std=filter_micro

  allocate(l3_macro%cct_mean_error(nl3x,nl3y))
  l3_macro%cct_mean_error=filter_micro
  allocate(l3_macro%cct_std_error(nl3x,nl3y))
  l3_macro%cct_std_error=filter_micro

  allocate(l3_macro%cct_prop_uncertainty(nl3x,nl3y))
  l3_macro%cct_prop_uncertainty=filter_micro
  allocate(l3_macro%cct_correlated_uncertainty(nl3x,nl3y))
  l3_macro%cct_correlated_uncertainty=filter_micro
  !   allocate(l3_macro%cct_v1(nl3x,nl3y))
  !   l3_macro%cct_v1=filter_micro
  !   allocate(l3_macro%  cct_v2(nl3x,nl3y))
  !   l3_macro%cct_v2=filter_micro

  allocate(l3_macro%cct_mean_low(nl3x,nl3y))
  l3_macro%cct_mean_low=filter_micro
  allocate(l3_macro%cct_mean_middle(nl3x,nl3y))
  l3_macro%cct_mean_middle=filter_micro
  allocate(l3_macro%cct_mean_high(nl3x,nl3y))
  l3_macro%cct_mean_high=filter_micro


  !allocate(l3_macro%cct_points_toaboa_sw)
  !l3_macro%cct_points_toaboa_sw=0
  !allocate(l3_macro%cct_points_toaboa_lw)
  !l3_macro%cct_points_toaboa_lw=0

  allocate(l3_macro%cct_points(nl3x,nl3y))
  l3_macro%cct_points=0
  allocate(l3_macro%cct_points_clear(nl3x,nl3y))
  l3_macro%cct_points_clear=0
  allocate(l3_macro%cct_low_points(nl3x,nl3y))
  l3_macro%cct_low_points=0
  allocate(l3_macro%cct_middle_points(nl3x,nl3y))
  l3_macro%cct_middle_points=0
  allocate(l3_macro%cct_high_points(nl3x,nl3y))
  l3_macro%cct_high_points=0


  allocate(l3_macro%cct_points_day(nl3x,nl3y))
  l3_macro%cct_points_day=0
  allocate(l3_macro%cct_points_clear_day(nl3x,nl3y))
  l3_macro%cct_points_clear_day=0

  allocate(l3_macro%cct_points_twl(nl3x,nl3y))
  l3_macro%cct_points_twl=0
  allocate(l3_macro%cct_points_clear_twl(nl3x,nl3y))
  l3_macro%cct_points_clear_twl=0

  allocate(l3_macro%cct_points_night(nl3x,nl3y))
  l3_macro%cct_points_night=0
  allocate(l3_macro%cct_points_clear_night(nl3x,nl3y))
  l3_macro%cct_points_clear_night=0

  allocate(l3_macro%cct_day(nl3x,nl3y))
  l3_macro%cct_day=filter_micro
  allocate(l3_macro%cct_night(nl3x,nl3y))
  l3_macro%cct_night=filter_micro
  allocate(l3_macro%cct_twl(nl3x,nl3y))
  l3_macro%cct_twl=filter_micro

  !stapel raw becomes mean
  allocate(l3_macro%cct_points_raw(nl3x,nl3y))
  l3_macro%cct_points_raw=0
  allocate(l3_macro%cct_points_clear_raw(nl3x,nl3y))
  l3_macro%cct_points_clear_raw=0
  !
  !   allocate(l3_macro%cct_raw(nl3x,nl3y))
  !   l3_macro%cct_raw=filter_micro

  !   allocate(l3_macro%cct_post_points_raw(nl3x,nl3y))
  !   l3_macro%cct_post_points_raw=0
  !   allocate(l3_macro%cct_post_points_clear_raw(nl3x,nl3y))
  !   l3_macro%cct_post_points_clear_raw=0
  !
  !   allocate(l3_macro%cct_post_raw(nl3x,nl3y))
  !   l3_macro%cct_post_raw=filter_micro


  !stapel
  allocate(l3_macro%histogram_ctp_phase(nl3x,nl3y,n_ctp_bins,n_hist_phase))
  l3_macro%histogram_ctp_phase=long_int_zero

  allocate(l3_macro%histogram_ctt_phase(nl3x,nl3y,n_ctt_bins,n_hist_phase))
  l3_macro%histogram_ctt_phase=long_int_zero
  !stapel

  !cee
  allocate(l3_macro%cee_mean(nl3x,nl3y))
  l3_macro%cee_mean=filter_micro
  allocate(l3_macro%cee_mean_error(nl3x,nl3y))
  l3_macro%cee_mean_error=filter_micro
  allocate(l3_macro%cee_std(nl3x,nl3y))
  l3_macro%cee_std=filter_micro
  allocate(l3_macro%cee_std_error(nl3x,nl3y))
  l3_macro%cee_std_error=filter_micro
  allocate(l3_macro%cee_prop_uncertainty(nl3x,nl3y))
  l3_macro%cee_prop_uncertainty=filter_micro
  allocate(l3_macro%cee_correlated_uncertainty(nl3x,nl3y))
  l3_macro%cee_correlated_uncertainty=filter_micro
  allocate(l3_macro%cee_points(nl3x,nl3y))
  l3_macro%cee_points=0

  allocate(l3_macro%rad_points_sw(nl3x,nl3y))
  l3_macro%rad_points_sw=0
  allocate(l3_macro%rad_points_lw(nl3x,nl3y))
  l3_macro%rad_points_lw=0
  allocate(l3_macro%rad_points_sw_low(nl3x,nl3y))
  l3_macro%rad_points_sw_low=0
  allocate(l3_macro%rad_points_lw_low(nl3x,nl3y))
  l3_macro%rad_points_lw_low=0
  allocate(l3_macro%rad_points_sw_mid(nl3x,nl3y))
  l3_macro%rad_points_sw_mid=0
  allocate(l3_macro%rad_points_lw_mid(nl3x,nl3y))
  l3_macro%rad_points_lw_mid=0
  allocate(l3_macro%rad_points_sw_hig(nl3x,nl3y))
  l3_macro%rad_points_sw_hig=0
  allocate(l3_macro%rad_points_lw_hig(nl3x,nl3y))
  l3_macro%rad_points_lw_hig=0  
  
  allocate(l3_macro%stemp_clear_points(nl3x,nl3y))
  l3_macro%stemp_clear_points=0

end subroutine set_l3_macro_struct



!-------------------------------------------
!-------------------------------------------
subroutine set_l3d_macro_input_struct(l3d_macro_input,nl3x,nl3y,nfiles,calgo)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y, nfiles

  type(l3d_macro_input_struct) :: l3d_macro_input

  character (len=paramlength) :: calgo

  allocate(l3d_macro_input%points(nl3x,nl3y,nfiles))
  l3d_macro_input%points=0!long_int_fill_value

  allocate(l3d_macro_input%ctp_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%ctp_mean=filter_micro

  allocate(l3d_macro_input%ctp_corrected_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%ctp_corrected_mean=filter_micro

  allocate(l3d_macro_input%ctp_std(nl3x,nl3y))
  l3d_macro_input%ctp_std=filter_micro

  allocate(l3d_macro_input%ctp_mean_cleared(nl3x,nl3y,nfiles))
  l3d_macro_input%ctp_mean_cleared=filter_micro

  allocate(l3d_macro_input%ctp_phi(nl3x,nl3y,nfiles))
  l3d_macro_input%ctp_phi=filter_micro

  allocate(l3d_macro_input%cth_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%cth_mean=filter_micro

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l3d_macro_input%cth2_mean(nl3x,nl3y,nfiles))
     l3d_macro_input%cth2_mean=filter_micro
  endif

  allocate(l3d_macro_input%cth_corrected_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%cth_corrected_mean=filter_micro

  allocate(l3d_macro_input%ctt_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%ctt_mean=filter_micro

  allocate(l3d_macro_input%ctt_corrected_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%ctt_corrected_mean=filter_micro

  allocate(l3d_macro_input%stemp_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%stemp_mean=filter_micro

  allocate(l3d_macro_input%cct_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%cct_mean=filter_micro

  allocate(l3d_macro_input%cee_mean(nl3x,nl3y,nfiles))
  l3d_macro_input%cee_mean=filter_micro

end subroutine set_l3d_macro_input_struct


!-------------------------------------------
!-------------------------------------------
subroutine set_l3d_macro_struct(l3d_macro,nl3x,nl3y,calgo)
  !-------------------------------------------_
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y

  type(l3d_macro_struct) :: l3d_macro

  character (len=paramlength) :: calgo

  allocate(l3d_macro%points(nl3x,nl3y))
  l3d_macro%points=0!long_int_fill_value

  allocate(l3d_macro%pairs(nl3x,nl3y))
  l3d_macro%pairs=0!long_int_fill_value

  allocate(l3d_macro%ctp_mean(nl3x,nl3y))
  l3d_macro%ctp_mean=filter_micro

  allocate(l3d_macro%ctp_std(nl3x,nl3y))
  l3d_macro%ctp_std=filter_micro

  allocate(l3d_macro%ctp_std_corr(nl3x,nl3y))
  l3d_macro%ctp_std_corr=filter_micro

  allocate(l3d_macro%ctp_phi_total(nl3x,nl3y))
  l3d_macro%ctp_phi_total=filter_micro

  allocate(l3d_macro%cth_mean(nl3x,nl3y))
  l3d_macro%cth_mean=filter_micro

  allocate(l3d_macro%cth_corrected_mean(nl3x,nl3y))
  l3d_macro%cth_corrected_mean=filter_micro

  allocate(l3d_macro%ctp_corrected_mean(nl3x,nl3y))
  l3d_macro%ctp_corrected_mean=filter_micro

  allocate(l3d_macro%ctt_corrected_mean(nl3x,nl3y))
  l3d_macro%ctt_corrected_mean=filter_micro

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l3d_macro%cth2_mean(nl3x,nl3y))
     l3d_macro%cth2_mean=filter_micro
  endif

  allocate(l3d_macro%ctt_mean(nl3x,nl3y))
  l3d_macro%ctt_mean=filter_micro



  allocate(l3d_macro%stemp_mean(nl3x,nl3y))
  l3d_macro%stemp_mean=filter_micro


  allocate(l3d_macro%cct_mean(nl3x,nl3y))
  l3d_macro%cct_mean=filter_micro

end subroutine set_l3d_macro_struct





!-------------------------------------------
!-------------------------------------------
! subroutine set_l3_micro_struct(l3_micro,nl3x,nl3y,n_bins,nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features)
subroutine set_l3_micro_struct(l3_micro,nl3x,nl3y)
  !-------------------------------------------
  !-------------------------------------------
  !   integer(kind=lint), dimension(n_methods, n_variables, n_features,nl3x,nl3y) :: l3_n_points
  !   real(kind=sreal), dimension(n_methods,n_variables,n_features,nl3x,nl3y) :: l3_vector
  !   real(kind=sreal), dimension(n_methods,n_variables,n_bins+2,nl3x,nl3y) :: l3_histogram
  !   real(kind=sreal), dimension(n_methods,n_variables,n_bins_power,nl3x,nl3y) :: l3_power_spec
  !   real(kind=sreal), dimension(n_methods,nl3x,nl3y) :: l3_f_para

  !   real(kind=sreal), dimension(n_methods,n_variables,n_error_features,nl3x,nl3y) :: l3_errors

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y!, n_bins

  !   integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features

  type(l3_micro_struct) :: l3_micro

  !for the new micro cloud cover
  allocate(l3_micro%cct_points(nl3x,nl3y))
  l3_micro%cct_points=0
  allocate(l3_micro%cct_points_clear(nl3x,nl3y))
  l3_micro%cct_points_clear=0

  allocate(l3_micro%cct_mean(nl3x,nl3y))
  l3_micro%cct_mean=filter_micro
  allocate(l3_micro%cct_std(nl3x,nl3y))
  l3_micro%cct_std=filter_micro

  !liquid and ice cloud cover
  allocate(l3_micro%cct_liq(nl3x,nl3y))
  l3_micro%cct_liq=filter_micro
  allocate(l3_micro%cct_ice(nl3x,nl3y))
  l3_micro%cct_ice=filter_micro


  allocate(l3_micro%points(nl3x,nl3y))
  l3_micro%points=0!long_int_fill_value

  allocate(l3_micro%clear_points(nl3x,nl3y))
  l3_micro%clear_points=0!long_int_fill_value

  allocate(l3_micro%cloudy_points(nl3x,nl3y))
  l3_micro%cloudy_points=0!long_int_fill_value

  allocate(l3_micro%cloudy_points_liq(nl3x,nl3y))
  l3_micro%cloudy_points_liq=0!long_int_fill_value

  allocate(l3_micro%cloudy_points_ice(nl3x,nl3y))
  l3_micro%cloudy_points_ice=0!long_int_fill_value

  !   allocate(l3_micro%remedian_counter(nl3x,nl3y,n_remedian_exponent))
  !   l3_micro%remedian_counter=0

  !cot
  allocate(l3_micro%cot_mean(nl3x,nl3y))
  l3_micro%cot_mean=filter_micro
  !   allocate(l3_micro%cot_mean_error_sq(nl3x,nl3y))
  !   l3_micro%cot_mean_error_sq=filter_micro

  allocate(l3_micro%cot_log_mean(nl3x,nl3y))
  l3_micro%cot_log_mean=filter_micro
  allocate(l3_micro%cot_std(nl3x,nl3y))
  l3_micro%cot_std=filter_micro


  allocate(l3_micro%cot_prop_uncertainty(nl3x,nl3y))
  l3_micro%cot_prop_uncertainty=filter_micro
  allocate(l3_micro%cot_correlated_uncertainty(nl3x,nl3y))
  l3_micro%cot_correlated_uncertainty=filter_micro
  !   allocate(l3_micro%cot_v1(nl3x,nl3y))
  !   l3_micro%cot_v1=filter_micro
  !   allocate(l3_micro%cot_v2(nl3x,nl3y))
  !   l3_micro%cot_v2=filter_micro

  !   allocate(l3_micro%cot_median(nl3x,nl3y))
  !   l3_micro%cot_median=filter_micro
  allocate(l3_micro%cot_mean_error(nl3x,nl3y))
  l3_micro%cot_mean_error=filter_micro
  allocate(l3_micro%cot_std_error(nl3x,nl3y))
  l3_micro%cot_std_error=filter_micro
  !   allocate(l3_micro%cot_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_micro%cot_md=filter_micro


  !ref
  allocate(l3_micro%ref_mean(nl3x,nl3y))
  l3_micro%ref_mean=filter_micro
  !   allocate(l3_micro%ref_mean_error_sq(nl3x,nl3y))
  !   l3_micro%ref_mean_error_sq=filter_micro
  allocate(l3_micro%ref_std(nl3x,nl3y))
  l3_micro%ref_std=filter_micro

  allocate(l3_micro%ref_prop_uncertainty(nl3x,nl3y))
  l3_micro%ref_prop_uncertainty=filter_micro
  allocate(l3_micro%ref_correlated_uncertainty(nl3x,nl3y))
  l3_micro%ref_correlated_uncertainty=filter_micro
  !   allocate(l3_micro%ref_v1(nl3x,nl3y))
  !   l3_micro%ref_v1=filter_micro
  !   allocate(l3_micro%ref_v2(nl3x,nl3y))
  !   l3_micro%ref_v2=filter_micro

  !   allocate(l3_micro%ref_median(nl3x,nl3y))
  !   l3_micro%ref_median=filter_micro
  allocate(l3_micro%ref_mean_error(nl3x,nl3y))
  l3_micro%ref_mean_error=filter_micro
  allocate(l3_micro%ref_std_error(nl3x,nl3y))
  l3_micro%ref_std_error=filter_micro
  !   allocate(l3_micro%ref_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_micro%ref_md=filter_micro


  !cwp
  allocate(l3_micro%cwp_mean(nl3x,nl3y))
  l3_micro%cwp_mean=filter_micro
  !   allocate(l3_micro%cwp_mean_error_sq(nl3x,nl3y))
  !   l3_micro%cwp_mean_error_sq=filter_micro
  allocate(l3_micro%cwp_std(nl3x,nl3y))
  l3_micro%cwp_std=filter_micro

  allocate(l3_micro%cwp_prop_uncertainty(nl3x,nl3y))
  l3_micro%cwp_prop_uncertainty=filter_micro
  allocate(l3_micro%cwp_correlated_uncertainty(nl3x,nl3y))
  l3_micro%cwp_correlated_uncertainty=filter_micro
  !   allocate(l3_micro%cwp_v1(nl3x,nl3y))
  !   l3_micro%cwp_v1=filter_micro
  !   allocate(l3_micro%cwp_v2(nl3x,nl3y))
  !   l3_micro%cwp_v2=filter_micro

  !   allocate(l3_micro%cwp_median(nl3x,nl3y))
  !   l3_micro%cwp_median=filter_micro
  allocate(l3_micro%cwp_mean_error(nl3x,nl3y))
  l3_micro%cwp_mean_error=filter_micro
  allocate(l3_micro%cwp_std_error(nl3x,nl3y))
  l3_micro%cwp_std_error=filter_micro
  !   allocate(l3_micro%cwp_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_micro%cwp_md=filter_micro


  !cwp liq.
  allocate(l3_micro%cwp_mean_liq(nl3x,nl3y))
  l3_micro%cwp_mean_liq = filter_micro
  allocate(l3_micro%lwp_allsky(nl3x,nl3y))
  l3_micro%lwp_allsky = filter_micro
  !   allocate(l3_micro%cwp_mean_error_liq_sq(nl3x,nl3y))
  !   l3_micro%cwp_mean_error_liq_sq=filter_micro
  allocate(l3_micro%cwp_std_liq(nl3x,nl3y))
  l3_micro%cwp_std_liq=filter_micro

  allocate(l3_micro%cwp_prop_uncertainty_liq(nl3x,nl3y))
  l3_micro%cwp_prop_uncertainty_liq=filter_micro
  allocate(l3_micro%cwp_correlated_uncertainty_liq(nl3x,nl3y))
  l3_micro%cwp_correlated_uncertainty_liq=filter_micro
  !   allocate(l3_micro%cwp_v1_liq(nl3x,nl3y))
  !   l3_micro%cwp_v1_liq=filter_micro
  !   allocate(l3_micro%cwp_v2_liq(nl3x,nl3y))
  !   l3_micro%cwp_v2_liq=filter_micro

  !  allocate(l3_micro%cwp_median_liq(nl3x,nl3y))
  !  l3_micro%cwp_median_liq=filter_micro
  allocate(l3_micro%cwp_mean_error_liq(nl3x,nl3y))
  l3_micro%cwp_mean_error_liq=filter_micro
  allocate(l3_micro%cwp_std_error_liq(nl3x,nl3y))
  l3_micro%cwp_std_error_liq=filter_micro
  ! allocate(l3_micro%cwp_md_liq(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cwp_md_liq=filter_micro


  !cwp ice
  allocate(l3_micro%cwp_mean_ice(nl3x,nl3y))
  l3_micro%cwp_mean_ice = filter_micro
  allocate(l3_micro%iwp_allsky(nl3x,nl3y))
  l3_micro%iwp_allsky = filter_micro
  !   allocate(l3_micro%cwp_mean_error_ice_sq(nl3x,nl3y))
  !   l3_micro%cwp_mean_error_ice_sq=filter_micro
  allocate(l3_micro%cwp_std_ice(nl3x,nl3y))
  l3_micro%cwp_std_ice=filter_micro

  allocate(l3_micro%cwp_prop_uncertainty_ice(nl3x,nl3y))
  l3_micro%cwp_prop_uncertainty_ice=filter_micro
  allocate(l3_micro%cwp_correlated_uncertainty_ice(nl3x,nl3y))
  l3_micro%cwp_correlated_uncertainty_ice=filter_micro
  !   allocate(l3_micro%cwp_v1_ice(nl3x,nl3y))
  !   l3_micro%cwp_v1_ice=filter_micro
  !   allocate(l3_micro%cwp_v2_ice(nl3x,nl3y))
  !   l3_micro%cwp_v2_ice=filter_micro

  !  allocate(l3_micro%cwp_median_ice(nl3x,nl3y))
  !  l3_micro%cwp_median_ice=filter_micro
  allocate(l3_micro%cwp_mean_error_ice(nl3x,nl3y))
  l3_micro%cwp_mean_error_ice=filter_micro
  allocate(l3_micro%cwp_std_error_ice(nl3x,nl3y))
  l3_micro%cwp_std_error_ice=filter_micro
  !  allocate(l3_micro%cwp_md_ice(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cwp_md_ice=filter_micro









  !cloud_albedo2
  allocate(l3_micro%cloud_albedo2_mean(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean=filter_micro
  !   allocate(l3_micro%cloud_albedo2_mean_error_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_mean_error_sq=filter_micro
  allocate(l3_micro%cloud_albedo2_std(nl3x,nl3y))
  l3_micro%cloud_albedo2_std=filter_micro

  allocate(l3_micro%cloud_albedo2_prop_uncertainty(nl3x,nl3y))
  l3_micro%cloud_albedo2_prop_uncertainty=filter_micro
  allocate(l3_micro%cloud_albedo2_correlated_uncertainty(nl3x,nl3y))
  l3_micro%cloud_albedo2_correlated_uncertainty=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v1(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v1=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v2(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v2=filter_micro

  !   allocate(l3_micro%cloud_albedo2_median(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_median=filter_micro
  allocate(l3_micro%cloud_albedo2_mean_error(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean_error=filter_micro
  allocate(l3_micro%cloud_albedo2_std_error(nl3x,nl3y))
  l3_micro%cloud_albedo2_std_error=filter_micro
  !   allocate(l3_micro%cloud_albedo2_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_micro%cloud_albedo2_md=filter_micro


  !cloud_albedo2 liq.
  allocate(l3_micro%cloud_albedo2_mean_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo2_mean_error_liq_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_mean_error_liq_sq=filter_micro
  allocate(l3_micro%cloud_albedo2_std_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_std_liq=filter_micro

  allocate(l3_micro%cloud_albedo2_prop_uncertainty_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_prop_uncertainty_liq=filter_micro
  allocate(l3_micro%cloud_albedo2_correlated_uncertainty_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_correlated_uncertainty_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v1_liq(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v1_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v2_liq(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v2_liq=filter_micro

  !  allocate(l3_micro%cloud_albedo2_median_liq(nl3x,nl3y))
  !  l3_micro%cloud_albedo2_median_liq=filter_micro
  allocate(l3_micro%cloud_albedo2_mean_error_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean_error_liq=filter_micro
  allocate(l3_micro%cloud_albedo2_std_error_liq(nl3x,nl3y))
  l3_micro%cloud_albedo2_std_error_liq=filter_micro
  ! allocate(l3_micro%cloud_albedo2_md_liq(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cloud_albedo2_md_liq=filter_micro


  !cloud_albedo2 ice
  allocate(l3_micro%cloud_albedo2_mean_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo2_mean_error_ice_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_mean_error_ice_sq=filter_micro
  allocate(l3_micro%cloud_albedo2_std_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_std_ice=filter_micro

  allocate(l3_micro%cloud_albedo2_prop_uncertainty_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_prop_uncertainty_ice=filter_micro
  allocate(l3_micro%cloud_albedo2_correlated_uncertainty_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_correlated_uncertainty_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v1_ice(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v1_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo2_v2_ice(nl3x,nl3y))
  !   l3_micro%cloud_albedo2_v2_ice=filter_micro

  !  allocate(l3_micro%cloud_albedo2_median_ice(nl3x,nl3y))
  !  l3_micro%cloud_albedo2_median_ice=filter_micro
  allocate(l3_micro%cloud_albedo2_mean_error_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_mean_error_ice=filter_micro
  allocate(l3_micro%cloud_albedo2_std_error_ice(nl3x,nl3y))
  l3_micro%cloud_albedo2_std_error_ice=filter_micro
  !  allocate(l3_micro%cloud_albedo2_md_ice(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cloud_albedo2_md_ice=filter_micro











  !cloud_albedo1
  allocate(l3_micro%cloud_albedo1_mean(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean=filter_micro
  !   allocate(l3_micro%cloud_albedo1_mean_error_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_mean_error_sq=filter_micro
  allocate(l3_micro%cloud_albedo1_std(nl3x,nl3y))
  l3_micro%cloud_albedo1_std=filter_micro

  allocate(l3_micro%cloud_albedo1_prop_uncertainty(nl3x,nl3y))
  l3_micro%cloud_albedo1_prop_uncertainty=filter_micro
  allocate(l3_micro%cloud_albedo1_correlated_uncertainty(nl3x,nl3y))
  l3_micro%cloud_albedo1_correlated_uncertainty=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v1(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v1=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v2(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v2=filter_micro

  !   allocate(l3_micro%cloud_albedo1_median(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_median=filter_micro
  allocate(l3_micro%cloud_albedo1_mean_error(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean_error=filter_micro
  allocate(l3_micro%cloud_albedo1_std_error(nl3x,nl3y))
  l3_micro%cloud_albedo1_std_error=filter_micro
  !   allocate(l3_micro%cloud_albedo1_md(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !   l3_micro%cloud_albedo1_md=filter_micro


  !cloud_albedo1 liq.
  allocate(l3_micro%cloud_albedo1_mean_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo1_mean_error_liq_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_mean_error_liq_sq=filter_micro
  allocate(l3_micro%cloud_albedo1_std_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_std_liq=filter_micro

  allocate(l3_micro%cloud_albedo1_prop_uncertainty_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_prop_uncertainty_liq=filter_micro
  allocate(l3_micro%cloud_albedo1_correlated_uncertainty_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_correlated_uncertainty_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v1_liq(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v1_liq=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v2_liq(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v2_liq=filter_micro

  !  allocate(l3_micro%cloud_albedo1_median_liq(nl3x,nl3y))
  !  l3_micro%cloud_albedo1_median_liq=filter_micro
  allocate(l3_micro%cloud_albedo1_mean_error_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean_error_liq=filter_micro
  allocate(l3_micro%cloud_albedo1_std_error_liq(nl3x,nl3y))
  l3_micro%cloud_albedo1_std_error_liq=filter_micro
  ! allocate(l3_micro%cloud_albedo1_md_liq(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cloud_albedo1_md_liq=filter_micro


  !cloud_albedo1 ice
  allocate(l3_micro%cloud_albedo1_mean_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo1_mean_error_ice_sq(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_mean_error_ice_sq=filter_micro
  allocate(l3_micro%cloud_albedo1_std_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_std_ice=filter_micro

  allocate(l3_micro%cloud_albedo1_prop_uncertainty_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_prop_uncertainty_ice=filter_micro
  allocate(l3_micro%cloud_albedo1_correlated_uncertainty_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_correlated_uncertainty_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v1_ice(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v1_ice=filter_micro
  !   allocate(l3_micro%cloud_albedo1_v2_ice(nl3x,nl3y))
  !   l3_micro%cloud_albedo1_v2_ice=filter_micro

  !  allocate(l3_micro%cloud_albedo1_median_ice(nl3x,nl3y))
  !  l3_micro%cloud_albedo1_median_ice=filter_micro
  allocate(l3_micro%cloud_albedo1_mean_error_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_mean_error_ice=filter_micro
  allocate(l3_micro%cloud_albedo1_std_error_ice(nl3x,nl3y))
  l3_micro%cloud_albedo1_std_error_ice=filter_micro
  !  allocate(l3_micro%cloud_albedo1_md_ice(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cloud_albedo1_md_ice=filter_micro













  !ref liq.
  allocate(l3_micro%ref_mean_liq(nl3x,nl3y))
  l3_micro%ref_mean_liq=filter_micro
  !   allocate(l3_micro%ref_mean_error_liq_sq(nl3x,nl3y))
  !   l3_micro%ref_mean_error_liq_sq=filter_micro
  allocate(l3_micro%ref_std_liq(nl3x,nl3y))
  l3_micro%ref_std_liq=filter_micro

  allocate(l3_micro%ref_prop_uncertainty_liq(nl3x,nl3y))
  l3_micro%ref_prop_uncertainty_liq=filter_micro
  allocate(l3_micro%ref_correlated_uncertainty_liq(nl3x,nl3y))
  l3_micro%ref_correlated_uncertainty_liq=filter_micro
  !   allocate(l3_micro%ref_v1_liq(nl3x,nl3y))
  !   l3_micro%ref_v1_liq=filter_micro
  !   allocate(l3_micro%ref_v2_liq(nl3x,nl3y))
  !   l3_micro%ref_v2_liq=filter_micro

  !  allocate(l3_micro%ref_median_liq(nl3x,nl3y))
  !  l3_micro%ref_median_liq=filter_micro
  allocate(l3_micro%ref_mean_error_liq(nl3x,nl3y))
  l3_micro%ref_mean_error_liq=filter_micro
  allocate(l3_micro%ref_std_error_liq(nl3x,nl3y))
  l3_micro%ref_std_error_liq=filter_micro
  ! allocate(l3_micro%ref_md_liq(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%ref_md_liq=filter_micro

  !ref ice
  allocate(l3_micro%ref_mean_ice(nl3x,nl3y))
  l3_micro%ref_mean_ice=filter_micro
  !   allocate(l3_micro%ref_mean_error_ice_sq(nl3x,nl3y))
  !   l3_micro%ref_mean_error_ice_sq=filter_micro
  allocate(l3_micro%ref_std_ice(nl3x,nl3y))
  l3_micro%ref_std_ice=filter_micro

  allocate(l3_micro%ref_prop_uncertainty_ice(nl3x,nl3y))
  l3_micro%ref_prop_uncertainty_ice=filter_micro
  allocate(l3_micro%ref_correlated_uncertainty_ice(nl3x,nl3y))
  l3_micro%ref_correlated_uncertainty_ice=filter_micro
  !   allocate(l3_micro%ref_v1_ice(nl3x,nl3y))
  !   l3_micro%ref_v1_ice=filter_micro
  !   allocate(l3_micro%ref_v2_ice(nl3x,nl3y))
  !   l3_micro%ref_v2_ice=filter_micro

  !  allocate(l3_micro%ref_median_ice(nl3x,nl3y))
  !  l3_micro%ref_median_ice=filter_micro
  allocate(l3_micro%ref_mean_error_ice(nl3x,nl3y))
  l3_micro%ref_mean_error_ice=filter_micro
  allocate(l3_micro%ref_std_error_ice(nl3x,nl3y))
  l3_micro%ref_std_error_ice=filter_micro
  !  allocate(l3_micro%ref_md_ice(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%ref_md_ice=filter_micro



  !cot liq.
  allocate(l3_micro%cot_mean_liq(nl3x,nl3y))
  l3_micro%cot_mean_liq=filter_micro
  !   allocate(l3_micro%cot_mean_error_liq_sq(nl3x,nl3y))
  !   l3_micro%cot_mean_error_liq_sq=filter_micro
  allocate(l3_micro%cot_std_liq(nl3x,nl3y))
  l3_micro%cot_std_liq=filter_micro

  allocate(l3_micro%cot_prop_uncertainty_liq(nl3x,nl3y))
  l3_micro%cot_prop_uncertainty_liq=filter_micro
  allocate(l3_micro%cot_correlated_uncertainty_liq(nl3x,nl3y))
  l3_micro%cot_correlated_uncertainty_liq=filter_micro
  !   allocate(l3_micro%cot_v1_liq(nl3x,nl3y))
  !   l3_micro%cot_v1_liq=filter_micro
  !   allocate(l3_micro%cot_v2_liq(nl3x,nl3y))
  !   l3_micro%cot_v2_liq=filter_micro

  !  allocate(l3_micro%cot_median_liq(nl3x,nl3y))
  !  l3_micro%cot_median_liq=filter_micro
  allocate(l3_micro%cot_mean_error_liq(nl3x,nl3y))
  l3_micro%cot_mean_error_liq=filter_micro
  allocate(l3_micro%cot_std_error_liq(nl3x,nl3y))
  l3_micro%cot_std_error_liq=filter_micro
  ! allocate(l3_micro%cot_md_liq(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cot_md_liq=filter_micro

  !cot ice
  allocate(l3_micro%cot_mean_ice(nl3x,nl3y))
  l3_micro%cot_mean_ice=filter_micro
  !   allocate(l3_micro%cot_mean_error_ice_sq(nl3x,nl3y))
  !   l3_micro%cot_mean_error_ice_sq=filter_micro
  allocate(l3_micro%cot_std_ice(nl3x,nl3y))
  l3_micro%cot_std_ice=filter_micro

  allocate(l3_micro%cot_prop_uncertainty_ice(nl3x,nl3y))
  l3_micro%cot_prop_uncertainty_ice=filter_micro
  allocate(l3_micro%cot_correlated_uncertainty_ice(nl3x,nl3y))
  l3_micro%cot_correlated_uncertainty_ice=filter_micro
  !   allocate(l3_micro%cot_v1_ice(nl3x,nl3y))
  !   l3_micro%cot_v1_ice=filter_micro
  !   allocate(l3_micro%cot_v2_ice(nl3x,nl3y))
  !   l3_micro%cot_v2_ice=filter_micro

  !  allocate(l3_micro%cot_median_ice(nl3x,nl3y))
  !  l3_micro%cot_median_ice=filter_micro
  allocate(l3_micro%cot_mean_error_ice(nl3x,nl3y))
  l3_micro%cot_mean_error_ice=filter_micro
  allocate(l3_micro%cot_std_error_ice(nl3x,nl3y))
  l3_micro%cot_std_error_ice=filter_micro
  !  allocate(l3_micro%cot_md_ice(nl3x,nl3y, n_remedian_base, n_remedian_exponent+1))
  !  l3_micro%cot_md_ice=filter_micro

  !cty_day
  allocate(l3_micro%cty_mean(nl3x,nl3y))
  l3_micro%cty_mean=filter_micro
  allocate(l3_micro%cty_std(nl3x,nl3y))
  l3_micro%cty_std=filter_micro
  allocate(l3_micro%cty_mean_error(nl3x,nl3y))
  l3_micro%cty_mean_error=filter_micro
  allocate(l3_micro%cty_std_error(nl3x,nl3y))
  l3_micro%cty_std_error=filter_micro
  allocate(l3_micro%cty_prop_uncertainty(nl3x,nl3y))
  l3_micro%cty_prop_uncertainty=filter_micro
  allocate(l3_micro%cty_correlated_uncertainty(nl3x,nl3y))
  l3_micro%cty_correlated_uncertainty=filter_micro

  allocate(l3_micro%histogram_cot_ctp_phase(nl3x,nl3y,n_hist_cot-1,n_hist_ctp-1,n_hist_phase))
  l3_micro%histogram_cot_ctp_phase=long_int_zero

  !stapel
  allocate(l3_micro%histogram_cot_phase(nl3x,nl3y,n_cot_bins,n_hist_phase))
  l3_micro%histogram_cot_phase=long_int_zero

  allocate(l3_micro%histogram_ref_phase(nl3x,nl3y,n_ref_bins,n_hist_phase))
  l3_micro%histogram_ref_phase=long_int_zero

  allocate(l3_micro%histogram_cwp_phase(nl3x,nl3y,n_cwp_bins,n_hist_phase))
  l3_micro%histogram_cwp_phase=long_int_zero



  allocate(l3_micro%histogram_cloud_albedo2_phase(nl3x,nl3y,n_cloud_albedo2_bins,n_hist_phase))
  l3_micro%histogram_cloud_albedo2_phase=long_int_zero


  allocate(l3_micro%histogram_cloud_albedo1_phase(nl3x,nl3y,n_cloud_albedo1_bins,n_hist_phase))
  l3_micro%histogram_cloud_albedo1_phase=long_int_zero
  !stapel

end subroutine set_l3_micro_struct


!-------------------------------------------
!-------------------------------------------
subroutine set_l3d_micro_struct(l3d_micro,nl3x,nl3y)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y

  type(l3d_micro_struct) :: l3d_micro

  allocate(l3d_micro%points(nl3x,nl3y))
  l3d_micro%points=0!long_int_fill_value

  allocate(l3d_micro%pairs(nl3x,nl3y))
  l3d_micro%pairs=0!long_int_fill_value

  allocate(l3d_micro%cot_mean(nl3x,nl3y))
  l3d_micro%cot_mean=filter_micro

  allocate(l3d_micro%cot_std(nl3x,nl3y))
  l3d_micro%cot_std=filter_micro

  allocate(l3d_micro%cot_std_corr(nl3x,nl3y))
  l3d_micro%cot_std_corr=filter_micro

  allocate(l3d_micro%cot_phi_total(nl3x,nl3y))
  l3d_micro%cot_phi_total=filter_micro


  allocate(l3d_micro%ref_mean(nl3x,nl3y))
  l3d_micro%ref_mean=filter_micro

  allocate(l3d_micro%cwp_mean(nl3x,nl3y))
  l3d_micro%cwp_mean=filter_micro



  allocate(l3d_micro%cloud_albedo1_mean(nl3x,nl3y))
  l3d_micro%cloud_albedo1_mean=filter_micro


  allocate(l3d_micro%cloud_albedo2_mean(nl3x,nl3y))
  l3d_micro%cloud_albedo2_mean=filter_micro



  allocate(l3d_micro%cty_mean(nl3x,nl3y))
  l3d_micro%cty_mean=filter_micro

end subroutine set_l3d_micro_struct




!-------------------------------------------
!-------------------------------------------
subroutine set_l3d_micro_input_struct(l3d_micro_input,nl3x,nl3y, nfiles)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: nl3x,nl3y, nfiles

  type(l3d_micro_input_struct) :: l3d_micro_input

  allocate(l3d_micro_input%points(nl3x,nl3y,nfiles))
  l3d_micro_input%points=0!long_int_fill_value


  allocate(l3d_micro_input%cot_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%cot_mean=filter_micro

  allocate(l3d_micro_input%cot_std(nl3x,nl3y))
  l3d_micro_input%cot_std=filter_micro

  allocate(l3d_micro_input%cot_mean_cleared(nl3x,nl3y,nfiles))
  l3d_micro_input%cot_mean_cleared=filter_micro

  allocate(l3d_micro_input%cot_phi(nl3x,nl3y,nfiles))
  l3d_micro_input%cot_phi=filter_micro

  allocate(l3d_micro_input%ref_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%ref_mean=filter_micro

  allocate(l3d_micro_input%cwp_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%cwp_mean=filter_micro



  allocate(l3d_micro_input%cloud_albedo1_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%cloud_albedo1_mean=filter_micro


  allocate(l3d_micro_input%cloud_albedo2_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%cloud_albedo2_mean=filter_micro

  allocate(l3d_micro_input%cty_mean(nl3x,nl3y,nfiles))
  l3d_micro_input%cty_mean=filter_micro

end subroutine set_l3d_micro_input_struct



!-------------------------------------------
!-------------------------------------------
subroutine set_l1_input_struct_2d(l1_input_2d,xdim1km,ydim1km)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: xdim1km,ydim1km

  !   integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error, n_oe_features

  type(l1_input_struct_2d) :: l1_input_2d

  allocate(l1_input_2d%refl_1(xdim1km,ydim1km))
  l1_input_2d%refl_1=real_fill_value

end subroutine set_l1_input_struct_2d

!-------------------------------------------
!-------------------------------------------
subroutine unset_l1_input_struct_2d(l1_input_2d)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l1_input_struct_2d) :: l1_input_2d

  deallocate(l1_input_2d%refl_1)

end subroutine unset_l1_input_struct_2d



!-------------------------------------------
!-------------------------------------------
subroutine set_l2_input_struct_2d(l2_input_2d,xdim1km,ydim1km,nnodes,nl2vars_1km,nl2vars_errors_1km, &
     & n_val_plus_error,n_oe_features,calgo,llocal)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: xdim1km,ydim1km,nnodes

  integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error, n_oe_features

  type(l2_input_struct_2d) :: l2_input_2d

  character (len=paramlength) :: calgo

  integer :: ierr

  logical,intent(in) :: llocal

  allocate(l2_input_2d%time(xdim1km,ydim1km,nnodes),stat = ierr)
  l2_input_2d%time=double_fill_value

  allocate(l2_input_2d%lon(xdim1km,ydim1km),stat = ierr)
  l2_input_2d%lon=real_fill_value

  allocate(l2_input_2d%lat(xdim1km,ydim1km),stat = ierr)
  l2_input_2d%lat=real_fill_value

  allocate(l2_input_2d%satzen(xdim1km,ydim1km),stat = ierr)
  l2_input_2d%satzen=real_fill_value

  allocate(l2_input_2d%solzen(xdim1km,ydim1km))
  l2_input_2d%solzen=real_fill_value

  allocate(l2_input_2d%relazi(xdim1km,ydim1km))
  l2_input_2d%relazi=real_fill_value

  allocate(l2_input_2d%ctt(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctt=real_fill_value

  allocate(l2_input_2d%ctt_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctt_error=real_fill_value

  allocate(l2_input_2d%ctt_corrected(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctt_corrected=real_fill_value

  allocate(l2_input_2d%ctt_corrected_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctt_corrected_error=real_fill_value

  !   allocate(l2_input_2d%stemp(xdim1km,ydim1km,nnodes))
  !   l2_input_2d%stemp=real_fill_value

  !   allocate(l2_input_2d%stemp_error(xdim1km,ydim1km,nnodes))
  !   l2_input_2d%stemp_error=real_fill_value

  allocate(l2_input_2d%cth(xdim1km,ydim1km,nnodes))
  l2_input_2d%cth=real_fill_value

  allocate(l2_input_2d%cth_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cth_error=real_fill_value

  allocate(l2_input_2d%cth_corrected(xdim1km,ydim1km,nnodes))
  l2_input_2d%cth_corrected=real_fill_value

  allocate(l2_input_2d%cth_corrected_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cth_corrected_error=real_fill_value

  allocate(l2_input_2d%ctp(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctp=real_fill_value

  allocate(l2_input_2d%ctp_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctp_error=real_fill_value

  allocate(l2_input_2d%ctp_corrected(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctp_corrected=real_fill_value

  allocate(l2_input_2d%ctp_corrected_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%ctp_corrected_error=real_fill_value

  allocate(l2_input_2d%cct(xdim1km,ydim1km,nnodes))
  l2_input_2d%cct=real_fill_value

  allocate(l2_input_2d%cct_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cct_error=real_fill_value

  allocate(l2_input_2d%cct_pre(xdim1km,ydim1km,nnodes))
  l2_input_2d%cct_pre=real_fill_value

  allocate(l2_input_2d%cct_post(xdim1km,ydim1km,nnodes))
  l2_input_2d%cct_post=real_fill_value

  allocate(l2_input_2d%cot(xdim1km,ydim1km,nnodes))
  l2_input_2d%cot=real_fill_value

  allocate(l2_input_2d%cot_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cot_error=real_fill_value

  allocate(l2_input_2d%cccot(xdim1km,ydim1km,nnodes))
  l2_input_2d%cccot=real_fill_value

  allocate(l2_input_2d%cccot_pre(xdim1km,ydim1km,nnodes))
  l2_input_2d%cccot_pre=real_fill_value

  allocate(l2_input_2d%ann_phase_uncertainty(xdim1km,ydim1km,nnodes))
  l2_input_2d%ann_phase_uncertainty=real_fill_value

  allocate(l2_input_2d%cphcot(xdim1km,ydim1km,nnodes))
  l2_input_2d%cphcot=real_fill_value

  allocate(l2_input_2d%ref(xdim1km,ydim1km,nnodes))
  l2_input_2d%ref=real_fill_value

  allocate(l2_input_2d%ref_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%ref_error=real_fill_value

  allocate(l2_input_2d%cty(xdim1km,ydim1km,nnodes))
  l2_input_2d%cty=real_fill_value

  allocate(l2_input_2d%cty_pre(xdim1km,ydim1km,nnodes))
  l2_input_2d%cty_pre=real_fill_value

  allocate(l2_input_2d%cty_post(xdim1km,ydim1km,nnodes))
  l2_input_2d%cty_post=real_fill_value

  allocate(l2_input_2d%cty_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cty_error=real_fill_value

  allocate(l2_input_2d%cwp(xdim1km,ydim1km,nnodes))
  l2_input_2d%cwp=real_fill_value

  allocate(l2_input_2d%cwp_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cwp_error=real_fill_value

  allocate(l2_input_2d%cloud_albedo1(xdim1km,ydim1km,nnodes))
  l2_input_2d%cloud_albedo1=real_fill_value

  allocate(l2_input_2d%cloud_albedo1_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cloud_albedo1_error=real_fill_value

  allocate(l2_input_2d%cloud_albedo2(xdim1km,ydim1km,nnodes))
  l2_input_2d%cloud_albedo2=real_fill_value

  allocate(l2_input_2d%cloud_albedo2_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cloud_albedo2_error=real_fill_value

  allocate(l2_input_2d%cee(xdim1km,ydim1km,nnodes))
  l2_input_2d%cee=real_fill_value

  allocate(l2_input_2d%cee_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%cee_error=real_fill_value

  allocate(l2_input_2d%qcflag(xdim1km,ydim1km,nnodes))
  l2_input_2d%qcflag=nint_fill_value

  allocate(l2_input_2d%convergence(xdim1km,ydim1km,nnodes))
  l2_input_2d%convergence=nint_fill_value

  allocate(l2_input_2d%illum(xdim1km,ydim1km,nnodes))
  l2_input_2d%illum=short_int_fill_value

  allocate(l2_input_2d%lsflag(xdim1km,ydim1km,nnodes))
  l2_input_2d%lsflag=short_int_fill_value
  
  allocate(l2_input_2d%stemp(xdim1km,ydim1km,nnodes))
  l2_input_2d%stemp=real_fill_value

  allocate(l2_input_2d%stemp_error(xdim1km,ydim1km,nnodes))
  l2_input_2d%stemp_error=real_fill_value

  allocate(l2_input_2d%cld_type(xdim1km,ydim1km,nnodes))
  l2_input_2d%cld_type=short_int_fill_value

  allocate(l2_input_2d%ann_phase(xdim1km,ydim1km,nnodes))
  l2_input_2d%ann_phase=short_int_fill_value

  !   allocate(l2_input_2d%l2_filter_array_2d_macro(xdim1km,ydim1km))
  !   l2_input_2d%l2_filter_array_2d_macro=0
  !   allocate(l2_input_2d%l2_filter_array_2d_micro(xdim1km,ydim1km))
  !   l2_input_2d%l2_filter_array_2d_micro=0

  !   allocate(l2_input_2d%vname(nl2vars_1km))
  !   l2_input_2d%vname=''
  !
  !   allocate(l2_input_2d%ename(nl2vars_errors_1km))
  !   l2_input_2d%ename=''


  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l2_input_2d%cth2(xdim1km,ydim1km,nnodes))
     l2_input_2d%cth2=real_fill_value

     allocate(l2_input_2d%cth2_error(xdim1km,ydim1km,nnodes))
     l2_input_2d%cth2_error=real_fill_value

     allocate(l2_input_2d%qcflag_ctt(xdim1km,ydim1km,nnodes))
     l2_input_2d%qcflag_ctt=nint_fill_value

     allocate(l2_input_2d%qcflag_ctp(xdim1km,ydim1km,nnodes))
     l2_input_2d%qcflag_ctp=nint_fill_value

     allocate(l2_input_2d%convergence(xdim1km,ydim1km,nnodes))
     l2_input_2d%convergence=nint_fill_value

     allocate(l2_input_2d%convergence_ctt(xdim1km,ydim1km,nnodes))
     l2_input_2d%convergence_ctt=nint_fill_value

     allocate(l2_input_2d%convergence_ctp(xdim1km,ydim1km,nnodes))
     l2_input_2d%convergence_ctp=nint_fill_value
  endif

  !toa
  allocate(l2_input_2d%toa_swup(xdim1km,ydim1km,nnodes))
  l2_input_2d%toa_swup=real_fill_value
  allocate(l2_input_2d%toa_swdn(xdim1km,ydim1km,nnodes))
  l2_input_2d%toa_swdn=real_fill_value
  allocate(l2_input_2d%toa_swup_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%toa_swup_clr=real_fill_value
  allocate(l2_input_2d%toa_lwup(xdim1km,ydim1km,nnodes))
  l2_input_2d%toa_lwup=real_fill_value
  allocate(l2_input_2d%toa_lwup_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%toa_lwup_clr=real_fill_value

  !boa
  allocate(l2_input_2d%boa_swup(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_swup=real_fill_value
  allocate(l2_input_2d%boa_swup_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_swup_clr=real_fill_value
  allocate(l2_input_2d%boa_lwup(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_lwup=real_fill_value
  allocate(l2_input_2d%boa_lwup_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_lwup_clr=real_fill_value
  allocate(l2_input_2d%boa_swdn(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_swdn=real_fill_value
  allocate(l2_input_2d%boa_swdn_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_swdn_clr=real_fill_value
  allocate(l2_input_2d%boa_lwdn(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_lwdn=real_fill_value
  allocate(l2_input_2d%boa_lwdn_clr(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_lwdn_clr=real_fill_value
  allocate(l2_input_2d%boa_par_dif(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_par_dif=real_fill_value
  allocate(l2_input_2d%boa_par_tot(xdim1km,ydim1km,nnodes))
  l2_input_2d%boa_par_tot=real_fill_value



  if (llocal) then
     allocate(l2_input_2d%reflectance06(xdim1km,ydim1km,nnodes))
     l2_input_2d%reflectance06=real_fill_value
     allocate(l2_input_2d%reflectance08(xdim1km,ydim1km,nnodes))
     l2_input_2d%reflectance08=real_fill_value
     allocate(l2_input_2d%reflectance16(xdim1km,ydim1km,nnodes))
     l2_input_2d%reflectance16=real_fill_value
     allocate(l2_input_2d%BT37(xdim1km,ydim1km,nnodes))
     l2_input_2d%BT37=real_fill_value
     allocate(l2_input_2d%BT11(xdim1km,ydim1km,nnodes))
     l2_input_2d%BT11=real_fill_value
     allocate(l2_input_2d%BT12(xdim1km,ydim1km,nnodes))
     l2_input_2d%BT12=real_fill_value
  endif

end subroutine set_l2_input_struct_2d

!-------------------------------------------
!-------------------------------------------
subroutine set_l2b_macro_micro_struct(l2b_macro_micro,ncols,nrows,calgo,llocal)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: ncols,nrows

  type(l2b_output_struct_2d) :: l2b_macro_micro

  character (len=paramlength) :: calgo
  logical :: llocal

  allocate(l2b_macro_micro%counter(ncols,nrows))
  l2b_macro_micro%counter=0

  allocate(l2b_macro_micro%time(ncols,nrows))
  l2b_macro_micro%time=long_int_fill_value

  allocate(l2b_macro_micro%satzen(ncols,nrows))
  l2b_macro_micro%satzen=(-1.0)*real_fill_value

  allocate(l2b_macro_micro%solzen(ncols,nrows))
  l2b_macro_micro%solzen=real_fill_value

  allocate(l2b_macro_micro%relazi(ncols,nrows))
  l2b_macro_micro%relazi=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctt(ncols,nrows))
  l2b_macro_micro%ctt=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctt_error(ncols,nrows))
  l2b_macro_micro%ctt_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctt_corrected(ncols,nrows))
  l2b_macro_micro%ctt_corrected=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctt_corrected_error(ncols,nrows))
  l2b_macro_micro%ctt_corrected_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cth(ncols,nrows))
  l2b_macro_micro%cth=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cth_error(ncols,nrows))
  l2b_macro_micro%cth_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cth_corrected(ncols,nrows))
  l2b_macro_micro%cth_corrected=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cth_corrected_error(ncols,nrows))
  l2b_macro_micro%cth_corrected_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctp(ncols,nrows))
  l2b_macro_micro%ctp=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctp_error(ncols,nrows))
  l2b_macro_micro%ctp_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctp_corrected(ncols,nrows))
  l2b_macro_micro%ctp_corrected=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%ctp_corrected_error(ncols,nrows))
  l2b_macro_micro%ctp_corrected_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cct(ncols,nrows))
  l2b_macro_micro%cct=short_int_fill_value

  allocate(l2b_macro_micro%cct_error(ncols,nrows))
  l2b_macro_micro%cct_error=real_fill_value

  !  allocate(l2b_macro_micro%cct_pre(ncols,nrows))
  !  l2b_macro_micro%cct_pre=int(-1,kind=sint)

  !  allocate(l2b_macro_micro%cct_post(ncols,nrows))
  !  l2b_macro_micro%cct_post=int(-1,kind=sint)

  allocate(l2b_macro_micro%cot(ncols,nrows))
  l2b_macro_micro%cot=real_fill_value

  allocate(l2b_macro_micro%cot_error(ncols,nrows))
  l2b_macro_micro%cot_error=real_fill_value

  !  allocate(l2b_macro_micro%cccot(ncols,nrows))
  !  l2b_macro_micro%cccot=real_fill_value

  allocate(l2b_macro_micro%ann_phase_uncertainty(ncols,nrows))
  l2b_macro_micro%ann_phase_uncertainty=real_fill_value

  allocate(l2b_macro_micro%cphcot(ncols,nrows))
  l2b_macro_micro%cphcot=real_fill_value

  allocate(l2b_macro_micro%cccot_pre(ncols,nrows))
  l2b_macro_micro%cccot_pre=real_fill_value

  allocate(l2b_macro_micro%ref(ncols,nrows))
  l2b_macro_micro%ref=real_fill_value

  allocate(l2b_macro_micro%ref_error(ncols,nrows))
  l2b_macro_micro%ref_error=real_fill_value

  allocate(l2b_macro_micro%cty(ncols,nrows))
  l2b_macro_micro%cty=short_int_fill_value

  !  allocate(l2b_macro_micro%cty_pre(ncols,nrows))
  !  l2b_macro_micro%cty_pre=int(-1,kind=sint)

  !  allocate(l2b_macro_micro%cty_post(ncols,nrows))
  !  l2b_macro_micro%cty_post=int(-1,kind=sint)

  allocate(l2b_macro_micro%illum(ncols,nrows))
  l2b_macro_micro%illum=short_int_fill_value

  allocate(l2b_macro_micro%cwp(ncols,nrows))
  l2b_macro_micro%cwp=real_fill_value

  allocate(l2b_macro_micro%cwp_error(ncols,nrows))
  l2b_macro_micro%cwp_error=real_fill_value

  allocate(l2b_macro_micro%cloud_albedo1(ncols,nrows))
  l2b_macro_micro%cloud_albedo1=real_fill_value

  allocate(l2b_macro_micro%cloud_albedo1_error(ncols,nrows))
  l2b_macro_micro%cloud_albedo1_error=real_fill_value

  allocate(l2b_macro_micro%cloud_albedo2(ncols,nrows))
  l2b_macro_micro%cloud_albedo2=real_fill_value

  allocate(l2b_macro_micro%cloud_albedo2_error(ncols,nrows))
  l2b_macro_micro%cloud_albedo2_error=real_fill_value

  allocate(l2b_macro_micro%qcflag(ncols,nrows))
  l2b_macro_micro%qcflag=nint_fill_value

  allocate(l2b_macro_micro%cld_type(ncols,nrows))
  l2b_macro_micro%cld_type=short_int_fill_value

  allocate(l2b_macro_micro%ann_phase(ncols,nrows))
  l2b_macro_micro%ann_phase=short_int_fill_value

  allocate(l2b_macro_micro%stemp(ncols,nrows))
  l2b_macro_micro%stemp=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%stemp_error(ncols,nrows))
  l2b_macro_micro%stemp_error=real(stint_fill_value, kind=sreal)

  allocate(l2b_macro_micro%cee(ncols,nrows))
  l2b_macro_micro%cee=real_fill_value

  allocate(l2b_macro_micro%cee_error(ncols,nrows))
  l2b_macro_micro%cee_error=real_fill_value

  allocate(l2b_macro_micro%toa_lwup(ncols,nrows))
  l2b_macro_micro%toa_lwup=real_fill_value
  allocate(l2b_macro_micro%toa_lwup_clr(ncols,nrows))
  l2b_macro_micro%toa_lwup_clr=real_fill_value
  allocate(l2b_macro_micro%toa_swup(ncols,nrows))
  l2b_macro_micro%toa_swup=real_fill_value
  allocate(l2b_macro_micro%toa_swup_clr(ncols,nrows))
  l2b_macro_micro%toa_swup_clr=real_fill_value
  allocate(l2b_macro_micro%toa_swdn(ncols,nrows))
  l2b_macro_micro%toa_swdn=real_fill_value

  !boa
  allocate(l2b_macro_micro%boa_lwup(ncols,nrows))
  l2b_macro_micro%boa_lwup=real_fill_value
  allocate(l2b_macro_micro%boa_lwup_clr(ncols,nrows))
  l2b_macro_micro%boa_lwup_clr=real_fill_value
  allocate(l2b_macro_micro%boa_swup(ncols,nrows))
  l2b_macro_micro%boa_swup=real_fill_value
  allocate(l2b_macro_micro%boa_swup_clr(ncols,nrows))
  l2b_macro_micro%boa_swup_clr=real_fill_value
  allocate(l2b_macro_micro%boa_lwdn(ncols,nrows))
  l2b_macro_micro%boa_lwdn=real_fill_value
  allocate(l2b_macro_micro%boa_lwdn_clr(ncols,nrows))
  l2b_macro_micro%boa_lwdn_clr=real_fill_value
  allocate(l2b_macro_micro%boa_swdn(ncols,nrows))
  l2b_macro_micro%boa_swdn=real_fill_value
  allocate(l2b_macro_micro%boa_swdn_clr(ncols,nrows))
  l2b_macro_micro%boa_swdn_clr=real_fill_value
  allocate(l2b_macro_micro%boa_par_dif(ncols,nrows))
  l2b_macro_micro%boa_par_dif=real_fill_value
  allocate(l2b_macro_micro%boa_par_tot(ncols,nrows))
  l2b_macro_micro%boa_par_tot=real_fill_value




  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l2b_macro_micro%cth2(ncols,nrows))
     l2b_macro_micro%cth2=real_fill_value

     allocate(l2b_macro_micro%cth2_error(ncols,nrows))
     l2b_macro_micro%cth2_error=real_fill_value

     allocate(l2b_macro_micro%qcflag_ctt(ncols,nrows))
     l2b_macro_micro%qcflag_ctt=int(-1,kind=stint)

     allocate(l2b_macro_micro%qcflag_ctp(ncols,nrows))
     l2b_macro_micro%qcflag_ctp=int(-1,kind=stint)


     allocate(l2b_macro_micro%convergence(ncols,nrows))
     l2b_macro_micro%convergence=int(-1,kind=stint)

     allocate(l2b_macro_micro%convergence_ctt(ncols,nrows))
     l2b_macro_micro%convergence_ctt=int(-1,kind=stint)

     allocate(l2b_macro_micro%convergence_ctp(ncols,nrows))
     l2b_macro_micro%convergence_ctp=int(-1,kind=stint)
     !  l2b_macro_micro%qcflag=nint_fill_value

  endif

  if (llocal) then
     allocate(l2b_macro_micro%reflectance06(ncols,nrows))
     l2b_macro_micro%reflectance06=real_fill_value
     allocate(l2b_macro_micro%reflectance08(ncols,nrows))
     l2b_macro_micro%reflectance08=real_fill_value
     allocate(l2b_macro_micro%reflectance16(ncols,nrows))
     l2b_macro_micro%reflectance16=real_fill_value
     allocate(l2b_macro_micro%BT37(ncols,nrows))
     l2b_macro_micro%BT37=real(stint_fill_value, kind=sreal)
     allocate(l2b_macro_micro%BT11(ncols,nrows))
     l2b_macro_micro%BT11=real(stint_fill_value, kind=sreal)
     allocate(l2b_macro_micro%BT12(ncols,nrows))
     l2b_macro_micro%BT12=real(stint_fill_value, kind=sreal)
  endif


end subroutine set_l2b_macro_micro_struct






!-------------------------------------------
!-------------------------------------------
subroutine set_l2b_input_struct(l2b_input_2d,ncols,nrows,nnodes,calgo)
  !-------------------------------------------
  !-------------------------------------------


  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: ncols,nrows,nnodes

  type(l2b_input_struct_2d) :: l2b_input_2d

  character (len=paramlength) :: calgo

  allocate(l2b_input_2d%counter(ncols,nrows,nnodes))
  l2b_input_2d%counter=0

  allocate(l2b_input_2d%time(ncols,nrows,nnodes))
  l2b_input_2d%time=double_fill_value

  allocate(l2b_input_2d%satzen(ncols,nrows,nnodes))
  l2b_input_2d%satzen=(-1.0)*real_fill_value

  allocate(l2b_input_2d%solzen(ncols,nrows,nnodes))
  l2b_input_2d%solzen=real_fill_value

  allocate(l2b_input_2d%relazi(ncols,nrows,nnodes))
  l2b_input_2d%relazi=real_fill_value

  allocate(l2b_input_2d%ctt(ncols,nrows,nnodes))
  l2b_input_2d%ctt=real_fill_value

  allocate(l2b_input_2d%ctt_error(ncols,nrows,nnodes))
  l2b_input_2d%ctt_error=real_fill_value

  allocate(l2b_input_2d%ctt_corrected(ncols,nrows,nnodes))
  l2b_input_2d%ctt=real_fill_value

  allocate(l2b_input_2d%ctt_corrected_error(ncols,nrows,nnodes))
  l2b_input_2d%ctt_error=real_fill_value

  allocate(l2b_input_2d%stemp(ncols,nrows,nnodes))
  l2b_input_2d%stemp=real_fill_value

  allocate(l2b_input_2d%stemp_error(ncols,nrows,nnodes))
  l2b_input_2d%stemp_error=real_fill_value

  allocate(l2b_input_2d%cth(ncols,nrows,nnodes))
  l2b_input_2d%cth=real_fill_value

  allocate(l2b_input_2d%cth_error(ncols,nrows,nnodes))
  l2b_input_2d%cth_error=real_fill_value

  allocate(l2b_input_2d%cth_corrected(ncols,nrows,nnodes))
  l2b_input_2d%cth_corrected=real_fill_value

  allocate(l2b_input_2d%cth_corrected_error(ncols,nrows,nnodes))
  l2b_input_2d%cth_corrected_error=real_fill_value

  allocate(l2b_input_2d%ctp(ncols,nrows,nnodes))
  l2b_input_2d%ctp=real_fill_value

  allocate(l2b_input_2d%ctp_error(ncols,nrows,nnodes))
  l2b_input_2d%ctp_error=real_fill_value

  allocate(l2b_input_2d%ctp_corrected(ncols,nrows,nnodes))
  l2b_input_2d%ctp_corrected=real_fill_value

  allocate(l2b_input_2d%ctp_corrected_error(ncols,nrows,nnodes))
  l2b_input_2d%ctp_corrected_error=real_fill_value

  allocate(l2b_input_2d%cct(ncols,nrows,nnodes))
  l2b_input_2d%cct=int(-1,kind=sint)

  allocate(l2b_input_2d%cct_error(ncols,nrows,nnodes))
  l2b_input_2d%cct_error=int(-1,kind=sint)

  allocate(l2b_input_2d%cct_pre(ncols,nrows,nnodes))
  l2b_input_2d%cct_pre=int(-1,kind=sint)

  allocate(l2b_input_2d%cct_post(ncols,nrows,nnodes))
  l2b_input_2d%cct_post=int(-1,kind=sint)

  allocate(l2b_input_2d%cot(ncols,nrows,nnodes))
  l2b_input_2d%cot=real_fill_value

  allocate(l2b_input_2d%cot_error(ncols,nrows,nnodes))
  l2b_input_2d%cot_error=real_fill_value

  allocate(l2b_input_2d%cccot(ncols,nrows,nnodes))
  l2b_input_2d%cccot=real_fill_value

  allocate(l2b_input_2d%cccot_pre(ncols,nrows,nnodes))
  l2b_input_2d%cccot_pre=real_fill_value

  allocate(l2b_input_2d%ann_phase_uncertainty(ncols,nrows,nnodes))
  l2b_input_2d%ann_phase_uncertainty=real_fill_value

  allocate(l2b_input_2d%cphcot(ncols,nrows,nnodes))
  l2b_input_2d%cphcot=real_fill_value

  allocate(l2b_input_2d%ref(ncols,nrows,nnodes))
  l2b_input_2d%ref=real_fill_value

  allocate(l2b_input_2d%ref_error(ncols,nrows,nnodes))
  l2b_input_2d%ref_error=real_fill_value

  allocate(l2b_input_2d%cty(ncols,nrows,nnodes))
  l2b_input_2d%cty=int(-1,kind=sint)!real_fill_value

  allocate(l2b_input_2d%cty_pre(ncols,nrows,nnodes))
  l2b_input_2d%cty_pre=int(-1,kind=sint)!real_fill_value

  allocate(l2b_input_2d%cty_post(ncols,nrows,nnodes))
  l2b_input_2d%cty_post=int(-1,kind=sint)!real_fill_value

  allocate(l2b_input_2d%illum(ncols,nrows,nnodes))
  l2b_input_2d%illum=int(-1,kind=sint)!real_fill_value

  allocate(l2b_input_2d%cwp(ncols,nrows,nnodes))
  l2b_input_2d%cwp=real_fill_value

  allocate(l2b_input_2d%cwp_error(ncols,nrows,nnodes))
  l2b_input_2d%cwp_error=real_fill_value

  allocate(l2b_input_2d%cloud_albedo1(ncols,nrows,nnodes))
  l2b_input_2d%cloud_albedo1=real_fill_value

  allocate(l2b_input_2d%cloud_albedo1_error(ncols,nrows,nnodes))
  l2b_input_2d%cloud_albedo1_error=real_fill_value

  allocate(l2b_input_2d%cloud_albedo2(ncols,nrows,nnodes))
  l2b_input_2d%cloud_albedo2=real_fill_value

  allocate(l2b_input_2d%cloud_albedo2_error(ncols,nrows,nnodes))
  l2b_input_2d%cloud_albedo2_error=real_fill_value

  allocate(l2b_input_2d%qcflag(ncols,nrows,nnodes))
  l2b_input_2d%qcflag=int(-1,kind=stint)
  !  l2b_input_2d%qcflag=nint_fill_value

  allocate(l2b_input_2d%cee(ncols,nrows,nnodes))
  l2b_input_2d%cee=real_fill_value

  allocate(l2b_input_2d%cee_error(ncols,nrows,nnodes))
  l2b_input_2d%cee_error=real_fill_value

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     allocate(l2b_input_2d%cth2(ncols,nrows,nnodes))
     l2b_input_2d%cth2=real_fill_value

     allocate(l2b_input_2d%cth2_error(ncols,nrows,nnodes))
     l2b_input_2d%cth2_error=real_fill_value
  endif



end subroutine set_l2b_input_struct



!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_input_struct_2d(l2_input_2d,calgo,llocal)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l2_input_struct_2d) :: l2_input_2d

  character (len=paramlength) :: calgo

  logical,intent(in) :: llocal

  ! Local
  INTEGER ::  ierr

  !  if(allocated(l2_input_2d%time)) print,* 'time allocated'

  deallocate(l2_input_2d%time,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%time failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%lon,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%lon failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%lat,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%lat failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%satzen,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%satzen failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%solzen,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%solzen failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%relazi,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%relazi failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ctp,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctp failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ctp_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctp_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ctp_corrected,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctp_corrected failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ctp_corrected_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctp_corrected_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cth,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cth failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cth_corrected,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cth_corrected failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%ctt,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%ctt_corrected,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cct,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cct failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cct_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cct_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cct_pre,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cct_pre failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cct_post,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cct_post failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cot,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cot failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cot_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cot_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cccot,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cccot failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cccot_pre,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cccot_pre failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ann_phase_uncertainty,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ann_phase_uncertainty failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cphcot,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cphcot failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ref,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ref failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ref_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ref_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cwp,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cwp failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cwp_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cwp_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cee,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cee failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cee_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cee_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cloud_albedo1,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cloud_albedo1 failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cloud_albedo1_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cloud_albedo1_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cloud_albedo2,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cloud_albedo2 failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cloud_albedo2_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cloud_albedo2_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cty,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cty failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cty_pre,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cty_pre failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cty_post,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cty_post failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cty_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cty_error failed: ierr=', ierr
  endif  
  deallocate(l2_input_2d%illum,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%illum failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%qcflag,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%qcflag failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%convergence,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%convergence failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%stemp,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%stemp failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%stemp_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%stemp_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cld_type,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cld_type failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%ann_phase,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ann_phase failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%cth_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cth_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%cth_corrected_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%cth_corrected_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%ctt_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_error failed: ierr=', ierr
  endif

  deallocate(l2_input_2d%ctt_corrected_error,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif

  !toa                                                                                                                                                                                        
  deallocate(l2_input_2d%toa_swup,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%toa_swdn,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%toa_swup_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%toa_lwup,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%toa_lwup_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
 
  !boa                                                                                                                                                                              

  deallocate(l2_input_2d%boa_swup,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_swup_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_lwup,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_lwup_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_swdn,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_swdn_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_lwdn,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_lwdn_clr,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_par_dif,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif
  deallocate(l2_input_2d%boa_par_tot,stat = ierr)
  if ( ierr /= 0 ) then
     print *, 'deallocate l2_input_2d%ctt_corrected_error failed: ierr=', ierr
  endif

  if (llocal) then
     deallocate(l2_input_2d%reflectance06, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%reflectance06 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%reflectance08, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%reflectance08 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%reflectance16, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%reflectance16 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%BT37, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%BT37 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%BT11, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%BT11 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%BT12, stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%BT12 failed: ierr=', ierr
     endif
  endif


  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     deallocate(l2_input_2d%cth2,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%cth2 failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%cth2_error,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%cth2_error failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%qcflag_ctt,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%qcflag_ctt failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%qcflag_stemp,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%qcflag_stemp failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%qcflag_ctp,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%qcflag_ctp failed: ierr=', ierr
     endif
     deallocate(l2_input_2d%convergence,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%convergence failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%convergence_ctt,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%convergence_ctt failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%convergence_stemp,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%convergence_stemp failed: ierr=', ierr
     endif

     deallocate(l2_input_2d%convergence_ctp,stat = ierr)
     if ( ierr /= 0 ) then
        print *, 'deallocate l2_input_2d%convergence_ctp failed: ierr=', ierr
     endif
  endif

end subroutine unset_l2_input_struct_2d


!-------------------------------------------
!-------------------------------------------
subroutine unset_l3d_macro_struct(l3d_macro,calgo)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l3d_macro_struct) :: l3d_macro

  character (len=paramlength) :: calgo

  deallocate(l3d_macro%points)
  deallocate(l3d_macro%ctp_mean)
  deallocate(l3d_macro%ctp_corrected_mean)
  deallocate(l3d_macro%cth_mean)
  deallocate(l3d_macro%cth_corrected_mean)
  deallocate(l3d_macro%stemp_mean)
  deallocate(l3d_macro%ctt_mean)
  deallocate(l3d_macro%ctt_corrected_mean)
  deallocate(l3d_macro%cct_mean)
  !sstapelb ergäze fehlende
  deallocate(l3d_macro%ctp_phi_total)
  deallocate(l3d_macro%ctp_std_corr)
  deallocate(l3d_macro%ctp_std)
  deallocate(l3d_macro%pairs)

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     deallocate(l3d_macro%cth2_mean)
  endif


end subroutine unset_l3d_macro_struct


!-------------------------------------------
!-------------------------------------------
subroutine unset_l3d_macro_input_struct(l3d_macro_input,calgo)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l3d_macro_input_struct) :: l3d_macro_input

  character (len=paramlength) :: calgo

  deallocate(l3d_macro_input%points)
  deallocate(l3d_macro_input%ctp_mean)
  deallocate(l3d_macro_input%ctp_corrected_mean)
  deallocate(l3d_macro_input%cth_mean)
  deallocate(l3d_macro_input%cth_corrected_mean)
  deallocate(l3d_macro_input%ctt_mean)
  deallocate(l3d_macro_input%ctt_corrected_mean)
  deallocate(l3d_macro_input%stemp_mean)
  deallocate(l3d_macro_input%cct_mean)
  !sstapelb ergäze fehlende
  deallocate(l3d_macro_input%ctp_mean_cleared)
  deallocate(l3d_macro_input%ctp_phi)
  deallocate(l3d_macro_input%ctp_std)
  deallocate(l3d_macro_input%cee_mean)

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then
     deallocate(l3d_macro_input%cth2_mean)
  endif

end subroutine unset_l3d_macro_input_struct





!-------------------------------------------
!-------------------------------------------
subroutine unset_l3d_micro_struct(l3d_micro)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l3d_micro_struct) :: l3d_micro

  deallocate(l3d_micro%points)
  deallocate(l3d_micro%cot_mean)
  deallocate(l3d_micro%ref_mean)
  deallocate(l3d_micro%cwp_mean)
  deallocate(l3d_micro%cloud_albedo1_mean)
  deallocate(l3d_micro%cloud_albedo2_mean)
  deallocate(l3d_micro%cty_mean)
  !sstapelb ergänze fehlende
  deallocate(l3d_micro%cot_phi_total)
  deallocate(l3d_micro%cot_std_corr)
  deallocate(l3d_micro%cot_std)
  deallocate(l3d_micro%pairs)


end subroutine unset_l3d_micro_struct



!-------------------------------------------
!-------------------------------------------
subroutine set_l2_input_struct_local(l2_input_local,&
     & mmin_local_lon,mmax_local_lon,mmin_local_lat,mmax_local_lat,maxcounter, &
     & nl2vars_1km,nl2vars_errors_1km,n_val_plus_error,n_oe_features)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  integer(kind=lint) :: mmin_local_lon,mmax_local_lon,mmin_local_lat,mmax_local_lat,maxcounter

  integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error, n_oe_features

  type(l2_input_struct_local) :: l2_input_local

  allocate(l2_input_local%time(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat,maxcounter))
  l2_input_local%time=real_fill_value

  allocate(l2_input_local%lon(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat,maxcounter))
  l2_input_local%lon=real_fill_value

  allocate(l2_input_local%lat(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat,maxcounter))
  l2_input_local%lat=real_fill_value

  allocate(l2_input_local%l2_phys_vars_2d(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat, &
       & nl2vars_1km,n_val_plus_error,maxcounter))
  l2_input_local%l2_phys_vars_2d=real_fill_value

  allocate(l2_input_local%l2_oe_vars_2d(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat, &
       & n_oe_features,maxcounter))
  l2_input_local%l2_oe_vars_2d=real_fill_value

  allocate(l2_input_local%counter_local(mmin_local_lon:mmax_local_lon,mmin_local_lat:mmax_local_lat))
  l2_input_local%counter_local=long_int_fill_value

end subroutine set_l2_input_struct_local


!sstapelb 19.11.2012
!-------------------------------------------
!-------------------------------------------
subroutine unset_l2_input_struct_local(l2_input_local)
  !-------------------------------------------
  !-------------------------------------------

  use vartypes

  use structures

  implicit none

  type(l2_input_struct_local) :: l2_input_local

  deallocate(l2_input_local%time)
  deallocate(l2_input_local%lon)
  deallocate(l2_input_local%lat)
  deallocate(l2_input_local%l2_phys_vars_2d)
  deallocate(l2_input_local%l2_oe_vars_2d)
  deallocate(l2_input_local%counter_local)

end subroutine unset_l2_input_struct_local
