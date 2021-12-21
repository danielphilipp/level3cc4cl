! Name: structure.f90
!
!
! Purpose: F90 Module file which declares user defined variable type structures.
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
!2013/04/12 CC added variables ice/liq for cot and ref
!2014/11/19 OS added variables cccot_pre, cty_pre, cty_post, cct, cct_pre, cct_post,
!  cty_pre_error, cct_error, cct_pre_error cct_post_points_raw, cct_post_points_clear_raw,
!  cct_post_raw
!2015/02/27 OS and SST removed l2_filter_array_2d_micro/macro, vname, ename, weighted means/standard deviations; added cloudy points and cloud type, histogram_ref_phase, histogram_cwp_phase 
!2015/03/11 CP added cloud albedo
!2015/04/23 OS added variables cld_type, solzen, relazi, and stemp(_error)
!2015/07/07 CP added corrected cloud top height
!2015/07/10 OS removed variable cct_error
!2015/10/20 CP added stemp
!2015/12/16 OS minor edit
!2016/01/21 OS added allsky IWP and LWP
!2016/02/18 OS added CTP_corrected, CTT_corrected, CEE
!2016/02/23 OS changed type of time variable
!2016/02/26 OS added counter for CEE
!2017/06/22 OS added stemp_clear/cloudy, toa/boa, ann phase, MODIS refl/BT
!2017/06/29 OS added cty, which stands for cph (it should not)
!
! $Id$
!
! Bugs:
!
!none known

module structures

  use vartypes

  implicit none

  type l1_input_struct_2d

     real(kind=sreal), dimension(:,:), pointer ::  refl_1

  end type l1_input_struct_2d

  type l2_input_struct_2d

     real(kind=dreal), dimension(:,:,:), pointer :: time
     real(kind=sreal), dimension(:,:),   pointer :: lon, lat,satzen,solzen,relazi
     real(kind=sreal), dimension(:,:,:), pointer ::  ctt, cth, ctp, cct, cct_pre, cct_post, cth2, cth_corrected,stemp, ctp_corrected, ctt_corrected
     real(kind=sreal), dimension(:,:,:), pointer ::  ctp_error, cct_error,cct_pre_error,ctt_error,cth_error, cth2_error, cth_corrected_error,stemp_error, ctp_corrected_error, ctt_corrected_error
     real(kind=sreal), dimension(:,:,:), pointer ::  cot, ref, cty, cty_pre, cty_post, cty_error, cwp,cccot, cccot_pre,cloud_albedo1,cloud_albedo2
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_error, ref_error, cwp_error,cloud_albedo1_error,cloud_albedo2_error

     real(kind=sreal), dimension(:,:,:), pointer ::  ctp_prop_uncertainty, cct_prop_uncertainty,cct_pre_prop_uncertainty,ctt_prop_uncertainty,cth_prop_uncertainty, cth2_prop_uncertainty,cth_corrected_prop_uncertainty,stemp_prop_uncertainty
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_prop_uncertainty, ref_prop_uncertainty, cwp_prop_uncertainty,cloud_albedo1_prop_uncertainty,cloud_albedo2_prop_uncertainty,ctp_corrected_prop_uncertainty,ctt_corrected_prop_uncertainty

     real(kind=sreal), dimension(:,:,:), pointer ::  ctp_correlated_uncertainty, cct_correlated_uncertainty,cct_pre_correlated_uncertainty,ctt_correlated_uncertainty,cth_correlated_uncertainty, cth2_correlated_uncertainty,cth_corrected_correlated_uncertainty,stemp_correlated_uncertainty
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_correlated_uncertainty, ref_correlated_uncertainty, cwp_correlated_uncertainty,cloud_albedo1_correlated_uncertainty,cloud_albedo2_correlated_uncertainty,ctp_corrected_correlated_uncertainty,ctt_corrected_correlated_uncertainty
     real(kind=sreal), dimension(:,:,:), pointer ::  cee, cee_error
     real(kind=sreal), dimension(:,:,:), pointer ::  reflectance06, reflectance08, reflectance16, BT37, BT11, BT12
     real(kind=sreal), dimension(:,:,:), pointer ::  toa_swup,toa_swup_clr,toa_lwup,toa_lwup_clr,toa_swdn
     real(kind=sreal), dimension(:,:,:), pointer ::  boa_swup,boa_swup_clr,boa_lwup,boa_lwup_clr,boa_swdn,boa_swdn_clr,boa_lwdn,boa_lwdn_clr
     real(kind=sreal), dimension(:,:,:), pointer ::  boa_par_dif,boa_par_tot
     real(kind=sreal), dimension(:,:,:), pointer ::  ann_phase_uncertainty, cphcot
      
     !     real(kind=sreal), dimension(:,:,:), pointer :: l2_oe_vars_2d
     !      integer(kind=lint), dimension(:,:), pointer :: l2_filter_array_2d_micro
     !      integer(kind=lint), dimension(:,:), pointer :: l2_filter_array_2d_macro


     integer(kind=stint), dimension(:,:,:), pointer :: qcflag,qcflag_ctt,qcflag_ctp,convergence,convergence_ctt,convergence_ctp,qcflag_stemp,convergence_stemp

     integer(kind=sint), dimension(:,:,:), pointer :: illum, cld_type, ann_phase, lsflag

     !      character(len=varlength), dimension(:), pointer :: vname, ename

     !     real(kind=sreal), dimension(:,:,:), pointer :: stemp, stemp_error

  end type l2_input_struct_2d

  type l2b_input_struct_2d

     integer(kind=lint), dimension(:,:,:), pointer :: counter

     real(kind=dreal), dimension(:,:,:), pointer ::  time

     real(kind=sreal), dimension(:,:,:), pointer ::  satzen, solzen, relazi

     real(kind=sreal), dimension(:,:,:), pointer ::  ctt, ctt_corrected, cth, cth_corrected, ctp, ctp_corrected, cth2, stemp,cee
     real(kind=sreal), dimension(:,:,:), pointer ::  cot, ref, cwp,cccot_pre, cccot,cloud_albedo1,cloud_albedo2
     integer(kind=sint), dimension(:,:,:), pointer :: cty, cty_pre, cty_post, cct, cct_pre, cct_post, illum

     real(kind=sreal), dimension(:,:,:), pointer ::  ctt_error, ctt_corrected_error, cth_corrected_error,cth_error, ctp_error, ctp_corrected_error, cth2_error,stemp_error,cee_error
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_error, ref_error, cwp_error,cloud_albedo1_error,cloud_albedo2_error
     integer(kind=sint), dimension(:,:,:), pointer :: cty_error, cty_pre_error, cct_error, cct_pre_error



     real(kind=sreal), dimension(:,:,:), pointer ::  ctt_prop_uncertainty, ctt_corrected_prop_uncertainty, cth_prop_uncertainty, ctp_prop_uncertainty, ctp_corrected_prop_uncertainty, cth2_prop_uncertainty, cth_corrected_prop_uncertainty,stemp_prop_uncertainty,cee_prop_uncertainty
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_prop_uncertainty, ref_prop_uncertainty, cwp_prop_uncertainty,cloud_albedo1_prop_uncertainty,cloud_albedo2_prop_uncertainty
     integer(kind=sint), dimension(:,:,:), pointer :: cty_prop_uncertainty, cty_pre_prop_uncertainty, cct_prop_uncertainty, cct_pre_prop_uncertainty



     real(kind=sreal), dimension(:,:,:), pointer ::  ctt_correlated_uncertainty, cth_correlated_uncertainty, ctp_correlated_uncertainty, cth2_correlated_uncertainty,cth_corrected_correlated_uncertainty,stemp_correlated_uncertainty,cee_correlated_uncertainty
     real(kind=sreal), dimension(:,:,:), pointer ::  cot_correlated_uncertainty, ref_correlated_uncertainty, cwp_correlated_uncertainty,cloud_albedo1_correlated_uncertainty,cloud_albedo2_correlated_uncertainty
     integer(kind=sint), dimension(:,:,:), pointer :: cty_correlated_uncertainty, cty_pre_correlated_uncertainty, cct_correlated_uncertainty, cct_pre_correlated_uncertainty,ctt_corrected_correlated_uncertainty,ctp_corrected_correlated_uncertainty

     real(kind=sreal), dimension(:,:,:), pointer ::  ann_phase_uncertainty, cphcot

     integer(kind=stint), dimension(:,:,:), pointer :: qcflag

  end type l2b_input_struct_2d

  type l2b_output_struct_2d

     integer(kind=lint), dimension(:,:), pointer :: counter

     integer(kind=lint), dimension(:,:), pointer ::  time

     real(kind=sreal), dimension(:,:), pointer ::  satzen, solzen, relazi

     real(kind=sreal), dimension(:,:), pointer ::  ctt, cth, ctp,cth2,cth_corrected,stemp,ctp_corrected,ctt_corrected,cee
     real(kind=sreal), dimension(:,:), pointer ::  cot, ref, cwp,cccot, cccot_pre,cloud_albedo1,cloud_albedo2
     integer(kind=sint), dimension(:,:), pointer :: cty, cty_pre, cty_post, cct, cct_pre, cct_post, illum, cld_type, ann_phase

     real(kind=sreal), dimension(:,:), pointer ::  reflectance06, reflectance08, reflectance16, BT37, BT11, BT12

     real(kind=sreal), dimension(:,:), pointer ::  ctt_error, cth_error, ctp_error, cth2_error,cct_error,cth_corrected_error ,stemp_error, ctp_corrected_error, ctt_corrected_error,cee_error
     real(kind=sreal), dimension(:,:), pointer ::  cot_error, ref_error, cwp_error,cloud_albedo1_error,cloud_albedo2_error
     integer(kind=sint), dimension(:,:), pointer :: cty_error, cty_pre_error, cct_pre_error



     real(kind=sreal), dimension(:,:), pointer ::  ctt_prop_uncertainty, cth_prop_uncertainty, ctp_prop_uncertainty, cth2_prop_uncertainty, cth_corrected_prop_uncertainty,stemp_prop_uncertainty,cee_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer ::  cot_prop_uncertainty, ref_prop_uncertainty, cwp_prop_uncertainty,cloud_albedo1_prop_uncertainty,cloud_albedo2_prop_uncertainty
     integer(kind=sint), dimension(:,:), pointer :: cty_prop_uncertainty, cty_pre_prop_uncertainty, cct_prop_uncertainty, cct_pre_prop_uncertainty,ctp_corrected_prop_uncertainty,ctt_corrected_prop_uncertainty


     real(kind=sreal), dimension(:,:), pointer ::  ctt_correlated_uncertainty, cth_correlated_uncertainty, ctp_correlated_uncertainty, cth2_correlated_uncertainty, cth_corrected_correlated_uncertainty,stemp_correlated_uncertainty,cee_correlated_uncertainty
     real(kind=sreal), dimension(:,:), pointer ::  cot_correlated_uncertainty, ref_correlated_uncertainty, cwp_correlated_uncertainty,cloud_albedo1_correlated_uncertainty,cloud_albedo2_correlated_uncertainty
     integer(kind=sint), dimension(:,:), pointer :: cty_correlated_uncertainty, cty_pre_correlated_uncertainty, cct_correlated_uncertainty, cct_pre_correlated_uncertainty, ctt_corrected_correlated_uncertainty, ctp_corrected_correlated_uncertainty

     integer(kind=stint), dimension(:,:), pointer :: qcflag,qcflag_ctt,qcflag_ctp,convergence,convergence_ctp,convergence_ctt,qcflag_stemp

     real(kind=sreal), dimension(:,:), pointer ::  toa_lwup,toa_lwup_clr,toa_swup,toa_swup_clr,toa_swdn
     real(kind=sreal), dimension(:,:), pointer ::  boa_swup,boa_swup_clr,boa_lwup,boa_lwup_clr
     real(kind=sreal), dimension(:,:), pointer ::  boa_swdn,boa_swdn_clr,boa_lwdn,boa_lwdn_clr,boa_par_dif,boa_par_tot

     real(kind=sreal), dimension(:,:), pointer ::  ann_phase_uncertainty, cphcot
     
  end type l2b_output_struct_2d




  type l2_input_struct_2d_old

     real(kind=sreal), dimension(:,:), pointer :: time, lon, lat
     real(kind=sreal), dimension(:,:,:,:), pointer :: l2_phys_vars_2d !x,y,varnumber, error
     real(kind=sreal), dimension(:,:,:), pointer :: l2_oe_vars_2d
     integer(kind=lint), dimension(:,:), pointer :: l2_filter_array_2d

     character(len=varlength), dimension(:), pointer :: vname, ename

  end type l2_input_struct_2d_old



  type l2_input_struct_local

     real(kind=sreal), dimension(:,:,:), pointer :: time, lon, lat
     real(kind=sreal), dimension(:,:,:,:,:), pointer :: l2_phys_vars_2d !x,y gridbox,varnumber, error,pointnumber
     real(kind=sreal), dimension(:,:,:,:), pointer :: l2_oe_vars_2d

     integer(kind=lint), dimension(:,:), pointer :: counter_local

  end type l2_input_struct_local



  type l3_output_struct_2d

     integer(kind=lint), dimension(:,:), pointer :: l3_n_points
     !     real(kind=sreal), dimension(:,:), pointer :: l3_sensor_azimuth
     real(kind=sreal), dimension(:,:,:,:,:), pointer :: l3_vector
     integer(kind=lint), dimension(:,:,:,:,:), pointer :: l3_histogram_cot_ctp_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: l3_histogram_cot_phase

     !     real(kind=sreal), dimension(:,:,:), pointer ::  l3_median_cot

     real(kind=sreal), dimension(:,:,:), pointer ::  l3_median

     real(kind=sreal), dimension(:,:), pointer ::  l3_ctp_log

     !     real(kind=sreal), dimension(:,:,:,:), pointer :: l3_power_spec
     !     real(kind=sreal), dimension(:,:), pointer :: l3_f_para

     !     real(kind=sreal), dimension(:,:,:,:), pointer :: l3_errors


  end type l3_output_struct_2d

  type l3_rad_struct

     integer(kind=lint), dimension(:,:), pointer :: denominator 

     real(kind=sreal), dimension(:,:), pointer :: rad_mean

     real(kind=sreal), dimension(:,:), pointer :: tau_consistent,ppab,reduction_factor,tau_compare,variance_para

  end type l3_rad_struct


  type l3_macro_struct

     integer(kind=lint), dimension(:,:), pointer :: points
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points
     integer(kind=lint), dimension(:,:), pointer :: clear_points
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points_cph_liq
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points_cph_ice

     !      integer(kind=lint), dimension(:,:,:), pointer :: remedian_counter

     !cty (new in macro)
     real(kind=sreal), dimension(:,:), pointer :: cty_mean
     real(kind=sreal), dimension(:,:), pointer :: cty_std
     real(kind=sreal), dimension(:,:), pointer :: cty_mean_error,cty_std_error
     real(kind=sreal), dimension(:,:), pointer :: cty_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cty_correlated_uncertainty

     !ctp
     real(kind=sreal), dimension(:,:), pointer :: ctp_mean!,ctp_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: ctp_std
     real(kind=sreal), dimension(:,:), pointer :: ctp_mean_error,ctp_std_error
     real(kind=sreal), dimension(:,:), pointer :: ctp_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ctp_correlated_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ctp_log_mean

     !ctp_corrected
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_mean!,ctp_corrected_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_std
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_mean_error,ctp_corrected_std_error
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_correlated_uncertainty

     !cth
     real(kind=sreal), dimension(:,:), pointer :: cth_mean!,cth_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cth_std
     real(kind=sreal), dimension(:,:), pointer :: cth_mean_error,cth_std_error
     real(kind=sreal), dimension(:,:), pointer :: cth_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cth_correlated_uncertainty

     !cth_corrected
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_mean!,cth_corrected_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_std
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_mean_error,cth_corrected_std_error
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_correlated_uncertainty

     !cth2 FAME-C
     real(kind=sreal), dimension(:,:), pointer :: cth2_mean,cth2_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cth2_std

     real(kind=sreal), dimension(:,:), pointer :: cth2_mean_error,cth2_std_error

     real(kind=sreal), dimension(:,:), pointer :: cth2_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cth2_correlated_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cth2_v1
     real(kind=sreal), dimension(:,:), pointer :: cth2_v2

     real(kind=sreal), dimension(:,:), pointer :: cth2_median
     real(kind=sreal), dimension(:,:,:,:), pointer :: cth2_md

     !ctt
     real(kind=sreal), dimension(:,:), pointer :: ctt_mean!,ctt_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: ctt_std
     real(kind=sreal), dimension(:,:), pointer :: ctt_mean_error,ctt_std_error
     real(kind=sreal), dimension(:,:), pointer :: ctt_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ctt_correlated_uncertainty

     !ctt_corrected
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_mean!,ctt_corrected_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_std
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_mean_error,ctt_corrected_std_error
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_correlated_uncertainty

     !stemp
     real(kind=sreal), dimension(:,:), pointer :: stemp_mean
     real(kind=sreal), dimension(:,:), pointer :: stemp_std
     real(kind=sreal), dimension(:,:), pointer :: stemp_mean_error,stemp_std_error
     real(kind=sreal), dimension(:,:), pointer :: stemp_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: stemp_correlated_uncertainty
     ! stemp clear
     real(kind=sreal), dimension(:,:), pointer :: stemp_clear_mean
     real(kind=sreal), dimension(:,:), pointer :: stemp_clear_std
     real(kind=sreal), dimension(:,:), pointer :: stemp_clear_mean_error,stemp_clear_std_error
     real(kind=sreal), dimension(:,:), pointer :: stemp_clear_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: stemp_clear_correlated_uncertainty
     

     real(kind=sreal), dimension(:,:), pointer :: ctt_median   
     real(kind=sreal), dimension(:,:,:,:), pointer :: ctt_md


     real(kind=sreal), dimension(:,:), pointer :: stemp_median   
     real(kind=sreal), dimension(:,:,:,:), pointer :: stemp_md


     !cct
     real(kind=sreal), dimension(:,:), pointer :: cct_mean,std

     real(kind=sreal), dimension(:,:), pointer ::  cct_mean_error,cct_std_error


     real(kind=sreal), dimension(:,:), pointer :: cct_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cct_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: cct_v1
     !      real(kind=sreal), dimension(:,:), pointer :: cct_v2


     real(kind=sreal), dimension(:,:), pointer :: cct_mean_low
     real(kind=sreal), dimension(:,:), pointer :: cct_mean_middle
     real(kind=sreal), dimension(:,:), pointer :: cct_mean_high
     real(kind=sreal), dimension(:,:), pointer :: cct_std

     integer(kind=lint), dimension(:,:), pointer :: cct_points
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear
     integer(kind=lint), dimension(:,:), pointer :: cct_low_points
     integer(kind=lint), dimension(:,:), pointer :: cct_middle_points
     integer(kind=lint), dimension(:,:), pointer :: cct_high_points

     integer(kind=lint), dimension(:,:), pointer :: cct_points_day
     integer(kind=lint), dimension(:,:), pointer :: cct_points_twl
     integer(kind=lint), dimension(:,:), pointer :: cct_points_night
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear_day
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear_twl
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear_night
     integer(kind=lint), dimension(:,:), pointer :: cee_points
     integer(kind=lint), dimension(:,:), pointer :: rad_points_sw, rad_points_lw
     integer(kind=lint), dimension(:,:), pointer :: rad_points_sw_low, rad_points_lw_low
     integer(kind=lint), dimension(:,:), pointer :: rad_points_sw_mid, rad_points_lw_mid
     integer(kind=lint), dimension(:,:), pointer :: rad_points_sw_hig, rad_points_lw_hig
     integer(kind=lint), dimension(:,:), pointer :: stemp_clear_points
     real(kind=sreal), dimension(:,:), pointer :: cct_day,cct_night,cct_twl

     !raw cloud cover
     integer(kind=lint), dimension(:,:), pointer :: cct_points_raw, cct_post_points_raw
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear_raw, cct_post_points_clear_raw
     real(kind=sreal), dimension(:,:), pointer :: cct_raw, cct_post_raw

     !integer(kind=lint), dimension(:,:), pointer :: cct_points_toaboa_sw, cct_points_toaboa_lw

     !1d hist
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_ctp_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_ctt_phase

     !cee
     real(kind=sreal), dimension(:,:), pointer :: cee_mean
     real(kind=sreal), dimension(:,:), pointer :: cee_std
     real(kind=sreal), dimension(:,:), pointer :: cee_mean_error,cee_std_error
     real(kind=sreal), dimension(:,:), pointer :: cee_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cee_correlated_uncertainty

     !toa
     real(kind=sreal), dimension(:,:), pointer :: toa_swup_mean,toa_swup_clr_mean,toa_swup_low_mean,toa_swup_mid_mean,toa_swup_hig_mean,toa_swdn_mean
     real(kind=sreal), dimension(:,:), pointer :: toa_lwup_mean,toa_lwup_clr_mean,toa_lwup_low_mean,toa_lwup_mid_mean,toa_lwup_hig_mean
     real(kind=sreal), dimension(:,:), pointer :: boa_swdn_mean,boa_swup_mean,boa_lwdn_mean,boa_lwup_mean
     real(kind=sreal), dimension(:,:), pointer :: boa_swdn_clr_mean,boa_swup_clr_mean,boa_lwdn_clr_mean,boa_lwup_clr_mean
     real(kind=sreal), dimension(:,:), pointer :: boa_par_dif_mean,boa_par_tot_mean
         
  end type l3_macro_struct



  type l3d_macro_struct

     integer(kind=lint), dimension(:,:), pointer :: points
     integer(kind=lint), dimension(:,:), pointer :: pairs

     real(kind=sreal), dimension(:,:), pointer :: ctp_mean
     real(kind=sreal), dimension(:,:), pointer :: ctp_corrected_mean
     real(kind=sreal), dimension(:,:), pointer :: cth_mean
     real(kind=sreal), dimension(:,:), pointer :: cth_corrected_mean
     real(kind=sreal), dimension(:,:), pointer :: cth2_mean  !FAME-C
     real(kind=sreal), dimension(:,:), pointer :: ctt_mean
     real(kind=sreal), dimension(:,:), pointer :: ctt_corrected_mean
     real(kind=sreal), dimension(:,:), pointer :: stemp_mean
     real(kind=sreal), dimension(:,:), pointer :: cct_mean

     real(kind=sreal), dimension(:,:), pointer :: ctp_std,ctp_std_corr
     real(kind=sreal), dimension(:,:), pointer :: ctp_phi_total

  end type l3d_macro_struct


  type l3d_macro_input_struct

     integer(kind=lint), dimension(:,:,:), pointer :: points

     real(kind=sreal), dimension(:,:,:), pointer :: cee_mean
     real(kind=sreal), dimension(:,:,:), pointer :: ctp_mean
     real(kind=sreal), dimension(:,:,:), pointer :: ctp_corrected_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cth_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cth_corrected_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cth2_mean  !FAME-C
     real(kind=sreal), dimension(:,:,:), pointer :: ctt_mean
     real(kind=sreal), dimension(:,:,:), pointer :: ctt_corrected_mean
     real(kind=sreal), dimension(:,:,:), pointer :: stemp_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cct_mean

     real(kind=sreal), dimension(:,:), pointer :: ctp_std
     real(kind=sreal), dimension(:,:,:), pointer :: ctp_mean_cleared
     real(kind=sreal), dimension(:,:,:), pointer :: ctp_phi

  end type l3d_macro_input_struct





  type l3_micro_struct

     integer(kind=lint), dimension(:,:), pointer :: points
     integer(kind=lint), dimension(:,:), pointer :: clear_points
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points_liq
     integer(kind=lint), dimension(:,:), pointer :: cloudy_points_ice

     integer(kind=lint), dimension(:,:), pointer :: cct_points
     integer(kind=lint), dimension(:,:), pointer :: cct_points_clear

     !micro cloud cover
     real(kind=sreal), dimension(:,:), pointer :: cct_mean
     real(kind=sreal), dimension(:,:), pointer :: cct_std

     !liquid and ice cloud cover
     real(kind=sreal), dimension(:,:), pointer :: cct_liq
     real(kind=sreal), dimension(:,:), pointer :: cct_ice

     !      integer(kind=lint), dimension(:,:,:), pointer :: remedian_counter


     !cot
     real(kind=sreal), dimension(:,:), pointer :: cot_mean!,cot_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cot_log_mean
     real(kind=sreal), dimension(:,:), pointer :: cot_std

     real(kind=sreal), dimension(:,:), pointer :: cot_mean_error,cot_std_error


     real(kind=sreal), dimension(:,:), pointer :: cot_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cot_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v1
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v2
     ! 
     !      real(kind=sreal), dimension(:,:), pointer :: cot_median   
     !      real(kind=sreal), dimension(:,:,:,:), pointer :: cot_md


     !ref
     real(kind=sreal), dimension(:,:), pointer :: ref_mean!,ref_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: ref_std

     real(kind=sreal), dimension(:,:), pointer :: ref_mean_error,ref_std_error


     real(kind=sreal), dimension(:,:), pointer :: ref_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: ref_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v1
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v2
     ! 
     !      real(kind=sreal), dimension(:,:), pointer :: ref_median
     !      real(kind=sreal), dimension(:,:,:,:), pointer :: ref_md



     !cwp
     real(kind=sreal), dimension(:,:), pointer :: cwp_mean!,cwp_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cwp_std

     real(kind=sreal), dimension(:,:), pointer :: cwp_mean_error,cwp_std_error


     real(kind=sreal), dimension(:,:), pointer :: cwp_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cwp_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v1
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v2
     ! 
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_median
     !      real(kind=sreal), dimension(:,:,:,:), pointer :: cwp_md


     !lwp
     real(kind=sreal), dimension(:,:), pointer :: cwp_mean_liq!,cwp_mean_error_liq_sq
     real(kind=sreal), dimension(:,:), pointer :: lwp_allsky 
     real(kind=sreal), dimension(:,:), pointer :: cwp_std_liq

     real(kind=sreal), dimension(:,:), pointer :: cwp_mean_error_liq,cwp_std_error_liq


     real(kind=sreal), dimension(:,:), pointer :: cwp_prop_uncertainty_liq
     real(kind=sreal), dimension(:,:), pointer :: cwp_correlated_uncertainty_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v1_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v2_liq


     !     real(kind=sreal), dimension(:,:), pointer :: cwp_median_liq
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cwp_md_liq

     !iwp
     real(kind=sreal), dimension(:,:), pointer :: cwp_mean_ice!,cwp_mean_error_ice_sq
     real(kind=sreal), dimension(:,:), pointer :: iwp_allsky 
     real(kind=sreal), dimension(:,:), pointer :: cwp_std_ice

     real(kind=sreal), dimension(:,:), pointer :: cwp_mean_error_ice,cwp_std_error_ice


     real(kind=sreal), dimension(:,:), pointer :: cwp_prop_uncertainty_ice
     real(kind=sreal), dimension(:,:), pointer :: cwp_correlated_uncertainty_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v1_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cwp_v2_ice


     !     real(kind=sreal), dimension(:,:), pointer :: cwp_median_ice
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cwp_md_ice





     !cloud_albedo1
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean!,cloud_albedo1_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_std

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean_error,cloud_albedo1_std_error


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v1
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v2
     ! 
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_median
     !      real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo1_md

     !cloud_albedo2
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean!,cloud_albedo2_mean_error_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_std

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean_error,cloud_albedo2_std_error


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_correlated_uncertainty
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v1
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v2
     ! 
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_median
     !      real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo2_md


     !cloud albedo1 liq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean_liq!,cloud_albedo1_mean_error_liq_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_std_liq

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean_error_liq,cloud_albedo1_std_error_liq


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_prop_uncertainty_liq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_correlated_uncertainty_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v1_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v2_liq


     !     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_median_liq
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo1_md_liq

     ! cloud albedo1 ice
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean_ice!,cloud_albedo1_mean_error_ice_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_std_ice

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean_error_ice,cloud_albedo1_std_error_ice


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_prop_uncertainty_ice
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_correlated_uncertainty_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v1_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_v2_ice


     !     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_median_ice
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo1_md_ice







     !cloud albedo2 liq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean_liq!,cloud_albedo2_mean_error_liq_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_std_liq

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean_error_liq,cloud_albedo2_std_error_liq


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_prop_uncertainty_liq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_correlated_uncertainty_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v1_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v2_liq

     !     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_median_liq
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo2_md_liq

     ! cloud albedo2 ice
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean_ice!,cloud_albedo2_mean_error_ice_sq
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_std_ice

     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_mean_error_ice,cloud_albedo2_std_error_ice


     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_prop_uncertainty_ice
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_correlated_uncertainty_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v1_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_v2_ice

     !     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo2_median_ice
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cloud_albedo2_md_ice





















     !ref liq
     real(kind=sreal), dimension(:,:), pointer :: ref_mean_liq!,ref_mean_error_liq_sq
     real(kind=sreal), dimension(:,:), pointer :: ref_std_liq

     real(kind=sreal), dimension(:,:), pointer :: ref_mean_error_liq,ref_std_error_liq


     real(kind=sreal), dimension(:,:), pointer :: ref_prop_uncertainty_liq
     real(kind=sreal), dimension(:,:), pointer :: ref_correlated_uncertainty_liq
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v1_liq
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v2_liq

     !     real(kind=sreal), dimension(:,:), pointer :: ref_median_liq
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: ref_md_liq

     !ref ice
     real(kind=sreal), dimension(:,:), pointer :: ref_mean_ice!,ref_mean_error_ice_sq
     real(kind=sreal), dimension(:,:), pointer :: ref_std_ice

     real(kind=sreal), dimension(:,:), pointer :: ref_mean_error_ice,ref_std_error_ice


     real(kind=sreal), dimension(:,:), pointer :: ref_prop_uncertainty_ice
     real(kind=sreal), dimension(:,:), pointer :: ref_correlated_uncertainty_ice
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v1_ice
     !      real(kind=sreal), dimension(:,:), pointer :: ref_v2_ice


     !     real(kind=sreal), dimension(:,:), pointer :: ref_median_ice
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: ref_md_ice

     !cot liq
     real(kind=sreal), dimension(:,:), pointer :: cot_mean_liq!,cot_mean_error_liq_sq
     real(kind=sreal), dimension(:,:), pointer :: cot_std_liq

     real(kind=sreal), dimension(:,:), pointer :: cot_mean_error_liq,cot_std_error_liq


     real(kind=sreal), dimension(:,:), pointer :: cot_prop_uncertainty_liq
     real(kind=sreal), dimension(:,:), pointer :: cot_correlated_uncertainty_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v1_liq
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v2_liq



     !     real(kind=sreal), dimension(:,:), pointer :: cot_median_liq
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cot_md_liq

     !cot ice
     real(kind=sreal), dimension(:,:), pointer :: cot_mean_ice!,cot_mean_error_ice_sq
     real(kind=sreal), dimension(:,:), pointer :: cot_std_ice

     real(kind=sreal), dimension(:,:), pointer :: cot_mean_error_ice,cot_std_error_ice


     real(kind=sreal), dimension(:,:), pointer :: cot_prop_uncertainty_ice
     real(kind=sreal), dimension(:,:), pointer :: cot_correlated_uncertainty_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v1_ice
     !      real(kind=sreal), dimension(:,:), pointer :: cot_v2_ice


     !     real(kind=sreal), dimension(:,:), pointer :: cot_median_ice
     !     real(kind=sreal), dimension(:,:,:,:), pointer :: cot_md_ice



     !cty_day
     real(kind=sreal), dimension(:,:), pointer :: cty_mean
     real(kind=sreal), dimension(:,:), pointer :: cty_std
     real(kind=sreal), dimension(:,:), pointer :: cty_mean_error,cty_std_error
     real(kind=sreal), dimension(:,:), pointer :: cty_prop_uncertainty
     real(kind=sreal), dimension(:,:), pointer :: cty_correlated_uncertainty

     integer(kind=lint), dimension(:,:,:,:,:), pointer :: histogram_cot_ctp_phase

     !no longer in code used
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_cot_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_ref_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_cwp_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: histogram_cloud_albedo1_phase,histogram_cloud_albedo2_phase

  end type l3_micro_struct


  type l3d_micro_struct

     integer(kind=lint), dimension(:,:), pointer :: points
     integer(kind=lint), dimension(:,:), pointer :: pairs

     real(kind=sreal), dimension(:,:), pointer :: cot_mean
     real(kind=sreal), dimension(:,:), pointer :: ref_mean
     real(kind=sreal), dimension(:,:), pointer :: cwp_mean
     real(kind=sreal), dimension(:,:), pointer :: cloud_albedo1_mean,cloud_albedo2_mean
     real(kind=sreal), dimension(:,:), pointer :: cty_mean

     real(kind=sreal), dimension(:,:), pointer :: cot_std,cot_std_corr
     real(kind=sreal), dimension(:,:), pointer :: cot_phi_total

  end type l3d_micro_struct


  type l3d_micro_input_struct

     integer(kind=lint), dimension(:,:,:), pointer :: points

     real(kind=sreal), dimension(:,:,:), pointer :: cot_mean
     real(kind=sreal), dimension(:,:,:), pointer :: ref_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cwp_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cloud_albedo1_mean,cloud_albedo2_mean
     real(kind=sreal), dimension(:,:,:), pointer :: cty_mean

     real(kind=sreal), dimension(:,:), pointer :: cot_std
     real(kind=sreal), dimension(:,:,:), pointer :: cot_mean_cleared
     real(kind=sreal), dimension(:,:,:), pointer :: cot_phi


  end type l3d_micro_input_struct



  type l3_output_struct_2d_old

     integer(kind=lint), dimension(:,:), pointer :: l3_n_points
     !     real(kind=sreal), dimension(:,:), pointer :: l3_sensor_azimuth
     real(kind=sreal), dimension(:,:,:,:,:), pointer :: l3_vector
     integer(kind=lint), dimension(:,:,:,:,:), pointer :: l3_histogram_cot_ctp_phase
     integer(kind=lint), dimension(:,:,:,:), pointer :: l3_histogram_cot_phase

     !     real(kind=sreal), dimension(:,:,:), pointer ::  l3_median_cot

     real(kind=sreal), dimension(:,:,:), pointer ::  l3_median

     real(kind=sreal), dimension(:,:), pointer ::  l3_ctp_log

     !     real(kind=sreal), dimension(:,:,:,:), pointer :: l3_power_spec
     !     real(kind=sreal), dimension(:,:), pointer :: l3_f_para

     !     real(kind=sreal), dimension(:,:,:,:), pointer :: l3_errors


  end type l3_output_struct_2d_old


end module structures
