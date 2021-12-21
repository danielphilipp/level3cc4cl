  ! Name: read_L2B_SUM_ORAC.inc
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
  !2015/04/23 OS changed type of read statement for hist2d_cot_ctp
  !2015-07-07 CP add corrected cth
  !2015/07/10 OS added number of processed orbits
  !2015-07-07 CP add stemp
  !2015-12-16 OS replaced wo with verbose; updated variable read interfaces
  !              to be consistent with new L3 variable naming
  !2016-01-21 OS replaced ref with cer
  !2016-02-18 OS added CTT_corrected, CTP_corrected, CEE
  !2016-02-26 OS read CEE counter
  !2016-03-07 OS now using correct dimension variables for 2D histogram
  !2017-06-22 OS added nretr_clear, stemp_cloudy/clear, toa/boa
  !2017/06/29 OS Added cph_day/uncertainty
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known
  !read more dimension information

  !read global attribute "number_of_processed_orbits"
  !and add to num_of_orbits variable
  ierr = nf90_get_att(ncid, NF90_GLOBAL, "number_of_processed_orbits", l1dvar_dummy)
  num_of_orbits = num_of_orbits + l1dvar_dummy

  allocate(var2d_dummy_float(ncols,nrows))
  var2d_dummy_float=filter_micro

  !   allocate(var2d_dummy_double(ncols,nrows))
  !   var2d_dummy_double=filter_micro

  allocate(var2d_dummy_lint(ncols,nrows))
  var2d_dummy_lint=0

  allocate(var5d_dummy_lint(ncols,nrows,n_hist_cot-1,n_hist_ctp-1,n_hist_phase))
  var5d_dummy_lint=0

  allocate(var4d_dummy_lint_ctp(ncols,nrows,n_ctp_bins,n_hist_phase))
  var4d_dummy_lint_ctp=0

  allocate(var4d_dummy_lint_ctt(ncols,nrows,n_ctt_bins,n_hist_phase))
  var4d_dummy_lint_ctt=0

  allocate(var4d_dummy_lint_cot(ncols,nrows,n_cot_bins,n_hist_phase))
  var4d_dummy_lint_cot=0


  allocate(var4d_dummy_lint_cwp(ncols,nrows,n_cwp_bins,n_hist_phase))
  var4d_dummy_lint_cwp=0

  allocate(var4d_dummy_lint_ref(ncols,nrows,n_ref_bins,n_hist_phase))
  var4d_dummy_lint_ref=0


  allocate(var4d_dummy_lint_cloud_albedo1(ncols,nrows,n_cloud_albedo1_bins,n_hist_phase))
  var4d_dummy_lint_cloud_albedo1=0

  allocate(var4d_dummy_lint_cloud_albedo2(ncols,nrows,n_cloud_albedo2_bins,n_hist_phase))
  var4d_dummy_lint_cloud_albedo2=0

  !variables follow:
  !time, lon,lat,hist_cot,hist_cot_bin,hist_ctp,hist_ctp_bin,hist_phase,hist_cot1d, etc kommt später automatisch oder?

  !npoints
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs',var2d_dummy_lint,verbose)
  l3_macro%points = l3_macro%points + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_day',var2d_dummy_lint,verbose)
  l3_micro%points = l3_micro%points  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_day_liq',var2d_dummy_lint,verbose)
  l3_micro%cloudy_points_liq = l3_micro%cloudy_points_liq  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_day_ice',var2d_dummy_lint,verbose)
  l3_micro%cloudy_points_ice =  l3_micro%cloudy_points_ice  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy',var2d_dummy_lint,verbose)
  l3_macro%cct_points = l3_macro%cct_points  +var2d_dummy_lint
  !  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear',var2d_dummy_lint,verbose)
  !   l3_macro%cct_points_clear = l3_macro%cct_points_clear   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_low',var2d_dummy_lint,verbose)
  l3_macro%cct_low_points = l3_macro%cct_low_points   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_mid',var2d_dummy_lint,verbose)
  l3_macro%cct_middle_points =  l3_macro%cct_middle_points  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_high',var2d_dummy_lint,verbose)
  l3_macro%cct_high_points = l3_macro%cct_high_points   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_cloudy',var2d_dummy_lint,verbose)
  l3_macro%cct_points_raw =  l3_macro%cct_points_raw  +var2d_dummy_lint
  !   CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear',var2d_dummy_lint,verbose)
  !   l3_macro%cct_points_clear_raw = l3_macro%cct_points_clear_raw  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_day',var2d_dummy_lint,verbose)
  l3_micro%cct_points = l3_micro%cct_points   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear_day',var2d_dummy_lint,verbose)
  l3_micro%cct_points_clear =  l3_micro%cct_points_clear   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_cloudy_day',var2d_dummy_lint,verbose)
  l3_macro%cct_points_day = l3_macro%cct_points_day   +var2d_dummy_lint
  !   CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear_day',var2d_dummy_lint,verbose)
  !   l3_macro%cct_points_clear_day =  l3_macro%cct_points_clear_day   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_cloudy_night',var2d_dummy_lint,verbose)
  l3_macro%cct_points_night =  l3_macro%cct_points_night  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear_night',var2d_dummy_lint,verbose)
  l3_macro%cct_points_clear_night =  l3_macro%cct_points_clear_night  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_cloudy_twl',var2d_dummy_lint,verbose)
  l3_macro%cct_points_twl =  l3_macro%cct_points_twl   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nobs_clear_twl',var2d_dummy_lint,verbose)
  l3_macro%cct_points_clear_twl = l3_macro%cct_points_clear_twl   +var2d_dummy_lint
  !CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'npoints_macro_clear',var2d_dummy_lint,verbose)
  !l3_macro%clear_points =  l3_macro%clear_points  +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy',var2d_dummy_lint,verbose)
  l3_macro%cloudy_points = l3_macro%cloudy_points   +var2d_dummy_lint
  !CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'npoints_micro_clear',var2d_dummy_lint,verbose)
  !l3_micro%clear_points = l3_micro%clear_points   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_day',var2d_dummy_lint,verbose)
  l3_micro%cloudy_points = l3_micro%cloudy_points   +var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_liq',var2d_dummy_lint,verbose)
  l3_macro%cloudy_points_cph_liq = l3_macro%cloudy_points_cph_liq + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cloudy_ice',var2d_dummy_lint,verbose)
  l3_macro%cloudy_points_cph_ice = l3_macro%cloudy_points_cph_ice + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_cee',var2d_dummy_lint,verbose)
  l3_macro%cee_points = l3_macro%cee_points + var2d_dummy_lint

  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_sw',var2d_dummy_lint,verbose)
  l3_macro%rad_points_sw = l3_macro%rad_points_sw + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_lw',var2d_dummy_lint,verbose)
  l3_macro%rad_points_lw = l3_macro%rad_points_lw + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_sw_low',var2d_dummy_lint,verbose)
  l3_macro%rad_points_sw_low = l3_macro%rad_points_sw_low + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_lw_low',var2d_dummy_lint,verbose)
  l3_macro%rad_points_lw_low = l3_macro%rad_points_lw_low + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_sw_mid',var2d_dummy_lint,verbose)
  l3_macro%rad_points_sw_mid = l3_macro%rad_points_sw_mid + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_lw_mid',var2d_dummy_lint,verbose)
  l3_macro%rad_points_lw_mid = l3_macro%rad_points_lw_mid + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_sw_hig',var2d_dummy_lint,verbose)
  l3_macro%rad_points_sw_hig = l3_macro%rad_points_sw_hig + var2d_dummy_lint
  CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_lw_hig',var2d_dummy_lint,verbose)
  l3_macro%rad_points_lw_hig = l3_macro%rad_points_lw_hig + var2d_dummy_lint


CALL nc_read_l2b_sum_2d_long_to_long(ncid,ncols,nrows,'nretr_clear',var2d_dummy_lint,verbose)
  l3_macro%stemp_clear_points = l3_macro%stemp_clear_points + var2d_dummy_lint

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt',var2d_dummy_float,verbose)
  l3_macro%ctt_mean = l3_macro%ctt_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_std',var2d_dummy_float,verbose)
  l3_macro%ctt_std = l3_macro%ctt_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_unc',var2d_dummy_float,verbose)
  l3_macro%ctt_mean_error = l3_macro%ctt_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_unc_std',var2d_dummy_float,verbose)
  l3_macro%ctt_std_error = l3_macro%ctt_std_error + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_corrected',var2d_dummy_float,verbose)
  l3_macro%ctt_corrected_mean = l3_macro%ctt_corrected_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_corrected_std',var2d_dummy_float,verbose)
  l3_macro%ctt_corrected_std = l3_macro%ctt_corrected_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_corrected_unc',var2d_dummy_float,verbose)
  l3_macro%ctt_corrected_mean_error = l3_macro%ctt_corrected_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctt_corrected_unc_std',var2d_dummy_float,verbose)
  l3_macro%ctt_corrected_std_error = l3_macro%ctt_corrected_std_error + var2d_dummy_float


  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_cloudy',var2d_dummy_float,verbose)
  l3_macro%stemp_mean = l3_macro%stemp_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_cloudy_std',var2d_dummy_float,verbose)
  l3_macro%stemp_std = l3_macro%stemp_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_cloudy_unc',var2d_dummy_float,verbose)
  l3_macro%stemp_mean_error = l3_macro%stemp_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_cloudy_unc_std',var2d_dummy_float,verbose)
  l3_macro%stemp_std_error = l3_macro%stemp_std_error + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_clear',var2d_dummy_float,verbose)
  l3_macro%stemp_clear_mean = l3_macro%stemp_clear_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_clear_std',var2d_dummy_float,verbose)
  l3_macro%stemp_clear_std = l3_macro%stemp_clear_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_clear_unc',var2d_dummy_float,verbose)
  l3_macro%stemp_clear_mean_error = l3_macro%stemp_clear_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'stemp_clear_unc_std',var2d_dummy_float,verbose)
  l3_macro%stemp_clear_std_error = l3_macro%stemp_clear_std_error + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth',var2d_dummy_float,verbose)
  l3_macro%cth_mean = l3_macro%cth_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_std',var2d_dummy_float,verbose)
  l3_macro%cth_std = l3_macro%cth_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_unc',var2d_dummy_float,verbose)
  l3_macro%cth_mean_error = l3_macro%cth_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_unc_std',var2d_dummy_float,verbose)
  l3_macro%cth_std_error = l3_macro%cth_std_error + var2d_dummy_float
  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth',var2d_dummy_float,verbose)
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_corrected',var2d_dummy_float,verbose)
  l3_macro%cth_corrected_mean = l3_macro%cth_corrected_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_corrected_std',var2d_dummy_float,verbose)
  l3_macro%cth_corrected_std = l3_macro%cth_corrected_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_corrected_unc',var2d_dummy_float,verbose)
  l3_macro%cth_corrected_mean_error = l3_macro%cth_corrected_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_corrected_unc_std',var2d_dummy_float,verbose)
  l3_macro%cth_corrected_std_error = l3_macro%cth_corrected_std_error + var2d_dummy_float
  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth_corrected',var2d_dummy_float,verbose)

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then

     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth2',var2d_dummy_float,verbose)
     l3_macro%cth2_mean = l3_macro%cth2_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth2_std',var2d_dummy_float,verbose)
     l3_macro%cth2_std = l3_macro%cth2_std + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth2_uncertainty',var2d_dummy_float,verbose)
     l3_macro%cth2_mean_error = l3_macro%cth2_mean_error + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cth2_uncertainty_std',var2d_dummy_float,verbose)
     l3_macro%cth2_std_error = l3_macro%cth2_std_error + var2d_dummy_float

  endif

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp',var2d_dummy_float,verbose)
  l3_macro%ctp_mean = l3_macro%ctp_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_std',var2d_dummy_float,verbose)
  l3_macro%ctp_std = l3_macro%ctp_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_unc',var2d_dummy_float,verbose)
  l3_macro%ctp_mean_error = l3_macro%ctp_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_unc_std',var2d_dummy_float,verbose)
  l3_macro%ctp_std_error = l3_macro%ctp_std_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_log',var2d_dummy_float,verbose)
  l3_macro%ctp_log_mean = l3_macro%ctp_log_mean + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_corrected',var2d_dummy_float,verbose)
  l3_macro%ctp_corrected_mean = l3_macro%ctp_corrected_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_corrected_std',var2d_dummy_float,verbose)
  l3_macro%ctp_corrected_std = l3_macro%ctp_corrected_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_corrected_unc',var2d_dummy_float,verbose)
  l3_macro%ctp_corrected_mean_error = l3_macro%ctp_corrected_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'ctp_corrected_unc_std',var2d_dummy_float,verbose)
  l3_macro%ctp_corrected_std_error = l3_macro%ctp_corrected_std_error + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc',var2d_dummy_float,verbose)
  l3_macro%cct_mean = l3_macro%cct_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_day',var2d_dummy_float,verbose)
  l3_macro%cct_day = l3_macro%cct_day + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_night',var2d_dummy_float,verbose)
  l3_macro%cct_night = l3_macro%cct_night + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_twl',var2d_dummy_float,verbose)
  l3_macro%cct_twl = l3_macro%cct_twl + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_std',var2d_dummy_float,verbose)
  l3_macro%cct_std =l3_macro%cct_std  + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cee',var2d_dummy_float,verbose)
  l3_macro%cee_mean = l3_macro%cee_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cee_std',var2d_dummy_float,verbose)
  l3_macro%cee_std = l3_macro%cee_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cee_unc',var2d_dummy_float,verbose)
  l3_macro%cee_mean_error = l3_macro%cee_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cee_unc_std',var2d_dummy_float,verbose)
  l3_macro%cee_std_error = l3_macro%cee_std_error + var2d_dummy_float

  !   CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_micro',var2d_dummy_float,verbose)
  !   l3_micro%cct_mean = l3_micro%cct_mean + var2d_dummy_float
  !   CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_micro_std',var2d_dummy_float,verbose)
  !   l3_micro%cct_std = l3_micro%cct_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_unc',var2d_dummy_float,verbose)
  l3_macro%cct_mean_error = l3_macro%cct_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_unc_std',var2d_dummy_float,verbose)
  l3_macro%cct_std_error = l3_macro%cct_std_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_low',var2d_dummy_float,verbose)
  l3_macro%cct_mean_low = l3_macro%cct_mean_low + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_mid',var2d_dummy_float,verbose)
  l3_macro%cct_mean_middle = l3_macro%cct_mean_middle + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cfc_high',var2d_dummy_float,verbose)
  l3_macro%cct_mean_high = l3_macro%cct_mean_high + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph',var2d_dummy_float,verbose)
  l3_macro%cty_mean = l3_macro%cty_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_std',var2d_dummy_float,verbose)
  l3_macro%cty_std = l3_macro%cty_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_unc',var2d_dummy_float,verbose)
  l3_macro%cty_mean_error = l3_macro%cty_mean_error +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_unc_std',var2d_dummy_float,verbose)
  l3_macro%cty_std_error = l3_macro%cty_std_error +  var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_day',var2d_dummy_float,verbose)
  l3_micro%cty_mean = l3_micro%cty_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_day_std',var2d_dummy_float,verbose)
  l3_micro%cty_std = l3_micro%cty_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_day_unc',var2d_dummy_float,verbose)
  l3_micro%cty_mean_error = l3_micro%cty_mean_error +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cph_day_unc_std',var2d_dummy_float,verbose)
  l3_micro%cty_std_error = l3_micro%cty_std_error +  var2d_dummy_float
  
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer',var2d_dummy_float,verbose)
  l3_micro%ref_mean = l3_micro%ref_mean +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_std',var2d_dummy_float,verbose)
  l3_micro%ref_std = l3_micro%ref_std +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_unc',var2d_dummy_float,verbose)
  l3_micro%ref_mean_error = l3_micro%ref_mean_error +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_unc_std',var2d_dummy_float,verbose)
  l3_micro%ref_std_error = l3_micro%ref_std_error +  var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean = l3_micro%cloud_albedo1_mean +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std = l3_micro%cloud_albedo1_std +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean_error = l3_micro%cloud_albedo1_mean_error +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std_error = l3_micro%cloud_albedo1_std_error +  var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean = l3_micro%cloud_albedo2_mean +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std = l3_micro%cloud_albedo2_std +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean_error = l3_micro%cloud_albedo2_mean_error +  var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std_error = l3_micro%cloud_albedo2_std_error +  var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot',var2d_dummy_float,verbose)
  l3_micro%cot_mean = l3_micro%cot_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_std',var2d_dummy_float,verbose)
  l3_micro%cot_std = l3_micro%cot_std + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_log',var2d_dummy_float,verbose)
  l3_micro%cot_log_mean = l3_micro%cot_log_mean + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_unc',var2d_dummy_float,verbose)
  l3_micro%cot_mean_error = l3_micro%cot_mean_error + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_unc_std',var2d_dummy_float,verbose)
  l3_micro%cot_std_error = l3_micro%cot_std_error + var2d_dummy_float

  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cwp',var2d_dummy_float,verbose)
  !l3_micro%cwp_mean = l3_micro%cwp_mean + var2d_dummy_float
  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cwp_std',var2d_dummy_float,verbose)
  !l3_micro%cwp_std =l3_micro%cwp_std  + var2d_dummy_float
  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cwp_unc',var2d_dummy_float,verbose)
  !l3_micro%cwp_mean_error = l3_micro%cwp_mean_error + var2d_dummy_float
  !CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cwp_unc_std',var2d_dummy_float,verbose)
  !l3_micro%cwp_std_error = l3_micro%cwp_std_error + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'lwp',var2d_dummy_float,verbose)
  l3_micro%cwp_mean_liq = l3_micro%cwp_mean_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'lwp_std',var2d_dummy_float,verbose)
  l3_micro%cwp_std_liq = l3_micro%cwp_std_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'lwp_unc',var2d_dummy_float,verbose)
  l3_micro%cwp_mean_error_liq = l3_micro%cwp_mean_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'lwp_unc_std',var2d_dummy_float,verbose)
  l3_micro%cwp_std_error_liq = l3_micro%cwp_std_error_liq + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'iwp',var2d_dummy_float,verbose)
  l3_micro%cwp_mean_ice = l3_micro%cwp_mean_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'iwp_std',var2d_dummy_float,verbose)
  l3_micro%cwp_std_ice = l3_micro%cwp_std_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'iwp_unc',var2d_dummy_float,verbose)
  l3_micro%cwp_mean_error_ice = l3_micro%cwp_mean_error_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'iwp_unc_std',var2d_dummy_float,verbose)
  l3_micro%cwp_std_error_ice = l3_micro%cwp_std_error_ice + var2d_dummy_float


  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_liq',var2d_dummy_float,verbose)
  l3_micro%ref_mean_liq = l3_micro%ref_mean_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_liq_std',var2d_dummy_float,verbose)
  l3_micro%ref_std_liq = l3_micro%ref_std_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_liq_unc',var2d_dummy_float,verbose)
  l3_micro%ref_mean_error_liq = l3_micro%ref_mean_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_liq_unc_std',var2d_dummy_float,verbose)
  l3_micro%ref_std_error_liq = l3_micro%ref_std_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_ice',var2d_dummy_float,verbose)
  l3_micro%ref_mean_ice = l3_micro%ref_mean_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_ice_std',var2d_dummy_float,verbose)
  l3_micro%ref_std_ice = l3_micro%ref_std_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_ice_unc',var2d_dummy_float,verbose)
  l3_micro%ref_mean_error_ice = l3_micro%ref_mean_error_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cer_ice_unc_std',var2d_dummy_float,verbose)
  l3_micro%ref_std_error_ice = l3_micro%ref_std_error_ice + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_liq',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean_liq = l3_micro%cloud_albedo1_mean_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_liq_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std_liq = l3_micro%cloud_albedo1_std_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_liq_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean_error_liq = l3_micro%cloud_albedo1_mean_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_liq_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std_error_liq = l3_micro%cloud_albedo1_std_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_ice',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean_ice = l3_micro%cloud_albedo1_mean_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_ice_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std_ice = l3_micro%cloud_albedo1_std_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_ice_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_mean_error_ice = l3_micro%cloud_albedo1_mean_error_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis006_ice_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo1_std_error_ice = l3_micro%cloud_albedo1_std_error_ice + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_liq',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean_liq = l3_micro%cloud_albedo2_mean_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_liq_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std_liq = l3_micro%cloud_albedo2_std_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_liq_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean_error_liq = l3_micro%cloud_albedo2_mean_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_liq_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std_error_liq = l3_micro%cloud_albedo2_std_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_ice',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean_ice = l3_micro%cloud_albedo2_mean_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_ice_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std_ice = l3_micro%cloud_albedo2_std_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_ice_unc',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_mean_error_ice = l3_micro%cloud_albedo2_mean_error_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cla_vis008_ice_unc_std',var2d_dummy_float,verbose)
  l3_micro%cloud_albedo2_std_error_ice = l3_micro%cloud_albedo2_std_error_ice + var2d_dummy_float

  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_liq',var2d_dummy_float,verbose)
  l3_micro%cot_mean_liq = l3_micro%cot_mean_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_liq_std',var2d_dummy_float,verbose)
  l3_micro%cot_std_liq = l3_micro%cot_std_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_liq_unc',var2d_dummy_float,verbose)
  l3_micro%cot_mean_error_liq = l3_micro%cot_mean_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_liq_unc_std',var2d_dummy_float,verbose)
  l3_micro%cot_std_error_liq = l3_micro%cot_std_error_liq + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_ice',var2d_dummy_float,verbose)
  l3_micro%cot_mean_ice = l3_micro%cot_mean_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_ice_std',var2d_dummy_float,verbose)
  l3_micro%cot_std_ice = l3_micro%cot_std_ice  + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_ice_unc',var2d_dummy_float,verbose)
  l3_micro%cot_mean_error_ice = l3_micro%cot_mean_error_ice + var2d_dummy_float
  CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'cot_ice_unc_std',var2d_dummy_float,verbose)
  l3_micro%cot_std_error_ice = l3_micro%cot_std_error_ice + var2d_dummy_float

  !toa
  if (proc_toa) then
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swup',var2d_dummy_float,verbose)
     l3_macro%toa_swup_mean = l3_macro%toa_swup_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swdn',var2d_dummy_float,verbose)
     l3_macro%toa_swdn_mean = l3_macro%toa_swdn_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swup_clr',var2d_dummy_float,verbose)
     l3_macro%toa_swup_clr_mean = l3_macro%toa_swup_clr_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_lwup',var2d_dummy_float,verbose)
     l3_macro%toa_lwup_mean = l3_macro%toa_lwup_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_lwup_clr',var2d_dummy_float,verbose)
     l3_macro%toa_lwup_clr_mean = l3_macro%toa_lwup_clr_mean + var2d_dummy_float

     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swup_low',var2d_dummy_float,verbose)
     l3_macro%toa_swup_low_mean = l3_macro%toa_swup_low_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swup_mid',var2d_dummy_float,verbose)
     l3_macro%toa_swup_mid_mean = l3_macro%toa_swup_mid_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_swup_hig',var2d_dummy_float,verbose)
     l3_macro%toa_swup_hig_mean = l3_macro%toa_swup_hig_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_lwup_low',var2d_dummy_float,verbose)
     l3_macro%toa_lwup_low_mean = l3_macro%toa_lwup_low_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_lwup_mid',var2d_dummy_float,verbose)
     l3_macro%toa_lwup_mid_mean = l3_macro%toa_lwup_mid_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'toa_lwup_hig',var2d_dummy_float,verbose)
     l3_macro%toa_lwup_hig_mean = l3_macro%toa_lwup_hig_mean + var2d_dummy_float

     !boa
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_swup',var2d_dummy_float,verbose)
     l3_macro%boa_swup_mean = l3_macro%boa_swup_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_swup_clr',var2d_dummy_float,verbose)
     l3_macro%boa_swup_clr_mean = l3_macro%boa_swup_clr_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_lwup',var2d_dummy_float,verbose)
     l3_macro%boa_lwup_mean = l3_macro%boa_lwup_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_lwup_clr',var2d_dummy_float,verbose)
     l3_macro%boa_lwup_clr_mean = l3_macro%boa_lwup_clr_mean + var2d_dummy_float

     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_swdn',var2d_dummy_float,verbose)
     l3_macro%boa_swdn_mean = l3_macro%boa_swdn_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_swdn_clr',var2d_dummy_float,verbose)
     l3_macro%boa_swdn_clr_mean = l3_macro%boa_swdn_clr_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_lwdn',var2d_dummy_float,verbose)
     l3_macro%boa_lwdn_mean = l3_macro%boa_lwdn_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_lwdn_clr',var2d_dummy_float,verbose)
     l3_macro%boa_lwdn_clr_mean = l3_macro%boa_lwdn_clr_mean + var2d_dummy_float

     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_par_dif',var2d_dummy_float,verbose)
     l3_macro%boa_par_dif_mean = l3_macro%boa_par_dif_mean + var2d_dummy_float
     CALL nc_read_l2b_sum_2d_float_to_float(ncid,ncols,nrows,'boa_par_tot',var2d_dummy_float,verbose)
     l3_macro%boa_par_tot_mean = l3_macro%boa_par_tot_mean + var2d_dummy_float
  endif

  CALL nc_read_l2b_sum_5d_long_to_long(ncid,ncols,nrows,n_hist_cot-1,n_hist_ctp-1,n_hist_phase,'hist2d_cot_ctp',var5d_dummy_lint,verbose)
  l3_micro%histogram_cot_ctp_phase=l3_micro%histogram_cot_ctp_phase+var5d_dummy_lint
  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_ctp_bins,n_hist_phase,'hist1d_ctp',var4d_dummy_lint_ctp,verbose)
  l3_macro%histogram_ctp_phase=l3_macro%histogram_ctp_phase+var4d_dummy_lint_ctp
  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_ctt_bins,n_hist_phase,'hist1d_ctt',var4d_dummy_lint_ctt,verbose)
  l3_macro%histogram_ctt_phase=l3_macro%histogram_ctt_phase+var4d_dummy_lint_ctt
  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_cot_bins,n_hist_phase,'hist1d_cot',var4d_dummy_lint_cot,verbose)
  l3_micro%histogram_cot_phase= l3_micro%histogram_cot_phase+var4d_dummy_lint_cot
  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_cwp_bins,n_hist_phase,'hist1d_cwp',var4d_dummy_lint_cwp,verbose)
  l3_micro%histogram_cwp_phase=l3_micro%histogram_cwp_phase+var4d_dummy_lint_cwp

  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_ref_bins,n_hist_phase,'hist1d_cer',var4d_dummy_lint_ref,verbose)
  l3_micro%histogram_ref_phase=l3_micro%histogram_ref_phase+var4d_dummy_lint_ref


  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_cloud_albedo1_bins,n_hist_phase,'hist1d_cla_vis006',var4d_dummy_lint_cloud_albedo1,verbose)
  l3_micro%histogram_cloud_albedo1_phase=l3_micro%histogram_cloud_albedo1_phase+var4d_dummy_lint_cloud_albedo1


  CALL  nc_read_l2b_sum_4d_long_to_long(ncid,ncols,nrows,n_cloud_albedo2_bins,n_hist_phase,'hist1d_cla_vis008',var4d_dummy_lint_cloud_albedo2,verbose)
  l3_micro%histogram_cloud_albedo2_phase=l3_micro%histogram_cloud_albedo2_phase+var4d_dummy_lint_cloud_albedo2

  deallocate(var2d_dummy_float)
  deallocate(var2d_dummy_lint)
  deallocate(var4d_dummy_lint_ctp)
  deallocate(var4d_dummy_lint_ctt)
  !deallocate(var4d_dummy_lint_stemp)
  deallocate(var4d_dummy_lint_cot)
  deallocate(var4d_dummy_lint_ref)
  deallocate(var4d_dummy_lint_cloud_albedo1)
  deallocate(var4d_dummy_lint_cloud_albedo2)
  deallocate(var4d_dummy_lint_cwp)
  deallocate(var5d_dummy_lint)
