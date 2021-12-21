  ! Name: write_L3abc_data.inc
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
  !2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
  !2013/04/12 Cintia Carbajal Henken added variables ice/liq for cot and ref
  !2013/06/11 MJ implemented code to enable writing of netcdf-4 output files.
  !2013/09/27 MJ cleans up and rearranges code
  !2014/03/28 Cintia Carbajal Henken added cth2 with if statements for FAME-C
  !2015/03/11 CP adds cloud albedo
  !2015/03/11 CP adds uncertainty info
  !2015/04/23 OS added some units; altered CTH and cloud albedo min/max values;
  !             added missing definitions for some cloud albedo variables
  !2015/07/07 CP adds corrected cloud top height
  !2015/07/10 OS added num_of_orbits, some variable units, cph_day,
  !              cfc_uncertainty and cty_day; now closing correct
  !              output file
  !2015/10/20 CP CF compliance and comments add stemp
  !2015/12/17 OS replaced wo with verbose; updated L3 variable names;
  !              removed bug in stemd IDs; removed output of CWP
  !2016/01/21 OS replaced ref with cer; increased vmax for nobs to 100000;
  !              renamed correlated to corr; added vmin and vmax for LWP;
  !              added allsky LWP and IWP
  !2016/02/18 OS replacing some hard-coded vmin/vmax with variables, added variables CEE, CTT_corrected, CTP_corrected
  !2016/02/26 OS some NetCDF variable unit/comment/name changes; allsky LWP/IWP now calculated with npoints of
  !              retrieved daytime pixels, not observed daytime pixels
  !2016/02/26 OS changed write_unc_std to write_l2b_sum_variables; write CEE counter into L2B_SUM files
  !2016/03/04 OS several changes in NetCDF attributes, comments, and variable names towards CF compliancy
  !2016/03/18 OS changed cloud water to cloud ice water
  !2017/06/22 OS Added toa/boa, nretr_clear, and stemp_cloudy/clear
  !2017/06/29 OS Added cph/_day/_unc

  ! $Id$
  !
  ! Bugs:
  !
  !none known

  if (verbose .eq. 1) write(*,*) "creating file ", trim(adjustl(l3_outputpath_and_file))

  if (verbose .eq. 1) write(*,*) "set global attributes"

  call nc_create_global( trim(adjustl(l3_outputpath_and_file)), ncols, nrows, gridx, gridy,&
       & slon,elon,slat,elat, &
       & ncid_out, dims_var, verbose,hist_cot,hist_ctp,hist_cot_bin,hist_ctp_bin,dims_var_hist2d, dims_var_phase, &
       & hist_cot_1d_axis, hist_cot_1d_bin,dims_var_histcot1d, &
       & hist_ctp_1d_axis, hist_ctp_1d_bin,dims_var_histctp1d, &
       & hist_ctt_1d_axis, hist_ctt_1d_bin,dims_var_histctt1d, &
       & hist_ref_1d_axis, hist_ref_1d_bin,dims_var_histref1d, &
       & hist_cwp_1d_axis, hist_cwp_1d_bin,dims_var_histcwp1d, &
       & hist_cloud_albedo1_1d_axis, hist_cloud_albedo1_1d_bin,dims_var_histcloud_albedo11d, &
       & hist_cloud_albedo2_1d_axis, hist_cloud_albedo2_1d_bin,dims_var_histcloud_albedo21d, &
       & cncver,ccon,cinst,l2cproc,l2cprocver,l3cproc,l3cprocver,contact,website, &
       & prodtype, platform, csensor, uuid_tag, exec_time, prod_name, year,  month, day, dom, dur, cgridx, cgridy, &
       & grid_type, reference, history, summary, keywords, comment, project,license,cfile_version,csource, standard_name_voc, &
       & naming_authority, cdm_data_type, keywords_vocabulary, daystart, calgo, num_of_orbits)

  !activate and modify parameter list once this is clear
  !  CALL nc_global_att_L3(ncid_new, fname_newL3, valid, st, st_iso, &
  !       tem, spa, instr, satser, satids, lev, type, yday, daynight, verbose, v_prod, proc_v_l2, proc_v_l3)

  !define and write here the actual data now!!!!

  !number of points
  vmax = 200000

  if (verbose .eq. 1) write(*,*) "defining output data"

  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs', vid1pmac, 'number of observations', &
       & 'number_of_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'total number of satellite observations available')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_day', vid1pmic, 'number of daytime observations', &
       & 'number_of_daytime_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'total number of satellite observations available')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_clear_day',vid1pmic_cct_clear,&
       & 'number of clear daytime observations', &
       & 'number_of_clear_daytime_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_cloudy_day',vid1pmac_cct_day_cloudy,&
       & 'number of cloudy daytime observations', &
       & 'number_of_cloudy_daytime_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_clear_night',vid1pmac_cct_night_clear,&
       & 'number of clear nighttime observations', &
       & 'number_of_clear_nighttime_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_cloudy_night',vid1pmac_cct_night_cloudy,&
       & 'number of cloudy nighttime observations', &
       & 'number_of_cloudy_nighttime_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_clear_twl',vid1pmac_cct_twl_clear,&
       & 'number of clear twilight observations', &
       & 'number_of_clear_twilight_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_cloudy_twl',vid1pmac_cct_twl_cloudy,&
       & 'number of cloudy twilight observations', &
       & 'number_of_cloudy_twilight_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')

  !   CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_clear',vid1pmac_cct_clear, 'number of cloud free classified points', &
  !        & 'number_of_L2_points_macroscopic_cct_clear', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nobs_cloudy',&
       & vid1pmac_cct_cloudy_raw, 'number of cloudy observations', &
       & 'number_of_cloudy_observations', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')

  ! nretr
  vmax = 200000
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_clear', vid1pmac_stemp_clear, &
       & 'number of valid clear sky retrievals', &
       & 'number_of_valid_clear_sky_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'number of valid clear sky retrievals, not valid = cloudy/high cost/fill_value/not converged')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy', vid1pmac_cloudy, &
       & 'number of valid cloudy retrievals', &
       & 'number_of_valid_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'number of valid cloudy retrievals, not valid = clear/high cost/fill_value/not converged')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_liq', vid1pmac_cph_liq, &
       & 'number of valid liquid phase cloudy retrievals', &
       & 'number_of_valid_liquid_phase_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'number of valid liquid phase cloudy retrievals, not valid = clear/high cost/fill_value/not converged')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_ice', vid1pmac_cph_ice, &
       & 'number of valid ice phase cloudy retrievals', &
       & 'number_of_valid_ice_phase_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'number of valid ice phase cloudy retrievals, not valid = clear/high cost/fill_value/not converged')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_day', vid1pmic_cloudy, &
       & 'number of valid daytime cloudy retrievals', &
       & 'number_of_valid_daytime_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_day_liq', vid1pmic_cloudy_liq, &
       & 'number of valid liquid phase daytime cloudy retrievals', &
       & 'number_of_valid_liquid_phase_daytime_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_day_ice', vid1pmic_cloudy_ice, &
       & 'number of valid ice phase daytime cloudy retrievals', &
       & 'number_of_valid_ice_phase_daytime_cloudy_retrievals', '1', long_int_fill_value,  verbose,1,0,0,vmax,0,0,'')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_low', vid1pmac_cct_low_cloudy, &
       & 'number of valid low level cloudy retrievals', &
       & 'number_of_valid_low_level_cloudy_retrievals', '1',long_int_fill_value, verbose,1,0,0,vmax,0,1,'680 hPa < ctp < 1100 hPa')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_mid', vid1pmac_cct_middle_cloudy, &
       & 'number of valid mid level cloudy retrievals', &
       & 'number_of_valid_mid_level_cloudy_retrievals', '1', long_int_fill_value, verbose,1,0,0,vmax,0,1,'440 hPa < ctp < 680 hPa')
  CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cloudy_high', vid1pmac_cct_high_cloudy, &
       & 'number of valid high level cloudy retrievals', &
       & 'number_of_valid_high_level_cloudy_retrievals', '1',long_int_fill_value, verbose,1,0,0,vmax,0,1,'ctp < 440 hPa')
  if (write_l2b_sum_variables) CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_cee', vid1pmac_cee, 'number of cloud effective emissivity points', &
       & 'number_of_cloud_effective_emissivity_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')

  if (write_l2b_sum_variables) then
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_sw', vid1pmac_sw, 'number of retrieved sw flux points', &
         & 'number_of_retrieved_sw_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_lw', vid1pmac_lw, 'number of retrieved lw flux points', &
         & 'number_of_retrieved_lw_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_sw_low', vid1pmac_sw_low, 'number of retrieved sw_low flux points', &
         & 'number_of_retrieved_sw_low_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_lw_low', vid1pmac_lw_low, 'number of retrieved lw_low flux points', &
         & 'number_of_retrieved_lw_low_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_sw_mid', vid1pmac_sw_mid, 'number of retrieved sw_mid flux points', &
         & 'number_of_retrieved_sw_mid_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_lw_mid', vid1pmac_lw_mid, 'number of retrieved lw_mid flux points', &
         & 'number_of_retrieved_lw_mid_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_sw_hig', vid1pmac_sw_hig, 'number of retrieved sw_hig flux points', &
         & 'number_of_retrieved_sw_hig_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
    CALL nc_defdata_lint(ncid_out, dims_var, 'nretr_lw_hig', vid1pmac_lw_hig, 'number of retrieved lw_hig flux points', &
         & 'number_of_retrieved_lw_hig_flux_points', '1', long_int_fill_value, verbose,1,0,0,vmax,0,0,'')
  endif


  ! cfc
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=1.0

  comment_prop_unc='total uncertainty from individual pixel uncertainty added in quadrature'
  comment_mean_unc='mean of the pixel uncertainties'
  comment_corr_unc='correlated uncertainty assuming correlation of 0.1'
  comment_std_unc='standard deviation of retrieved values'
  comment_std_unc_unc='standard deviation of uncertainty values'

  CALL nc_defdata_float(ncid_out, dims_var, 'cfc', vid8m, 'cloud fraction', &
       & 'cloud_area_fraction', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_std', vid8s, 'cloud fraction standard deviation', &
       & 'cloud_area_fraction', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_prop_unc', vid8wm, 'cloud fraction propagated uncertainty', &
       & 'cloud_area_fraction standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_corr_unc', vid8ws, 'cloud fraction correlated uncertainty', &
       & 'cloud_area_fraction standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_unc', vid8e, 'cloud fraction uncertainty', &
       & 'cloud_area_fraction standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cfc_unc_std', vid8es, 'cloud fraction uncertainty standard deviation', &
          & 'cloud_area_fraction standard error', '%',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_std_unc_unc)
  endif
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_low', vid8low, 'low level cloud fraction', &
       & 'cloud_area_fraction_in_atmosphere_layer', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,'low level clouds = 680 hPa < ctp < 1100 hPa')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_mid', vid8middle, 'mid level cloud fraction', &
       & 'cloud_area_fraction_in_atmosphere_layer', '1',&
       & real(real_fill_value,kind=sreal),verbose,scale,offset,vmin,vmax,0,1,'mid level clouds = 440 hPa < ctp < 680 hPa')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_high', vid8high, 'high level cloud fraction', &
       & 'cloud_area_fraction_in_atmosphere_layer', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,'high level clouds = ctp < 440 hPa')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cfc_micro', vid8mmicro, 'cloud fraction micro', &
  !        & 'cloud_area_fraction', '1',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cfc_micro_std', vid8smicro, 'cloud fraction standard deviation micro', &
  !        & 'cloud_area_fraction', '1',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_day', vid8mday, 'cloud fraction day', &
       & 'cloud_area_fraction', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_night', vid8mnight, 'cloud fraction night', &
       & 'cloud_area_fraction', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cfc_twl', vid8mtwl, 'cloud fraction twilight', &
       & 'cloud_area_fraction', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cfc_raw', vid8m_raw, 'cloud fraction raw', &
  !        & 'cloud_area_fraction', '1',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cfc_post_raw', vid8m_post_raw, 'cloud fraction raw', &
  !        & 'cloud_area_fraction', '1',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! write ctt related information
  ! ctt
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.
  vmax=320.
  vmax_sq=1.0E12
  comment_ctt = 'CTT retrieved at approx. 1 optical depth below cloud top'

  CALL nc_defdata_float(ncid_out, dims_var, 'ctt', vid1m, 'cloud top temperature', &
       & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,comment_ctt)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_std', vid1s, 'cloud top temperature standard deviation', &
       & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_prop_unc', vid1wm, 'cloud top temperature propagated uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corr_unc', vid1ws, 'cloud top temperature correlated uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_unc', vid1e, 'cloud top temperature uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctt_unc_sq', vid1esq, 'cloud top temperature uncertainty sq', &
  !        & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'ctt_unc_std', vid1es, 'cloud top temperature uncertainty standard deviation', &
          & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctt_med', vid1med, 'cloud top temperature median', &
  !        & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! ctt_corrected
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.
  vmax=320.
  vmax_sq=1.0E12
  comment_ctt_corrected = 'as CTT, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected', vid17m, 'cloud top temperature', &
       & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_ctt_corrected)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_std', vid17s, 'cloud top temperature standard deviation', &
       & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_prop_unc', vid17wm, 'cloud top temperature propagated uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_corr_unc', vid17ws, 'cloud top temperature correlated uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_unc', vid17e, 'cloud top temperature uncertainty', &
       & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_unc_sq', vid17esq, 'cloud top temperature uncertainty sq', &
  !        & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_unc_std', vid17es, 'cloud top temperature uncertainty standard deviation', &
          & 'air_temperature_at_cloud_top standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctt_corrected_med', vid17med, 'cloud top temperature median', &
  !        & 'air_temperature_at_cloud_top', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')


  ! write stemp related information
  ! stemp

  scale=1.00
  offset=0.00
  vmin=0.
  vmax=320.
  vmax_sq=1.0E12
  comment_stemp='stemp is retrieved only under optically thin clouds, and otherwise reverts to the apriori ECMWF ERA-Interim stemp value'

  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy', vid16m, 'surface temperature', &
       & 'surface_temperature', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_stemp)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy_std', vid16s, 'surface temperature standard deviation', &
       & 'surface_temperature', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy_prop_unc', vid16wm, 'surface temperature propagated uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy_corr_unc', vid16ws, 'surface temperature correlated uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy_unc', vid16e, 'surface temperature uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'stemp_cloudy_unc_std', vid16es, 'surface temperature uncertainty standard deviation', &
          & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  ! write stemp clear sky related information
  ! stemp

  scale=1.00
  offset=0.00
  vmin=0.
  vmax=320.
  vmax_sq=1.0E12
  comment_stemp='clear sky retrieved surface temperature'

  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear', vid19m, 'clear sky surface temperature', &
       & 'surface_temperature', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_stemp)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear_std', vid19s, 'clear sky surface temperature standard deviation', &
       & 'surface_temperature', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear_prop_unc', vid19wm, 'clear sky surface temperature propagated uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear_corr_unc', vid19ws, 'clear sky surface temperature correlated uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear_unc', vid19e, 'clear sky surface temperature uncertainty', &
       & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'stemp_clear_unc_std', vid19es, 'clear sky surface temperature uncertainty standard deviation', &
          & 'surface_temperature standard error', 'K', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  
  ! cth
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=-1.
  vmax=20.
  comment_cth = 'CTH retrieved at approx. 1 optical depth below cloud top'

  CALL nc_defdata_float(ncid_out, dims_var, 'cth', vid2m, 'cloud top height', &
       & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_cth)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_std', vid2s, 'cloud top height standard deviation', &
       & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_prop_unc', vid2wm, 'cloud top height propagated uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corr_unc', vid2ws, 'cloud top height correlated uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_unc', vid2e, 'cloud top height uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cth_unc_sq', vid2esq, 'cloud top height uncertainty sq', &
  !        & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cth_unc_std', vid2es, 'cloud top height uncertainty standard deviation', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cth_med', vid2med, 'cloud top height median', &
  !        & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! cth_corrected

  scale=1.00
  offset=0.00
  vmin=-1.
  vmax=50.
  comment_cth_corrected = 'as CTH, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected', vid15m, 'cloud top height', &
       & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_cth_corrected)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_std', vid15s, 'cloud top height standard deviation', &
       & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_prop_unc', vid15wm, 'cloud top height propagated uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_corr_unc', vid15ws, 'cloud top height correlated uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_unc', vid15e, 'cloud top height uncertainty', &
       & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_unc_sq', vid15esq, 'cloud top height uncertainty sq', &
  !        & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_unc_std', vid15es, 'cloud top height uncertainty standard deviation', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cth_corrected_med', vid15med, 'cloud top height median', &
  !        & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  if (trim(adjustl(calgo)) .eq. 'FAME-C') then

     ! cth2 FAME-C
     ! sstapelb atributes added 19.11.2012

     scale=1.00
     offset=0.00
     vmin=-1.
     vmax=20.

     CALL nc_defdata_float(ncid_out, dims_var, 'cth2', vid2bm, 'cloud top height', &
          & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_std', vid2bs, 'cloud top height standard deviation', &
          & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_prop_unc', vid2bwm, 'cloud top height propagated uncertainty', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_corr_unc', vid2bws, 'cloud top height correlated uncertainty', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_unc', vid2be, 'cloud top height uncertainty', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_unc_sq', vid2besq, 'cloud top height uncertainty sq', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_unc_std', vid2bes, 'cloud top height uncertainty standard deviation', &
          & 'cloud_top_altitude standard error', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'cth2_med', vid2bmed, 'cloud top height median', &
          & 'cloud_top_altitude', 'km', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  endif

  ! ctp
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=50.
  vmax=1200.
  comment_ctp = 'CTP retrieved at approx. 1 optical depth below cloud top'
  comment_ctp_log = 'as CTP, but values were log-transformed while averaging'

  CALL nc_defdata_float(ncid_out, dims_var, 'ctp', vid3m, 'cloud top pressure', &
       & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_ctp)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_std', vid3s, 'cloud top pressure standard deviation', &
       & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_prop_unc', vid3wm, 'cloud top pressure propagated uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corr_unc', vid3ws, 'cloud top pressure correlated uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_unc', vid3e, 'cloud top pressure uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctp_unc_sq', vid3esq, 'cloud top pressure uncertainty sq', &
  !        & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'ctp_unc_std', vid3es, 'cloud top pressure uncertainty standard deviation', &
          & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctp_med', vid3med, 'cloud top pressure median', &
  !        & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_log', vid3log, 'cloud top pressure logarithmically averaged', &
       & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_ctp_log)

  ! ctp_corrected
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=50.
  vmax=1200.
  comment_ctp_corrected = 'as CTP, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected', vid18m, 'cloud top pressure', &
       & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_ctp_corrected)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_std', vid18s, 'cloud top pressure standard deviation', &
       & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_prop_unc', vid18wm, 'cloud top pressure propagated uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_corr_unc', vid18ws, 'cloud top pressure correlated uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_unc', vid18e, 'cloud top pressure uncertainty', &
       & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_unc_sq', vid18esq, 'cloud top pressure uncertainty sq', &
  !        & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_unc_std', vid18es, 'cloud top pressure uncertainty standard deviation', &
          & 'air_pressure_at_cloud_top standard error', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,0.,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_med', vid18med, 'cloud top pressure median', &
  !        & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'ctp_corrected_log', vid18log, 'cloud top pressure logarithmic', &
  !        & 'air_pressure_at_cloud_top', 'hPa', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')


  ! cph
  
  ! what about cloud phase. does it make sense to average? vid4m
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=1.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cph', vid4m, 'fraction of liquid water clouds', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top', '1', real(real_fill_value,kind=sreal), &
       & verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_std', vid4s, 'fraction of liquid water clouds standard deviation', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top',&
       & '1', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_prop_unc', vid4wm, 'fraction of liquid water clouds propagated uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_corr_unc', vid4ws, 'fraction of liquid water clouds correlated uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_unc', vid4e, 'fraction of liquid water clouds uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cph_unc_std', vid4es, 'fraction of liquid water clouds uncertainty standard deviation', &
          & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_std_unc_unc)
  endif  
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_day', vid4md, 'daytime fraction of liquid water clouds', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top', '1', real(real_fill_value,kind=sreal), &
       & verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_day_std', vid4sd, 'daytime fraction of liquid water clouds standard deviation', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top',&
       & '1', real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_day_prop_unc', vid4wmd, 'fraction of liquid water clouds propagated uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_day_corr_unc', vid4wsd, 'fraction of liquid water clouds correlated uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cph_day_unc', vid4ed, 'fraction of liquid water clouds uncertainty', &
       & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cph_day_unc_std', vid4esd, 'fraction of liquid water clouds uncertainty standard deviation', &
          & 'area_fraction_of_liquid_cloud_water_particles_at_cloud_top standard error', '%',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax*100.,0,1,comment_std_unc_unc)
  endif


  
  ! cer
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=200.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cer', vid5m, 'cloud effective radius', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_std', vid5s, 'cloud effective radius standard deviation', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_prop_unc', vid5wm, 'cloud effective radius propagated uncertainty', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_corr_unc', vid5ws, 'cloud effective radius correlated uncertainty', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_unc', vid5e, 'cloud effective radius uncertainty', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_unc_sq', vid5esq, 'cloud effective radius uncertainty sq', &
  !        & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard error', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cer_unc_std', vid5es, 'cloud effective radius uncertainty standard deviation', &
          & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard error', 'um',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_med', vid5med, 'cloud effective radius median', &
  !        & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! cot
  ! sstapelb atributes added 19.11.2012
  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=320.00
  comment_cot_log = 'as COT, but values were log-transformed while averaging'

  CALL nc_defdata_float(ncid_out, dims_var, 'cot', vid6m, 'cloud optical thickness', &
       & 'atmosphere_optical_thickness_due_to_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_log', vid6log, 'cloud optical thickness logarithmically averaged', &
       & 'atmosphere_optical_thickness_due_to_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,1,comment_cot_log)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_std', vid6s, 'cloud optical thickness standard deviation', &
       & 'atmosphere_optical_thickness_due_to_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_prop_unc', vid6wm, 'cloud optical thickness propagated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_corr_unc', vid6ws, 'cloud optical thickness correlated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_unc', vid6e, 'cloud optical thickness uncertainty', &
       & 'atmosphere_optical_thickness_due_to_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cot_unc_sq', vid6esq, 'cloud optical thickness uncertainty sq', &
  !        & 'atmosphere_optical_thickness_due_to_cloud standard error','1', &
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cot_unc_std', vid6es, 'cloud optical thickness uncertainty standard deviation', &
          & 'atmosphere_optical_thickness_due_to_cloud standard error','1', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cot_med', vid6med, 'cloud optical thickness median', &
  !        & 'atmosphere_optical_thickness_due_to_cloud','1', &
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')


  ! toa
  ! sstapelb atributes added 19.11.2012
  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=1500.00
  comment_cot_log = ''

  if (proc_toa) then
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swup', vid_toa_swup, 'top of atmosphere upwelling solar radiation', &
          & 'toa_outgoing_shortwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swdn', vid_toa_swdn, 'top of atmosphere downwelling solar radiation', &
          & 'toa_downwelling_shortwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swup_clr', vid_toa_swup_clr, 'top of atmosphere upwelling solar radiation', &
          & 'toa_outgoing_shortwave_flux_assuming_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_lwup', vid_toa_lwup, 'top of atmosphere upwelling thermal radiation', &
          & 'toa_outgoing_longwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_lwup_clr', vid_toa_lwup_clr, 'top of atmosphere upwelling thermal radiation', &
          & 'toa_outgoing_longwave_flux_assuming_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')

     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swup_low', vid_toa_swup_low, 'top of atmosphere upwelling solar radiation low clouds', &
          & 'toa_outgoing_shortwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swup_mid', vid_toa_swup_mid, 'top of atmosphere upwelling solar radiation mid-level clouds', &
          & 'toa_outgoing_shortwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_swup_hig', vid_toa_swup_hig, 'top of atmosphere upwelling solar radiation high clouds', &
          & 'toa_outgoing_shortwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_lwup_low', vid_toa_lwup_low, 'top of atmosphere upwelling thermal radiation low clouds', &
          & 'toa_outgoing_longwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_lwup_mid', vid_toa_lwup_mid, 'top of atmosphere upwelling thermal radiation mid-level clouds', &
          & 'toa_outgoing_longwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'toa_lwup_hig', vid_toa_lwup_hig, 'top of atmosphere upwelling thermal radiation high clouds', &
          & 'toa_outgoing_longwave_flux','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')


     !boa
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_swup', vid_boa_swup, 'bottom of atmosphere upwelling solar radiation', &
          & 'surface_upwelling_shortwave_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_swup_clr', vid_boa_swup_clr, 'bottom of atmosphere upwelling solar radiation', &
          & 'surface_upwelling_shortwave_flux_in_air_assuming_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_lwup', vid_boa_lwup, 'bottom of atmosphere upwelling thermal radiation', &
          & 'surface_upwelling_longwave_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_lwup_clr', vid_boa_lwup_clr, 'bottom of atmosphere upwelling thermal radiation', &
          & 'surface_upwelling_longwave_flux_assuming_in_air_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_swdn', vid_boa_swdn, 'bottom of atmosphere downwelling solar radiation', &
          & 'surface_downwelling_shortwave_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_swdn_clr', vid_boa_swdn_clr, 'bottom of atmosphere downwelling solar radiation', &
          & 'surface_downwelling_shortwave_flux_in_air_assuming_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_lwdn', vid_boa_lwdn, 'bottom of atmosphere downwelling thermal radiation', &
          & 'surface_downwelling_longwave_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_lwdn_clr', vid_boa_lwdn_clr, 'bottom of atmosphere downwelling thermal radiation', &
          & 'surface_downwelling_longwave_flux_assuming_in_air_clear_sky','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_par_dif', vid_boa_par_dif, 'surface diffuse par', &
          & 'surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'boa_par_tot', vid_boa_par_tot, 'surface total par', &
          & 'surface_downwelling_photosynthetic_radiative_flux_in_air','W m-2', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  endif



  ! cwp
  ! sstapelb atributes added 19.11.2012

  !   scale=1.00
  !   offset=0.00
  !   vmin=0.0
  !   vmax=32000.0

  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp', vid7m, 'cloud water path', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_std', vid7s, 'cloud water path standard deviation', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_prop_unc', vid7wm, 'cloud water path propagated uncertainty', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_corr_unc', vid7ws, 'cloud water path correlated uncertainty', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_unc', vid7e, 'cloud water path uncertainty', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_unc_sq', vid7esq, 'cloud water path uncertainty sq', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_unc_std', vid7es, 'cloud water path uncertainty standard deviation', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cwp_med', vid7med, 'cloud water path median', &
  !        & 'atmosphere_mass_content_of_cloud_condensed_water', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! cloud effective emissivity

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=1.0

  CALL nc_defdata_float(ncid_out, dims_var, 'cee', vid_cee_m, 'cloud effective emissivity at 10.8 um', &
       & 'cloud_effective_emissivity', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cee_std', vid_cee_s, 'cloud effective emissivity at 10.8 um standard deviation', &
       & 'cloud_effective_emissivity', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cee_prop_unc', vid_cee_wm, 'cloud effective emissivity at 10.8 um propagated uncertainty', &
       & 'cloud_effective_emissivity standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cee_corr_unc', vid_cee_ws, 'cloud effective emissivity at 10.8 um correlated uncertainty', &
       & 'cloud_effective_emissivity standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cee_unc', vid_cee_e, 'cloud effective emissivity at 10.8 um uncertainty', &
       & 'cloud_effective_emissivity standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cee_unc_std', vid_cee_es, 'cloud effective emissivity at 10.8 um uncertainty standard deviation', &
          & 'cloud_effective_emissivity standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  ! cloud_albedo1

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=1.0

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006', vid13m, 'cloud albedo at 0.6 um', &
       & 'cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_std', vid13s, 'cloud albedo at 0.6 um standard deviation', &
       & 'cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_prop_unc', vid13wm,'cloud albedo at 0.6 um propagated uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_corr_unc', vid13ws, 'cloud albedo at 0.6 um correlated uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_unc', vid13e, 'cloud albedo at 0.6 um uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_unc_std', vid13es,'cloud albedo at 0.6 um uncertainty standard deviation', &
          & 'cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_liq', vid13lm, 'liquid water cloud albedo at 0.6 um', &
       & 'liquid_water_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_liq_std', vid13ls,'liquid water cloud albedo at 0.6 um standard deviation', &
       & 'liquid_water_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_liq_unc', vid13le,'liquid water cloud albedo at 0.6 um uncertainty', &
       & 'liquid_water_cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_liq_unc_std',vid13les, 'liquid water cloud albedo at 0.6 um uncertainty standard deviation', &
          & 'liquid_water_cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_ice', vid13im, 'ice water cloud albedo at 0.6 um', &
       & 'ice_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_ice_std', vid13is,'ice water cloud albedo at 0.6 um standard deviation', &
       & 'ice_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis006_ice_unc', vid13ie, 'ice water cloud albedo at 0.6 um uncertainty', &
       & 'ice_cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var,'cla_vis006_ice_unc_std',vid13ies, 'ice water cloud albedo at 0.6 um uncertainty standard deviation', &
          & 'ice_cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  ! cloud_albedo2


  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=1.0

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008', vid14m, 'cloud albedo at 0.8 um', &
       & 'cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_std', vid14s, 'cloud albedo at 0.8 um standard deviation', &
       & 'cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_prop_unc', vid14wm,'cloud albedo at 0.8 um propagated uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_corr_unc', vid14ws,'cloud albedo at 0.8 um correlated uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)

  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_unc', vid14e, 'cloud albedo at 0.8 um uncertainty', &
       & 'cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_unc_std', vid14es,'cloud albedo at 0.8 um uncertainty standard deviation', &
          & 'cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_liq', vid14lm, 'liquid water cloud albedo at 0.8 um', &
       & 'liquid_water_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_liq_std', vid14ls,'liquid water cloud albedo at 0.8 um standard deviation', &
       & 'liquid_water_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_liq_unc', vid14le,'liquid water cloud albedo at 0.8 um uncertainty', &
       & 'liquid_water_cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_liq_unc_std',vid14les, 'liquid water cloud albedo at 0.8 um uncertainty standard deviation', &
          & 'liquid_water_cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  endif
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_ice', vid14im, 'ice water cloud albedo at 0.8 um', &
       & 'ice_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_ice_std', vid14is,'ice water cloud albedo at 0.8 um standard deviation', &
       & 'ice_cloud_albedo', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cla_vis008_ice_unc', vid14ie,'ice water cloud albedo at 0.8 um uncertainty', &
       & 'ice_cloud_albedo standard error', '1',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var,'cla_vis008_ice_unc_std',vid14ies,'ice water cloud albedo at 0.8 um uncertainty standard deviation', &
          & 'ice_cloud_albedo standard error', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  !LWP

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=32000.0

  CALL nc_defdata_float(ncid_out, dims_var, 'lwp', vid7lm, 'cloud liquid water path', &
       & 'atmosphere_mass_content_of_cloud_liquid_water', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'lwp_std', vid7ls, 'cloud liquid water path standard deviation', &
       & 'atmosphere_mass_content_of_cloud_liquid_water', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'lwp_prop_unc', vid7lwm, 'cloud liquid water path propagated uncertainty', &
       & 'atmosphere_mass_content_of_cloud_liquid_water standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'lwp_corr_unc', vid7lws, 'cloud liquid water path correlated uncertainty', &
       & 'atmosphere_mass_content_of_cloud_liquid_water standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'lwp_unc', vid7le, 'cloud liquid water path uncertainty', &
       & 'atmosphere_mass_content_of_cloud_liquid_water standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'lwp_allsky', vid7lam, 'grid box mean of cloud liquid water path', &
       & 'atmosphere_mass_content_of_cloud_liquid_water', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'lwp_unc_sq', vid7lesq, 'cloud liquid water path uncertainty sq', &
  !        & 'atmosphere_mass_content_of_cloud_liquid_water standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'lwp_unc_std', vid7les, 'cloud liquid water path uncertainty standard deviation', &
          & 'atmosphere_mass_content_of_cloud_liquid_water standard error', 'g/m2',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  !IWP

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=32000.0

  CALL nc_defdata_float(ncid_out, dims_var, 'iwp', vid7im, 'cloud ice water path', &
       & 'atmosphere_mass_content_of_cloud_ice', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'iwp_std', vid7is, 'cloud ice water path standard deviation', &
       & 'atmosphere_mass_content_of_cloud_ice', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'iwp_prop_unc', vid7iwm, 'cloud ice water path propagated uncertainty', &
       & 'atmosphere_mass_content_of_cloud_ice standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'iwp_corr_unc', vid7iws, 'cloud ice water path correlated uncertainty', &
       & 'atmosphere_mass_content_of_cloud_ice standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'iwp_unc', vid7ie, 'cloud ice water path uncertainty', &
       & 'atmosphere_mass_content_of_cloud_ice standard error', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'iwp_allsky', vid7iam, 'grid box mean of cloud ice water path ', &
       & 'atmosphere_mass_content_of_cloud_ice', 'g/m2',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,1,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'iwp_unc_sq', vid7iesq, 'cloud ice water path uncertainty sq', &
  !        & 'atmosphere_mass_content_of_cloud_ice standard error', 'g/m2',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'iwp_unc_std', vid7ies, 'cloud ice water path uncertainty standard deviation', &
          & 'atmosphere_mass_content_of_cloud_ice standard error', 'g/m2',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  ! cer liquid
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=200.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq', vid9m, 'liquid water cloud effective radius', &
       & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq_std', vid9s, 'liquid water cloud effective radius standard deviation', &
       & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq_prop_unc', vid9wm, 'liquid water cloud effective radius propagated uncertainty', &
       & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq_corr_unc', vid9ws, 'liquid water cloud effective radius correlated uncertainty', &
       & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq_unc', vid9e, 'liquid water cloud effective radius uncertainty', &
       & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_unc_liq_sq', vid9esq, 'cloud liquid water effective radius uncertainty sq', &
  !        & 'effective_radius_of_cloud_liquid_water_particles_at_cloud_top standard error', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cer_liq_unc_std', vid9es, 'liquid water cloud effective radius uncertainty standard deviation', &
          & 'effective_radius_of_cloud_liquid_water_particle_at_liquid_water_cloud_top standard error', 'um',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_med_liq', vid9med, 'cloud liquid water effective radius median', &
  !        & 'effective_radius_of_cloud_liquid_water_particles_at_cloud_top', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! cer ice
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=200.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice', vid10m, 'ice water cloud effective radius', &
       & 'effective_radius_of_cloud_ice_crystals_at_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice_std', vid10s, 'ice water cloud effective radius standard deviation', &
       & 'effective_radius_of_cloud_ice_crystals_at_cloud_top', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice_prop_unc', vid10wm, 'ice water cloud effective radius propagated uncertainty', &
       & 'effective_radius_of_cloud_ice_crystals_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice_corr_unc', vid10ws, 'ice water cloud effective radius correlated uncertainty', &
       & 'effective_radius_of_cloud_ice_crystals_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice_unc', vid10e, 'ice water cloud effective radius uncertainty', &
       & 'effective_radius_of_cloud_ice_crystals_at_cloud_top standard error', 'um',&
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_unc_ice_sq', vid10esq, 'ice effective radius uncertainty sq', &
  !        & 'effective_radius_of_cloud_ice_crystals_at_cloud_top standard error', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,0,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cer_ice_unc_std', vid10es, 'ice water cloud effective radius uncertainty standard deviation', &
          & 'effective_radius_of_cloud_ice_crystals_at_cloud_top standard error', 'um',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cer_med_ice', vid10med, 'ice effective radius median', &
  !        & 'effective_radius_of_cloud_ice_crystals_at_cloud_top', 'um',&
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  ! cot liq
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=320.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq', vid11m, 'liquid water cloud optical thickness', &
       & 'atmosphere_optical_thickness_due_to_liquid_water_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq_std', vid11s, 'liquid water cloud optical thickness standard deviation', &
       & 'atmosphere_optical_thickness_due_to_liquid_water_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq_prop_unc', vid11wm, 'liquid water cloud optical thickness propagated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_liquid_water_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq_corr_unc', vid11ws, 'liquid water cloud optical thickness correlated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_liquid_water_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq_unc', vid11e, 'liquid water cloud optical thickness uncertainty', &
       & 'atmosphere_optical_thickness_due_to_liquid_water_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_mean_unc)
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cot_liq_unc_std', vid11es, 'liquid water cloud optical thickness uncertainty standard deviation', &
          & 'atmosphere_optical_thickness_due_to_liquid_water_cloud standard error','1', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif

  ! cot ice
  ! sstapelb atributes added 19.11.2012

  scale=1.00
  offset=0.00
  vmin=0.00
  vmax=320.00

  CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice', vid12m, 'ice water cloud optical thickness', &
       & 'atmosphere_optical_thickness_due_to_ice_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cot_log_ice', vid12log, 'ice water cloud optical thickness logarithmic', &
  !        & 'atmosphere_optical_thickness_due_to_ice_cloud','1', &
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice_std', vid12s, 'ice water cloud optical thickness standard deviation', &
       & 'atmosphere_optical_thickness_due_to_ice_cloud','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice_prop_unc', vid12wm, 'ice water cloud optical thickness propagated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_ice_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_prop_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice_corr_unc', vid12ws, 'ice water cloud optical thickness correlated uncertainty', &
       & 'atmosphere_optical_thickness_due_to_ice_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_corr_unc)
  CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice_unc', vid12e, 'ice water cloud optical thickness uncertainty', &
       & 'atmosphere_optical_thickness_due_to_ice_cloud standard error','1', &
       & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,10,1,comment_mean_unc)
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cot_unc_ice_sq', vid12esq, 'ice water cloud optical thickness uncertainty sq', &
  !        & 'atmosphere_optical_thickness_due_to_ice_cloud standard error','1', &
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax_sq,10,0,'')
  if (write_l2b_sum_variables) then
     CALL nc_defdata_float(ncid_out, dims_var, 'cot_ice_unc_std', vid12es, 'ice water cloud optical thickness uncertainty standard deviation', &
          & 'atmosphere_optical_thickness_due_to_ice_cloud standard error','1', &
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,1,comment_std_unc_unc)
  endif
  !   CALL nc_defdata_float(ncid_out, dims_var, 'cot_med_ice', vid12med, 'ice water cloud optical thickness median', &
  !        & 'atmosphere_optical_thickness_due_to_ice_cloud','1', &
  !        & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  !cot_ctp_water_histogram

  CALL nc_defdata_hist2d(ncid_out, dims_var_hist2d, 'hist2d_cot_ctp', vid1h, 'joint histogram of cloud optical thickness and cloud top pressure', &
       & 'atmosphere_optical_thickness_due_to_cloud_and_air_pressure_at_cloud_top_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  ! 1D histograms

  CALL nc_defdata_hist1d(ncid_out, dims_var_histcot1d, 'hist1d_cot', vid1h_cot, 'histogram of cloud optical thickness', &
       & 'atmosphere_optical_thickness_due_to_cloud_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histctp1d, 'hist1d_ctp', vid1h_ctp, 'histogram of cloud top pressure', &
       & 'air_pressure_at_cloud_top_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histctt1d, 'hist1d_ctt', vid1h_ctt, 'histogram of cloud top temperature', &
       & 'temperature_at_cloud_top_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histref1d, 'hist1d_cer', vid1h_ref, 'histogram of cloud effective radius', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histcwp1d, 'hist1d_cwp', vid1h_cwp, 'histogram of cloud water path', &
       & 'atmosphere_mass_content_of_cloud_condensed_water_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histcloud_albedo11d, 'hist1d_cla_vis006', vid1h_cloud_albedo1, 'histogram of cloud albedo at 0.6 um', &
       & 'cloud_albedo_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  CALL nc_defdata_hist1d(ncid_out, dims_var_histcloud_albedo21d, 'hist1d_cla_vis008', vid1h_cloud_albedo2, 'histogram of cloud albedo at 0.8 um', &
       & 'cloud_albedo_histogram', '1',&
       & long_int_fill_value,verbose,0,0,'')

  ! stapel new 1D histos

  if(lclosure) then

     ! sstapelb atributes added 19.11.2012

     scale=1.00
     offset=0.00
     vmin=0
     vmax=100

     CALL nc_defdata_float(ncid_out, dims_var, 'rad', vid1rad, 'image1', &
          & 'radiance_reflectance', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'tau_consistent', vid1taucon, 'tau_consistent', &
          & 'tau_consistent', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'tau_compare', vid1taucompare, 'tau_compare', &
          & 'tau_compare', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'ppab', vid1ppab, 'ppab', &
          & 'ppab', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'red_factor', vid1redfac, 'red_factor', &
          & 'red_factor', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')
     CALL nc_defdata_float(ncid_out, dims_var, 'var_para', vid1varpara, 'var_para', &
          & 'var_para', '1',&
          & real(real_fill_value,kind=sreal), verbose,scale,offset,vmin,vmax,0,0,'')

  endif

  if (verbose .eq. 1) write(*,*) "================== writing output data ===================="

  if (verbose .eq. 1) write(*,*) "writing counters"

  CALL nc_write_L3_lint(ncid_out,'nobs',vid1pmac, &
       & l3_macro%points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_day',vid1pmic, &
       & l3_micro%points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_day_liq',vid1pmic_cloudy_liq, &
       & l3_micro%cloudy_points_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_day_ice',vid1pmic_cloudy_ice, &
       & l3_micro%cloudy_points_ice(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'npoints_macro_cct_cloudy',vid1pmac_cct_cloudy, &
  !        & l3_macro%cct_points(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'nobs_clear',vid1pmac_cct_clear,   &
  !        & l3_macro%cct_points_clear(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_low',vid1pmac_cct_low_cloudy, &
       & l3_macro%cct_low_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_mid',vid1pmac_cct_middle_cloudy, &
       & l3_macro%cct_middle_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_high',vid1pmac_cct_high_cloudy, &
       & l3_macro%cct_high_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_cloudy',vid1pmac_cct_cloudy_raw, &
       & l3_macro%cct_points_raw(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'npoints_macro_cct_clear_raw',vid1pmac_cct_clear_raw,   &
  !        & l3_macro%cct_points_clear_raw(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'npoints_micro_cct_cloudy',vid1pmic_cct_cloudy, &
  !        & l3_micro%cct_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_clear_day',vid1pmic_cct_clear,   &
       & l3_micro%cct_points_clear(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_cloudy_day',vid1pmac_cct_day_cloudy, &
       & l3_macro%cct_points_day(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'npoints_macro_cct_day_clear',vid1pmac_cct_day_clear, &
  !        & l3_macro%cct_points_clear_day(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_cloudy_night',vid1pmac_cct_night_cloudy, &
       & l3_macro%cct_points_night(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_clear_night',vid1pmac_cct_night_clear, &
       & l3_macro%cct_points_clear_night(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_cloudy_twl',vid1pmac_cct_twl_cloudy, &
       & l3_macro%cct_points_twl(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nobs_clear_twl',vid1pmac_cct_twl_clear, &
       & l3_macro%cct_points_clear_twl(:,:),1,ncols,1,nrows, verbose)
  !  CALL nc_write_L3_lint(ncid_out,'npoints_macro_clear',vid1pmac_clear,l3_macro%clear_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy',vid1pmac_cloudy,l3_macro%cloudy_points(:,:),1,ncols,1,nrows, verbose)
  ! CALL nc_write_L3_lint(ncid_out,'npoints_micro_clear',vid1pmic_clear,l3_micro%clear_points(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_day',vid1pmic_cloudy,l3_micro%cloudy_points(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_liq',vid1pmac_cph_liq,l3_macro%cloudy_points_cph_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_cloudy_ice',vid1pmac_cph_ice,l3_macro%cloudy_points_cph_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'nretr_clear',vid1pmac_stemp_clear,l3_macro%stemp_clear_points(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_lint(ncid_out,'nretr_cee',vid1pmac_cee,l3_macro%cee_points(:,:),1,ncols,1,nrows, verbose)

  if (write_l2b_sum_variables) then
    CALL nc_write_L3_lint(ncid_out,'nretr_sw',vid1pmac_sw,l3_macro%rad_points_sw(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_lw',vid1pmac_lw,l3_macro%rad_points_lw(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_sw_low',vid1pmac_sw_low,l3_macro%rad_points_sw_low(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_lw_low',vid1pmac_lw_low,l3_macro%rad_points_lw_low(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_sw_mid',vid1pmac_sw_mid,l3_macro%rad_points_sw_mid(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_lw_mid',vid1pmac_lw_mid,l3_macro%rad_points_lw_mid(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_sw_hig',vid1pmac_sw_hig,l3_macro%rad_points_sw_hig(:,:),1,ncols,1,nrows, verbose)
    CALL nc_write_L3_lint(ncid_out,'nretr_lw_hig',vid1pmac_lw_hig,l3_macro%rad_points_lw_hig(:,:),1,ncols,1,nrows, verbose)
  endif

  if (verbose .eq. 1) write(*,*) "writing ctt"

  CALL nc_write_L3_float(ncid_out,'ctt',vid1m,l3_macro%ctt_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_std',vid1s,l3_macro%ctt_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_prop_unc',vid1wm,l3_macro%ctt_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corr_unc',vid1ws,l3_macro%ctt_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_unc',vid1e,l3_macro%ctt_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctt_unc_sq',vid1esq,l3_macro%ctt_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'ctt_unc_std',vid1es,l3_macro%ctt_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctt_med',vid1med,l3_macro%ctt_median(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing ctt_corrected"

  CALL nc_write_L3_float(ncid_out,'ctt_corrected',vid17m,l3_macro%ctt_corrected_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_std',vid17s,l3_macro%ctt_corrected_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_prop_unc',vid17wm,l3_macro%ctt_corrected_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_corr_unc',vid17ws,l3_macro%ctt_corrected_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_unc',vid17e,l3_macro%ctt_corrected_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctt_corrected_unc_sq',vid17esq,l3_macro%ctt_corrected_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'ctt_corrected_unc_std',vid17es,l3_macro%ctt_corrected_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctt_corrected_med',vid17med,l3_macro%ctt_corrected_median(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing stemp"

  CALL nc_write_L3_float(ncid_out,'stemp_cloudy',vid16m,l3_macro%stemp_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_cloudy_std',vid16s,l3_macro%stemp_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_cloudy_prop_unc',vid16wm,l3_macro%stemp_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_cloudy_corr_unc',vid16ws,l3_macro%stemp_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_cloudy_unc',vid16e,l3_macro%stemp_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'stemp_cloudy_unc_sq',vid16esq,l3_macro%stemp_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'stemp_cloudy_unc_std',vid16es,l3_macro%stemp_std_error(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'stemp_clear',vid19m,l3_macro%stemp_clear_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_clear_std',vid19s,l3_macro%stemp_clear_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_clear_prop_unc',vid19wm,l3_macro%stemp_clear_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_clear_corr_unc',vid19ws,l3_macro%stemp_clear_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_clear_unc',vid19e,l3_macro%stemp_clear_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'stemp_clear_unc_std',vid19es,l3_macro%stemp_clear_std_error(:,:),1,ncols,1,nrows, verbose)
  
  if (verbose .eq. 1) write(*,*) "writing cth"

  CALL nc_write_L3_float(ncid_out,'cth',vid2m,l3_macro%cth_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_std',vid2s,l3_macro%cth_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_prop_unc',vid2wm,l3_macro%cth_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corr_unc',vid2ws,l3_macro%cth_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_unc',vid2e,l3_macro%cth_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cth_unc_sq',vid2esq,l3_macro%cth_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables)  CALL nc_write_L3_float(ncid_out,'cth_unc_std',vid2es,l3_macro%cth_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cth_med',vid2med,l3_macro%cth_median(:,:),1,ncols,1,nrows, verbose)




  if (verbose .eq. 1) write(*,*) "writing cth_corrected"

  CALL nc_write_L3_float(ncid_out,'cth_corrected',vid15m,l3_macro%cth_corrected_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_std',vid15s,l3_macro%cth_corrected_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_prop_unc',vid15wm,l3_macro%cth_corrected_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_corr_unc',vid15ws,l3_macro%cth_corrected_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_unc',vid15e,l3_macro%cth_corrected_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cth_corrected_unc_sq',vid15esq,l3_macro%cth_corrected_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cth_corrected_unc_std',vid15es,l3_macro%cth_corrected_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cth_corrected_med',vid15med,l3_macro%cth_corrected_median(:,:),1,ncols,1,nrows, verbose)




  if (trim(adjustl(calgo)) .eq. 'FAME-C') then

     if (verbose .eq. 1) write(*,*) "cth2"

     CALL nc_write_L3_float(ncid_out,'cth2',vid2bm,l3_macro%cth2_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_std',vid2bs,l3_macro%cth2_std(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_prop_unc',vid2bwm,l3_macro%cth2_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_corr_unc',vid2bws,l3_macro%cth2_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_unc',vid2be,l3_macro%cth2_mean_error(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_unc_sq',vid2besq,l3_macro%cth2_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_unc_std',vid2bes,l3_macro%cth2_std_error(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'cth2_med',vid2bmed,l3_macro%cth2_median(:,:),1,ncols,1,nrows, verbose)

  endif

  if (verbose .eq. 1) write(*,*) "writing ctp"

  CALL nc_write_L3_float(ncid_out,'ctp',vid3m,l3_macro%ctp_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_std',vid3s,l3_macro%ctp_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_prop_unc',vid3wm,l3_macro%ctp_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corr_unc',vid3ws,l3_macro%ctp_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_unc',vid3e,l3_macro%ctp_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctp_unc_sq',vid3esq,l3_macro%ctp_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'ctp_unc_std',vid3es,l3_macro%ctp_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctp_med',vid3med,l3_macro%ctp_median(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_log',vid3log,l3_macro%ctp_log_mean(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing ctp_corrected"

  CALL nc_write_L3_float(ncid_out,'ctp_corrected',vid18m,l3_macro%ctp_corrected_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_std',vid18s,l3_macro%ctp_corrected_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_prop_unc',vid18wm,l3_macro%ctp_corrected_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_corr_unc',vid18ws,l3_macro%ctp_corrected_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_unc',vid18e,l3_macro%ctp_corrected_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctp_corrected_unc_sq',vid18esq,l3_macro%ctp_corrected_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'ctp_corrected_unc_std',vid18es,l3_macro%ctp_corrected_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'ctp_corrected_med',vid18med,l3_macro%ctp_corrected_median(:,:),1,ncols,1,nrows, verbose)

  !cloud cover day and night

  if (verbose .eq. 1) write(*,*) "writing cfc"

  CALL nc_write_L3_float(ncid_out,'cfc',vid8m,l3_macro%cct_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_day',vid8mday,l3_macro%cct_day(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_night',vid8mnight,l3_macro%cct_night(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_twl',vid8mtwl,l3_macro%cct_twl(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_std',vid8s,l3_macro%cct_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_prop_unc',vid8wm,l3_macro%cct_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_corr_unc',vid8ws,l3_macro%cct_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cfc_micro',vid8mmicro,l3_micro%cct_mean(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cfc_micro_std',vid8smicro,l3_micro%cct_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_unc',vid8e,l3_macro%cct_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cfc_unc_std',vid8es,l3_macro%cct_std_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_low',vid8low,l3_macro%cct_mean_low(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_mid',vid8middle,l3_macro%cct_mean_middle(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cfc_high',vid8high,l3_macro%cct_mean_high(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cc_rawt',vid8m_raw,l3_macro%cct_raw(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cc_post_rawt',vid8m_post_raw,l3_macro%cct_post_raw(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cph"

  CALL nc_write_L3_float(ncid_out,'cph',vid4m,l3_macro%cty_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_std',vid4s,l3_macro%cty_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_prop_unc',vid4wm,l3_macro%cty_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_corr_unc',vid4ws,l3_macro%cty_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_unc',vid4e,l3_macro%cty_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cph_unc_std',vid4es,l3_macro%cty_std_error(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cph_day"

  CALL nc_write_L3_float(ncid_out,'cph_day',vid4md,l3_micro%cty_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_day_std',vid4sd,l3_micro%cty_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_day_prop_unc',vid4wmd,l3_micro%cty_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_day_corr_unc',vid4wsd,l3_micro%cty_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_day_unc',vid4ed,l3_micro%cty_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cph_day_unc_std',vid4esd,l3_micro%cty_std_error(:,:),1,ncols,1,nrows, verbose)


  if (verbose .eq. 1) write(*,*) "writing cer"
  CALL nc_write_L3_float(ncid_out,'cer',vid5m,l3_micro%ref_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_std',vid5s,l3_micro%ref_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_prop_unc',vid5wm,l3_micro%ref_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_corr_unc',vid5ws,l3_micro%ref_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_unc',vid5e,l3_micro%ref_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cer_unc_sq',vid5esq,l3_micro%ref_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cer_unc_std',vid5es,l3_micro%ref_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cer_med',vid5med,l3_micro%ref_median(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cee"
  CALL nc_write_L3_float(ncid_out,'cee',vid_cee_m,l3_macro%cee_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_std',vid_cee_s,l3_macro%cee_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_prop_unc',vid_cee_wm,l3_macro%cee_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_corr_unc',vid_cee_ws,l3_macro%cee_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_unc',vid_cee_e,l3_macro%cee_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cee_unc_std',vid_cee_es,l3_macro%cee_std_error(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cot"
  CALL nc_write_L3_float(ncid_out,'cot',vid6m,l3_micro%cot_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_std',vid6s,l3_micro%cot_std(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_prop_unc',vid6wm,l3_micro%cot_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_corr_unc',vid6ws,l3_micro%cot_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_log',vid6log,l3_micro%cot_log_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_unc',vid6e,l3_micro%cot_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cot_unc_sq',vid6esq,l3_micro%cot_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cot_unc_std',vid6es,l3_micro%cot_std_error(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cot_med',vid6med,l3_micro%cot_median(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing toa"
  if (proc_toa) then
     CALL nc_write_L3_float(ncid_out,'toa_swup',vid_toa_swup,l3_macro%toa_swup_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swdn',vid_toa_swdn,l3_macro%toa_swdn_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_clr',vid_toa_swup_clr,l3_macro%toa_swup_clr_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup',vid_toa_lwup,l3_macro%toa_lwup_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_clr',vid_toa_lwup_clr,l3_macro%toa_lwup_clr_mean(:,:),1,ncols,1,nrows, verbose)

     CALL nc_write_L3_float(ncid_out,'toa_swup_low',vid_toa_swup_low,l3_macro%toa_swup_low_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_mid',vid_toa_swup_mid,l3_macro%toa_swup_mid_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_hig',vid_toa_swup_hig,l3_macro%toa_swup_hig_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_low',vid_toa_lwup_low,l3_macro%toa_lwup_low_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_mid',vid_toa_lwup_mid,l3_macro%toa_lwup_mid_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_hig',vid_toa_lwup_hig,l3_macro%toa_lwup_hig_mean(:,:),1,ncols,1,nrows, verbose)

     if (verbose .eq. 1) write(*,*) "writing boa"
     CALL nc_write_L3_float(ncid_out,'boa_swup',vid_boa_swup,l3_macro%boa_swup_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swup_clr',vid_boa_swup_clr,l3_macro%boa_swup_clr_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup',vid_boa_lwup,l3_macro%boa_lwup_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup_clr',vid_boa_lwup_clr,l3_macro%boa_lwup_clr_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn',vid_boa_swdn,l3_macro%boa_swdn_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn_clr',vid_boa_swdn_clr,l3_macro%boa_swdn_clr_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn',vid_boa_lwdn,l3_macro%boa_lwdn_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn_clr',vid_boa_lwdn_clr,l3_macro%boa_lwdn_clr_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_dif',vid_boa_par_dif,l3_macro%boa_par_dif_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_tot',vid_boa_par_tot,l3_macro%boa_par_tot_mean(:,:),1,ncols,1,nrows, verbose)
  endif


  !   if (verbose .eq. 1) write(*,*) "writing cwp"

  !   CALL nc_write_L3_float(ncid_out,'cwp',vid7m,l3_micro%cwp_mean(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cwp_std',vid7s,l3_micro%cwp_std(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cwp_prop_unc',vid7wm,l3_micro%cwp_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cwp_corr_unc',vid7ws,l3_micro%cwp_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cwp_unc',vid7e,l3_micro%cwp_mean_error(:,:),1,ncols,1,nrows, verbose)
  !   !   CALL nc_write_L3_float(ncid_out,'cwp_unc_sq',vid7esq,l3_micro%cwp_mean_error_sq(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cwp_unc_std',vid7es,l3_micro%cwp_std_error(:,:),1,ncols,1,nrows, verbose)
  !   !   CALL nc_write_L3_float(ncid_out,'cwp_med',vid7med,l3_micro%cwp_median(:,:),1,ncols,1,nrows, verbose)



  if (verbose .eq. 1) write(*,*) "writing cla_vis006"
  CALL nc_write_L3_float(ncid_out,'cla_vis006',vid13m,l3_micro%cloud_albedo1_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_std',vid13s,l3_micro%cloud_albedo1_std(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis006_prop_unc',vid13wm,l3_micro%cloud_albedo1_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_corr_unc',vid13ws,l3_micro%cloud_albedo1_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis006_unc',vid13e,l3_micro%cloud_albedo1_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis006_unc_std',vid13es,l3_micro%cloud_albedo1_std_error(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis006_liq',vid13lm,l3_micro%cloud_albedo1_mean_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_liq_std',vid13ls,l3_micro%cloud_albedo1_std_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_liq_unc',vid13le,l3_micro%cloud_albedo1_mean_error_liq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis006_liq_unc_std',vid13les,l3_micro%cloud_albedo1_std_error_liq(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis006_ice',vid13im,l3_micro%cloud_albedo1_mean_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_ice_std',vid13is,l3_micro%cloud_albedo1_std_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_ice_unc',vid13ie,l3_micro%cloud_albedo1_mean_error_ice(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis006_ice_unc_std',vid13ies,l3_micro%cloud_albedo1_std_error_ice(:,:),1,ncols,1,nrows, verbose)


  if (verbose .eq. 1) write(*,*) "writing cla_vis008"
  CALL nc_write_L3_float(ncid_out,'cla_vis008',vid14m,l3_micro%cloud_albedo2_mean(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_std',vid14s,l3_micro%cloud_albedo2_std(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis008_prop_unc',vid14wm,l3_micro%cloud_albedo2_prop_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_corr_unc',vid14ws,l3_micro%cloud_albedo2_correlated_uncertainty(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis008_unc',vid14e,l3_micro%cloud_albedo2_mean_error(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis008_unc_std',vid14es,l3_micro%cloud_albedo2_std_error(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis008_liq',vid14lm,l3_micro%cloud_albedo2_mean_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_liq_std',vid14ls,l3_micro%cloud_albedo2_std_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_liq_unc',vid14le,l3_micro%cloud_albedo2_mean_error_liq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis008_liq_unc_std',vid14les,l3_micro%cloud_albedo2_std_error_liq(:,:),1,ncols,1,nrows, verbose)

  CALL nc_write_L3_float(ncid_out,'cla_vis008_ice',vid14im,l3_micro%cloud_albedo2_mean_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_ice_std',vid14is,l3_micro%cloud_albedo2_std_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_ice_unc',vid14ie,l3_micro%cloud_albedo2_mean_error_ice(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cla_vis008_ice_unc_std',vid14ies,l3_micro%cloud_albedo2_std_error_ice(:,:),1,ncols,1,nrows, verbose)

  where ( (l3_micro%cloudy_points + l3_micro%cct_points_clear) .gt. 0 )
     l3_micro%lwp_allsky = l3_micro%cwp_mean_liq * l3_micro%cty_mean * &
          & real(l3_micro%cloudy_points,sreal)/real((l3_micro%cloudy_points+l3_micro%cct_points_clear),sreal)
  elsewhere
     l3_micro%lwp_allsky = real_fill_value
  endwhere

  where (( (l3_micro%cloudy_points + l3_micro%cct_points_clear) .gt. 0 ) .and. (l3_micro%cty_mean .eq. real_fill_value))
     l3_micro%lwp_allsky = 0.
  endwhere


  if (verbose .eq. 1) write(*,*) "writing lwp"
  CALL nc_write_L3_float(ncid_out,'lwp',vid7lm,l3_micro%cwp_mean_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'lwp_std',vid7ls,l3_micro%cwp_std_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'lwp_prop_unc',vid7lwm,l3_micro%cwp_prop_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'lwp_corr_unc',vid7lws,l3_micro%cwp_correlated_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'lwp_unc',vid7le,l3_micro%cwp_mean_error_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'lwp_allsky',vid7lam,l3_micro%lwp_allsky(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'lwp_unc_sq',vid7lesq,l3_micro%cwp_mean_error_liq_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'lwp_unc_std',vid7les,l3_micro%cwp_std_error_liq(:,:),1,ncols,1,nrows, verbose)

  where ( (l3_micro%cloudy_points + l3_micro%cct_points_clear) .gt. 0 )
     l3_micro%iwp_allsky = l3_micro%cwp_mean_ice * (1. - l3_micro%cty_mean) * &
          & real(l3_micro%cloudy_points,sreal)/real((l3_micro%cloudy_points+l3_micro%cct_points_clear),sreal)
  elsewhere
     l3_micro%iwp_allsky = real_fill_value
  endwhere

  where (( (l3_micro%cloudy_points + l3_micro%cct_points_clear) .gt. 0 ) .and. (l3_micro%cty_mean .eq. real_fill_value))
     l3_micro%iwp_allsky = 0.
  endwhere


  if (verbose .eq. 1) write(*,*) "writing iwp"
  CALL nc_write_L3_float(ncid_out,'iwp',vid7im,l3_micro%cwp_mean_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'iwp_std',vid7is,l3_micro%cwp_std_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'iwp_prop_unc',vid7iwm,l3_micro%cwp_prop_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'iwp_corr_unc',vid7iws,l3_micro%cwp_correlated_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'iwp_unc',vid7ie,l3_micro%cwp_mean_error_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'iwp_allsky',vid7iam,l3_micro%iwp_allsky(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'iwp_unc_sq',vid7iesq,l3_micro%cwp_mean_error_ice_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'iwp_unc_std',vid7ies,l3_micro%cwp_std_error_ice(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cer2"

  CALL nc_write_L3_float(ncid_out,'cer_liq',vid9m,l3_micro%ref_mean_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_liq_std',vid9s,l3_micro%ref_std_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_liq_prop_unc',vid9wm,l3_micro%ref_prop_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_liq_corr_unc',vid9ws,l3_micro%ref_correlated_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_liq_unc',vid9e,l3_micro%ref_mean_error_liq(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cer_unc_liq_sq',vid9esq,l3_micro%ref_mean_error_liq_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cer_liq_unc_std',vid9es,l3_micro%ref_std_error_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_ice',vid10m,l3_micro%ref_mean_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_ice_std',vid10s,l3_micro%ref_std_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_ice_prop_unc',vid10wm,l3_micro%ref_prop_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_ice_corr_unc',vid10ws,l3_micro%ref_correlated_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_ice_unc',vid10e,l3_micro%ref_mean_error_ice(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cer_unc_ice_sq',vid10esq,l3_micro%ref_mean_error_ice_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cer_ice_unc_std',vid10es,l3_micro%ref_std_error_ice(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing cot"

  CALL nc_write_L3_float(ncid_out,'cot_liq',vid11m,l3_micro%cot_mean_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_liq_std',vid11s,l3_micro%cot_std_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_liq_prop_unc',vid11wm,l3_micro%cot_prop_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_liq_corr_unc',vid11ws,l3_micro%cot_correlated_uncertainty_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_liq_unc',vid11e,l3_micro%cot_mean_error_liq(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cot_unc_liq_sq',vid11esq,l3_micro%cot_mean_error_liq_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cot_liq_unc_std',vid11es,l3_micro%cot_std_error_liq(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_ice',vid12m,l3_micro%cot_mean_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_ice_std',vid12s,l3_micro%cot_std_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_ice_prop_unc',vid12wm,l3_micro%cot_prop_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_ice_corr_unc',vid12ws,l3_micro%cot_correlated_uncertainty_ice(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_ice_unc',vid12e,l3_micro%cot_mean_error_ice(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_float(ncid_out,'cot_unc_ice_sq',vid12esq,l3_micro%cot_mean_error_ice_sq(:,:),1,ncols,1,nrows, verbose)
  if (write_l2b_sum_variables) CALL nc_write_L3_float(ncid_out,'cot_ice_unc_std',vid12es,l3_micro%cot_std_error_ice(:,:),1,ncols,1,nrows, verbose)

  if (verbose .eq. 1) write(*,*) "writing 2D and 1D histograms"

  CALL nc_write_L3_hist2d(ncid_out,'hist2d_cot_ctp',vid1h,l3_micro%histogram_cot_ctp_phase,&
       & 1,ncols,1,nrows, verbose)
  ! stapel new 1D histos
  CALL nc_write_L3_hist1d_ctp(ncid_out,'hist1d_ctp',vid1h_ctp,l3_macro%histogram_ctp_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_ctt(ncid_out,'hist1d_ctt',vid1h_ctt,l3_macro%histogram_ctt_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_cot(ncid_out,'hist1d_cot',vid1h_cot,l3_micro%histogram_cot_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_cwp(ncid_out,'hist1d_cwp',vid1h_cwp,l3_micro%histogram_cwp_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_cloud_albedo(ncid_out,'hist1d_cla_vis006',vid1h_cloud_albedo1,l3_micro%histogram_cloud_albedo1_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_cloud_albedo(ncid_out,'hist1d_cla_vis008',vid1h_cloud_albedo2,l3_micro%histogram_cloud_albedo2_phase,1,ncols,1,nrows, verbose)
  CALL nc_write_L3_hist1d_ref(ncid_out,'hist1d_cer',vid1h_ref,l3_micro%histogram_ref_phase,1,ncols,1,nrows, verbose)
  ! stapel

  if(lclosure) then

     if (verbose .eq. 1) write(*,*) "writing closure terms"

     CALL nc_write_L3_float(ncid_out,'rad',vid1rad,l3_rad%rad_mean(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'tau_consistent', vid1taucon,l3_rad%tau_consistent,1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'tau_compare', vid1taucompare,l3_rad%tau_compare,1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'ppab', vid1ppab,l3_rad%ppab,1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'red_factor', vid1redfac,l3_rad%reduction_factor,1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'var_para', vid1varpara,l3_rad%variance_para,1,ncols,1,nrows, verbose)

  endif

  !close the file
  CALL nc_close(ncid_out,trim(adjustl(L3_outputpath_and_file)), verbose)
