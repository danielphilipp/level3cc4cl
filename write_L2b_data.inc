  ! Name: write_L2b_data.inc
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
  !2015/04/23 OS set CTH vmin to -1; added cld_type, stemp, satzen, solzen, relazi
  !2015/07/10 OS updated time definition; added cc_total_uncertainty;
  !           bug in closing output file fixed
  !2015-07-07 CP add corrected cloud top height
  !2015-10-22 CP CF compliance and comments add stemp
  !2015-12-16 OS replaced wo with verbose; updated time definition;
  !           updated L3 variable names; removed npoints and duplicate stemp from output
  !2016-01-21 OS replaced ref with cer; added vmin and vmax for stemp unc
  !2016-02-18 OS change in time reference date and attributes, added
  !           CTT_corrected, CTP_corrected, CEE
  !2016-02-23 OS changed time write call
  !2016-02-26 OS Added cdm_data_type, naming_authority, and keywords_vocabulary; changed time scale/offset;
  !           some NetCDF variable unit/comment/name changes
  !2016-02-26 OS changed flag_masks of qcflag
  !2016-03-04 OS several changes in NetCDF attributes, comments, and variable names towards CF compliancy
  !2016-03-18 OS write float variables as scaled integers; some cleanup
  !2017/06/22 OS Added toa/boa, ann phase, MODIS BT and refl
  !2017/06/29 OS Renamed ann_phase to cph, added cph uncertainties
  ! 
  ! $Id$
  !
  ! Bugs:
  !
  !none known

  call nc_create_globall2b( trim(adjustl(l3_outputpath_and_file)), ncols, nrows, gridx, gridy,&
       & slon,elon,slat,elat, &
       & ncid_out, dims_var, verbose, &
       & cncver,ccon,cinst,l2cproc,l2cprocver,l3cproc,l3cprocver,contact,website, &
       & prodtype, platform, csensor, uuid_tag, exec_time, prod_name, &
       & year, month, day, dur, grid_type, standard_name_voc, &
       & naming_authority, cdm_data_type, keywords_vocabulary, &
       & reference, history, summary, keywords, comment, project, license,cfile_version,csource)

  !activate and modify parameter list once this is clear
  !  CALL nc_global_att_L3(ncid_new, fname_newL3, valid, st, st_iso, &
  !       tem, spa, instr, satser, satids, lev, type, yday, daynight, verbose, v_prod, proc_v_l2, proc_v_l3)

  !define and write here the actual data now!!!!
  !number of points
  !  CALL nc_defdata_lint(ncid_out, dims_var, 'npoints_l2b_asc', vid1pl2b_asc, 'number of l2b points ascending', &
  !       & 'number_of_l2b_points', '1', long_int_fill_value, verbose,1,0,0,1,0,1,'npoints for sampling must be cloud converged and within cost boundaries')
  !  CALL nc_defdata_lint(ncid_out, dims_var, 'npoints_l2b_desc', vid1pl2b_desc, 'number of l2b points descending', &
  !       & 'number_of_l2b_points', '1', long_int_fill_value, verbose,1,0,0,1,0,1,'npoints for sampling must be cloud converged and within cost boundaries')

  !---------------- time -------------------

  dummy_scale  = time_scale
  dummy_offset = real((time_start-jul_start), dreal)
  dummy_vmin   = 0
  dummy_vmax   = nint(1.0d0/dummy_scale, lint)
  CALL nc_defdata_time(ncid_out, dims_var, 'time_asc', vid0l2b_asc, 'time ascending node', &
       & 'time','standard','days since 1970-01-01 00:00:00',long_int_fill_value,verbose, &
       &  dummy_scale,dummy_offset,dummy_vmin,dummy_vmax,1,0,'')
  CALL nc_defdata_time(ncid_out, dims_var, 'time_desc', vid0l2b_desc, 'time descending node', &
       & 'time','standard','days since 1970-01-01 00:00:00',long_int_fill_value,verbose, &
       &  dummy_scale,dummy_offset,dummy_vmin,dummy_vmax,1,0,'')

  !--------------- ctt ------------------

  scale=scale_100
  offset=offset_temp
  vmin=(0. - offset_temp) / scale_100
  vmax=(350. - offset_temp) / scale_100
  comment_ctt = 'CTT retrieved at approx. 1 optical depth below cloud top'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_asc', vid1l2b_asc, 'cloud top temperature ascending', &
       & 'air_temperature_at_cloud_top', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctt)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_asc_unc', vid1l2be_asc, 'cloud top temperature uncertainty ascending', &
       & 'air_temperature_at_cloud_top standard_error', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_desc', vid1l2b_desc, 'cloud top temperature descending', &
       & 'air_temperature_at_cloud_top', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctt)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_desc_unc', vid1l2be_desc, 'cloud top temperature uncertainty descending', &
       & 'air_temperature_at_cloud_top standard_error', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !------------------ ctt_corrected ----------------------

  scale=scale_100
  offset=offset_temp
  vmin=(0. - offset_temp) / scale_100
  vmax=(350. - offset_temp) / scale_100
  comment_CTT_corrected = 'as CTT, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_corrected_asc',vid1l2b_corr_asc, 'corrected cloud top temperature ascending', &
       & 'air_temperature_at_cloud_top', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctt_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_corrected_asc_unc',vid1l2be_corr_asc, 'corrected cloud top temperature uncertainty ascending', &
       & 'air_temperature_at_cloud_top standard_error', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_corrected_desc',vid1l2b_corr_desc, 'corrected cloud top temperature descending', &
       & 'air_temperature_at_cloud_top', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctt_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctt_corrected_desc_unc',vid1l2be_corr_desc, 'corrected cloud top temperature uncertainty descending', &
       & 'air_temperature_at_cloud_top standard_error', 'K', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !--------------------- cee ------------------------

  scale=scale_10000
  offset=0.
  vmin=0.
  vmax= 1. / scale_10000

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cee_asc', vidceel2b_asc, 'cloud effective emissivity at 10.8 um ascending', &
       & 'cloud_effective_emissivity', '1', nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cee_asc_unc', vidceel2be_asc,'cloud effective emissivity uncertainty at 10.8 um ascending', &
       & 'cloud_effective_emissivity standard_error', '1', nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cee_desc', vidceel2b_desc, 'cloud effective emissivity at 10.8 um descending', &
       & 'cloud_effective_emissivity', '1', nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cee_desc_unc', vidceel2be_desc,'cloud effective emissivity uncertainty at 10.8 um descending', &
       & 'cloud_effective_emissivity standard_error', '1', nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !--------------------- cth -----------------------

  scale=scale_1000
  offset=0.00
  vmin=-1. / scale_1000
  vmax=20. / scale_1000
  comment_cth = 'CTH retrieved at approx. 1 optical depth below cloud top'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_asc', vidcthl2b_asc, 'cloud top height ascending', &
       & 'cloud_top_altitude', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_cth)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_asc_unc', vidcthl2be_asc, 'cloud top height uncertainty ascending', &
       & 'cloud_top_altitude standard_error', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_desc', vidcthl2b_desc, 'cloud top height descending', &
       & 'cloud_top_altitude', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_cth)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_desc_unc', vidcthl2be_desc, 'cloud top height uncertainty descending', &
       & 'cloud_top_altitude standard_error', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !---------------- cth_corrected ------------------

  scale=scale_1000
  offset=0.00
  vmin=-1. / scale_1000
  vmax=20. / scale_1000
  comment_CTH_corrected = 'as CTH, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_corrected_asc',vidcthcorrl2b_asc, 'corrected cloud top height ascending', &
       & 'cloud_top_altitude', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_CTH_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_corrected_asc_unc',vidcthcorrl2be_asc, 'corrected cloud top height uncertainty ascending', &
       & 'cloud_top_altitude standard_error', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_corrected_desc',vidcthcorrl2b_desc, 'corrected cloud top height descending', &
       & 'cloud_top_altitude', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_CTH_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cth_corrected_desc_unc',vidcthcorrl2be_desc, 'corrected cloud top height uncertainty descending', &
       & 'cloud_top_altitude standard_error', 'km', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !------------------- ctp --------------------

  scale=scale_ctp
  offset=offset_ctp
  vmin=(0. - offset_ctp) / scale_ctp
  vmax=(1200. - offset_ctp) / scale_ctp
  comment_ctp = 'CTP retrieved at approx. 1 optical depth below cloud top'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_asc', vid3l2b_asc, 'cloud top pressure ascending', &
       & 'air_pressure_at_cloud_top', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctp)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_asc_unc', vid3l2be_asc, 'cloud top pressure uncertainty ascending', &
       & 'air_pressure_at_cloud_top standard_error', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_desc', vid3l2b_desc, 'cloud top pressure descending', &
       & 'air_pressure_at_cloud_top', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctp)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_desc_unc', vid3l2be_desc, 'cloud top pressure uncertainty descending', &
       & 'air_pressure_at_cloud_top standard_error', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !-------------- ctp_corrected -----------------

  scale=scale_ctp
  offset=offset_ctp
  vmin=(0. - offset_ctp) / scale_ctp
  vmax=(1200. - offset_ctp) / scale_ctp
  comment_ctp_corrected = 'as CTP, but ice cloud values are corrected with microphysical information'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_corrected_asc',vid3l2b_corr_asc, 'corrected cloud top pressure ascending', &
       & 'air_pressure_at_cloud_top', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctp_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_corrected_asc_unc',vid3l2be_corr_asc, 'corrected cloud top pressure uncertainty ascending', &
       & 'air_pressure_at_cloud_top standard_error', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_corrected_desc',vid3l2b_corr_desc, 'corrected cloud top pressure descending', &
       & 'air_pressure_at_cloud_top', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_ctp_corrected)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'ctp_corrected_desc_unc',vid3l2be_corr_desc, 'corrected cloud top pressure uncertainty descending', &
       & 'air_pressure_at_cloud_top standard_error', 'hPa', stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !----------------- cph --------------------

  scale=1
  offset=0
  vmin=0
  vmax=2
  nflag=3
  allocate(flag_val(nflag))
  flag_val(1) = int(0,kind=sint)
  flag_val(2) = int(1,kind=sint)
  flag_val(3) = int(2,kind=sint)

  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cph_asc', vid4l2b_asc, 'cloud phase flag ascending', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_val, nflag, 'clear liquid ice',&
       &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')
  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cph_desc', vid4l2b_desc, 'cloud phase flag descending', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_val, nflag, 'clear liquid ice',&
       &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')

  deallocate(flag_val)

  !---------------- ann_phase -------------------

  ! scale=1
  ! offset=0
  ! vmin=0
  ! vmax=2
  ! nflag=3
  ! allocate(flag_val(nflag))
  ! flag_val(1) = int(0,kind=sint)
  ! flag_val(2) = int(1,kind=sint)
  ! flag_val(3) = int(2,kind=sint)
  
  ! CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'ann_phase_asc', vidannphase_asc, 'neural network cloud phase flag ascending', &
  !      & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_val, nflag, 'clear liquid ice',&
  !      &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')
  ! CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'ann_phase_desc', vidannphase_desc, 'neural network cloud phase flag descending', &
  !      & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top', flag_val, nflag, 'clear liquid ice',&
  !      &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')

  ! deallocate(flag_val)
  
  !------------------ illum ---------------------

  scale=1
  offset=0
  vmin=1
  vmax=12
  nflag=3
  allocate(flag_val(nflag))
  flag_val(1) = int(1,kind=sint)
  flag_val(2) = int(2,kind=sint)
  flag_val(3) = int(3,kind=sint)
  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'illum_asc', vidilluml2b_asc, 'solar illumination flag ascending', &
       & 'solar_illumination_flag', flag_val, nflag, 'day twilight night',short_int_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')
  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'illum_desc', vidilluml2b_desc, 'solar illumination flag descending', &
       & 'solar_illumination_flag', flag_val, nflag, 'day twilight night',short_int_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')

  deallocate(flag_val)

  !----------------- cld_type -------------------

  scale=1
  offset=0
  vmin=0
  vmax=9
  nflag=10
  allocate(flag_val(nflag))
  flag_val(1)  = int(0,kind=sint)
  flag_val(2)  = int(1,kind=sint)
  flag_val(3)  = int(2,kind=sint)
  flag_val(4)  = int(3,kind=sint)
  flag_val(5)  = int(4,kind=sint)
  flag_val(6)  = int(5,kind=sint)
  flag_val(7)  = int(6,kind=sint)
  flag_val(8)  = int(7,kind=sint)
  flag_val(9)  = int(8,kind=sint)
  flag_val(10) = int(9,kind=sint)
 
  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cty_asc', vidcld_type_l2b_asc, 'Pavolonis cloud type ascending', &
       & 'Pavolonis_cloud_type_ascending', flag_val, nflag, &
       & 'clear switched_to_liquid fog liquid supercooled switched_to_ice opaque_ice cirrus overlap probably_opaque_ice',&
       & short_int_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')

  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cty_desc', vidcld_type_l2b_desc, 'Pavolonis cloud type descending', &
       & 'Pavolonis_cloud_type_descending', flag_val, nflag, &
       & 'clear switched_to_liquid fog liquid supercooled switched_to_ice opaque_ice cirrus overlap probably_opaque_ice', &
       & short_int_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')

  deallocate(flag_val)

  !------------------- stemp -----------------------

  scale=scale_100
  offset=offset_temp !0.
  vmin=(0. - offset_temp) / scale_100 !200.00 / scale_100
  vmax=(350. - offset_temp) / scale_100 !350.00 / scale_100
  comment_stemp='stemp is retrieved only under optically thin clouds, and otherwise reverts to the apriori ECMWF ERA-Interim stemp value'

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'stemp_asc', vidstemp_l2b_asc, 'surface temperature ascending', &
       & 'surface_temperature', 'K',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_stemp)
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'stemp_desc', vidstemp_l2b_desc, 'surface temperature descending', &
       & 'surface_temperature', 'K',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,1,comment_stemp)

  !------------------ stemp unc ------------------
  scale=scale_100
  offset=offset_temp !0.
  vmin=(0. - offset_temp) / scale_100 !200.00 / scale_100
  vmax=(350. - offset_temp) / scale_100 !350.00 / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'stemp_asc_unc', vidstemp_l2be_asc, 'surface temperature uncertainty ascending', &
       & 'surface_temperature standard_error', 'K',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'stemp_desc_unc', vidstemp_l2be_desc, 'surface temperature uncertainty descending', &
       & 'surface_temperature standard_error', 'K',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !------------------- MODIS BT ------------------
  if (llocal) then
     scale=scale_100
     offset=offset_temp
     vmin=(0. - offset_temp) / scale_100
     vmax=(350. - offset_temp) / scale_100
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_nir037_asc', vidbt37_l2b_asc, 'brightness temperature at 3.7 um ascending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_nir037_desc', vidbt37_l2b_desc, 'brightness temperature at 3.7 um descending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_tir108_asc', vidbt11_l2b_asc, 'brightness temperature at 10.8 um ascending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_tir108_desc', vidbt11_l2b_desc, 'brightness temperature at 10.8 um descending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_tir120_asc', vidbt12_l2b_asc, 'brightness temperature at 12.0 um ascending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'bt_tir120_desc', vidbt12_l2b_desc, 'brightness temperature at 12.0 um descending', &
          & 'toa_brightness_temperature', 'K',&
          & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  endif


  !--------------------- platform zenith angle ----------------------
  scale=scale_100
  offset=0.00
  vmin=0.
  vmax=180. / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'satzen_asc', vidsatzen_l2b_asc, 'sensor zenith view no1 ascending', &
       & 'sensor_zenith_angle', 'degrees',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'satzen_desc', vidsatzen_l2b_desc, 'sensor zenith view no1 descending', &
       & 'sensor_zenith_angle', 'degrees',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')

  !--------------------- solar zenith angle ----------------------
  scale=scale_100
  offset=0.00
  vmin=  0.
  vmax=180. / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'solzen_asc', vidsolzen_l2b_asc, 'solar zenith view no1 ascending', &
       & 'solar_zenith_angle', 'degrees',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'solzen_desc', vidsolzen_l2b_desc, 'solar zenith view no1 descending', &
       & 'solar_zenith_angle', 'degrees',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')

  !-------------------- relative azimuth angle -------------------
  scale=scale_100
  offset=0.00
  vmin=-180. / scale_100
  vmax=180. / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'relazi_asc', vidrelazi_l2b_asc, 'relative azimuth view no1 ascending', &
       & 'relative_azimuth_angle', 'degrees',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'relazi_desc', vidrelazi_l2b_desc, 'relative azimuth view no1 descending', &
       & 'relative_azimuth_angle', 'degrees',&
       & stint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !--------------------- cer -------------------------
  scale=scale_100
  offset=0.00
  vmin=0.00
  vmax=200.00 / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cer_asc', vid5l2b_asc, 'cloud effective radius ascending', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', 'um',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cer_asc_unc', vid5l2be_asc, 'cloud effective radius uncertainty ascending', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard_error', 'um',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cer_desc', vid5l2b_desc, 'cloud effective radius descending', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top', 'um',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cer_desc_unc', vid5l2be_desc, 'cloud effective radius uncertainty descending', &
       & 'effective_radius_of_cloud_condensed_water_particles_at_cloud_top standard_error', 'um',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !------------------ cot ------------------------

  scale=scale_100
  offset=0.00
  vmin=0.00
  vmax=256.00 / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cot_asc', vid6l2b_asc, 'cloud optical thickness ascending', &
       & 'atmosphere_optical_thickness_due_to_cloud','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cot_asc_unc', vid6l2be_asc, 'cloud optical thickness uncertainty ascending', &
       & 'atmosphere_optical_thickness_due_to_cloud standard_error','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cot_desc', vid6l2b_desc, 'cloud optical thickness descending', &
       & 'atmosphere_optical_thickness_due_to_cloud','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cot_desc_unc',&
       & vid6l2be_desc, 'cloud optical thickness uncertainty descending',&
       & 'atmosphere_optical_thickness_due_to_cloud standard_error','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')


  !--------------------- toa ------------------
  if (proc_toa) then
     scale=0.1
     offset=0.00
     vmin=0.00
     vmax=1500. / 0.1

     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_lwup_asc', vidtoa_lwupl2b_asc, 'top of atmosphere upwelling thermal radiation ascending', &
          & 'toa_outgoing_longwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_lwup_clr_asc', vidtoa_lwup_clrl2b_asc, 'top of atmosphere upwelling thermal radiation ascending', &
          & 'toa_outgoing_longwave_flux_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swup_asc', vidtoa_swupl2b_asc, 'top of atmosphere upwelling solar radiation ascending', &
          & 'toa_outgoing_shortwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swup_clr_asc', vidtoa_swup_clrl2b_asc, 'top of atmosphere upwelling solar radiation ascending', &
          & 'toa_outgoing_shortwave_flux_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swdn_asc', vidtoa_swdnl2b_asc, 'top of atmosphere downwelling solar radiation ascending', &
          & 'toa_downwelling_shortwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')

     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_lwup_desc', vidtoa_lwupl2b_desc, 'top of atmosphere upwelling thermal radiation descending', &
          & 'toa_outgoing_longwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_lwup_clr_desc', vidtoa_lwup_clrl2b_desc, 'top of atmosphere upwelling thermal radiation descending', &
          & 'toa_outgoing_longwave_flux_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swup_desc', vidtoa_swupl2b_desc, 'top of atmosphere upwelling solar radiation descending', &
          & 'toa_outgoing_shortwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swup_clr_desc', vidtoa_swup_clrl2b_desc, 'top of atmosphere upwelling solar radiation descending', &
          & 'toa_outgoing_shortwave_flux_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'toa_swdn_desc', vidtoa_swdnl2b_desc, 'top of atmosphere downwelling solar radiation descending', &
          & 'toa_downwelling_shortwave_flux','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')

     !------------------------ boa -------------------------
     scale=0.1
     offset=0.00
     vmin=0.00
     vmax=1500. / 0.1

     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swup_asc', vidboa_swupl2b_asc, 'bottom of atmosphere upwelling solar radiation ascending', &
          & 'surface_upwelling_shortwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swup_clr_asc', vidboa_swup_clrl2b_asc, 'bottom of atmosphere upwelling solar radiation ascending', &
          & 'surface_upwelling_shortwave_flux_in_air_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwup_asc', vidboa_lwupl2b_asc, 'bottom of atmosphere upwelling thermal radiation ascending', &
          & 'surface_upwelling_longwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwup_clr_asc', vidboa_lwup_clrl2b_asc, 'bottom of atmosphere upwelling thermal radiation ascending', &
          & 'surface_upwelling_longwave_flux_assuming_in_air_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swdn_asc', vidboa_swdnl2b_asc, 'bottom of atmosphere downwelling solar radiation ascending', &
          & 'surface_downwelling_shortwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swdn_clr_asc', vidboa_swdn_clrl2b_asc, 'bottom of atmosphere downwelling solar radiation ascending', &
          & 'surface_downwelling_shortwave_flux_in_air_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwdn_asc', vidboa_lwdnl2b_asc, 'bottom of atmosphere downwelling thermal radiation ascending', &
          & 'surface_downwelling_longwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwdn_clr_asc', vidboa_lwdn_clrl2b_asc, 'bottom of atmosphere downwelling thermal radiation ascending', &
          & 'surface_downwelling_longwave_flux_assuming_in_air_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_par_dif_asc', vidboa_par_difl2b_asc, 'surface diffuse par ascending', &
          & 'surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_par_tot_asc', vidboa_par_totl2b_asc, 'surface total par ascending', &
          & 'surface_downwelling_photosynthetic_radiative_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')

     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swup_desc', vidboa_swupl2b_desc, 'bottom of atmosphere upwelling solar radiation descending', &
          & 'surface_upwelling_shortwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swup_clr_desc', vidboa_swup_clrl2b_desc, 'bottom of atmosphere upwelling solar radiation descending', &
          & 'surface_upwelling_shortwave_flux_in_air_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwup_desc', vidboa_lwupl2b_desc, 'bottom of atmosphere upwelling thermal radiation descending', &
          & 'surface_upwelling_longwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwup_clr_desc', vidboa_lwup_clrl2b_desc, 'bottom of atmosphere upwelling thermal radiation descending', &
          & 'surface_upwelling_longwave_flux_assuming_in_air_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swdn_desc', vidboa_swdnl2b_desc, 'bottom of atmosphere downwelling solar radiation descending', &
          & 'surface_downwelling_shortwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_swdn_clr_desc', vidboa_swdn_clrl2b_desc, 'bottom of atmosphere downwelling solar radiation descending', &
          & 'surface_downwelling_shortwave_flux_in_air_assuming_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwdn_desc', vidboa_lwdnl2b_desc, 'bottom of atmosphere downwelling thermal radiation descending', &
          & 'surface_downwelling_longwave_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_lwdn_clr_desc', vidboa_lwdn_clrl2b_desc, 'bottom of atmosphere downwelling thermal radiation descending', &
          & 'surface_downwelling_longwave_flux_assuming_in_air_clear_sky','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_par_dif_desc', vidboa_par_difl2b_desc, 'surface diffuse par descending', &
          & 'surface_diffuse_downwelling_photosynthetic_radiative_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'boa_par_tot_desc', vidboa_par_totl2b_desc, 'surface total par descending', &
          & 'surface_downwelling_photosynthetic_radiative_flux_in_air','1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),1,0,'')
  endif

  !------------------------ cccot ----------------------

  scale=scale_10000
  offset=0.00
  vmin=0.00
  vmax=1.00 / scale_10000

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cccot_asc', vidcccot_prel2b_asc, 'cloud mask pseudo optical thickness ascending', &
       & 'cloud_mask_pseudo_optical_thickness','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cccot_desc',vidcccot_prel2b_desc, 'cloud mask pseudo optical thickness descending', &
       & 'cloud_mask_pseudo_optical_thickness','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !---------------------- ann_phase_uncertainty -----------------------

  scale=scale_100
  offset=0.00
  vmin=0.00
  vmax=100. / scale_100

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cph_asc_unc', vidannphase_unc_asc, 'cloud phase uncertainty ascending', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cph_desc_unc',vidannphase_unc_desc, 'cloud phase uncertainty descending', &
       & 'thermodynamic_phase_of_cloud_water_particles_at_cloud_top','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  !------------------------ cphcot ------------------------------

  scale=scale_10000
  offset=0.00
  vmin=0.00
  vmax=1.00 / scale_10000

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cph_interim_asc', vidcphcot_asc, 'neural network interim cloud phase ascending', &
       & 'neural_network_interim_cloud_phase','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cph_interim_desc', vidcphcot_desc, 'neural network interim cloud phase descending', &
       & 'neural_network_interim_cloud_phase','1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  
  !------------------------- cwp -------------------------------

  scale=1.00
  offset=0.00
  vmin=0.0
  vmax=32000.0

  CALL nc_defdata_float(ncid_out, dims_var, 'cwp_asc', vid7l2b_asc, 'cloud water path ascending', &
       & 'atmosphere_mass_content_of_cloud_condensed_water', 'g/m2',&
       & real_fill_value, verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cwp_asc_unc', vid7l2be_asc, 'cloud water path uncertainty ascending', &
       & 'atmosphere_mass_content_of_cloud_condensed_water standard_error', 'g/m2',&
       & real_fill_value, verbose,scale,offset,vmin,vmax,0,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cwp_desc', vid7l2b_desc, 'cloud water path descending', &
       & 'atmosphere_mass_content_of_cloud_condensed_water', 'g/m2',&
       & real_fill_value, verbose,scale,offset,vmin,vmax,1,0,'')
  CALL nc_defdata_float(ncid_out, dims_var, 'cwp_desc_unc', vid7l2be_desc, 'cloud water path uncertainty descending', &
       & 'atmosphere_mass_content_of_cloud_condensed_water standard_error', 'g/m2',&
       & real_fill_value, verbose,scale,offset,vmin,vmax,0,0,'')

  !-------------------- cloud_albedo1 --------------------------

  scale=scale_10000
  offset=0.00
  vmin=0.0
  vmax=1.0 / scale_10000

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis006_asc', vid9l2b_asc,'cloud albedo at 0.6 um ascending', &
       & 'cloud_albedo', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis006_asc_unc', vid9l2be_asc,'cloud albedo at 0.6 um uncertainty ascending', &
       & 'cloud_albedo standard_error', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis006_desc', vid9l2b_desc,'cloud albedo at 0.6 um descending', &
       & 'cloud_albedo', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis006_desc_unc',vid9l2be_desc, 'cloud albedo at 0.6 um uncertainty descending', &
       & 'cloud_albedo standard_error', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')


  !----------------------- cloud_albedo2 ---------------------------

  scale=scale_10000
  offset=0.00
  vmin=0.0
  vmax=1.0 / scale_10000

  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis008_asc', vid10l2b_asc, 'cloud albedo at 0.8 um ascending', &
       & 'cloud_albedo', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis008_asc_unc', vid10l2be_asc, 'cloud albedo at 0.8 um uncertainty ascending', &
       & 'cloud_albedo standard_error', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis008_desc', vid10l2b_desc, 'cloud albedo at 0.8 um descending', &
       & 'cloud_albedo', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cla_vis008_desc_unc', vid10l2be_desc, 'cloud albedo at 0.8 um uncertainty descending', &
       & 'cloud_albedo standard_error', '1',&
       & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')

  ! ---------------------- MODIS reflectance ---------------------------
  if (llocal) then
     scale=scale_10000
     offset=0.00
     vmin=0.0
     vmax=1.3 / scale_10000

     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis006_asc', vidrefl06_l2b_asc,'reflectance at 0.6 um ascending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis006_desc', vidrefl06_l2b_desc,'reflectance at 0.6 um descending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis008_asc', vidrefl08_l2b_asc,'reflectance at 0.8 um ascending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis008_desc', vidrefl08_l2b_desc,'reflectance at 0.8 um descending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis016_asc', vidrefl16_l2b_asc,'reflectance at 1.6 um ascending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
     CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'refl_vis016_desc', vidrefl16_l2b_desc,'reflectance at 1.6 um descending', &
          & '', '1',&
          & nint_fill_value, verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  endif

  !---------------------- cloud masks ---------------------------
  scale=1
  offset=0
  vmin=0
  vmax=1
  nflag=2
  allocate(flag_val(nflag))
  flag_val(1) = int(0,kind=sint)
  flag_val(2) = int(1,kind=sint)

  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cmask_asc', vid8l2b_asc, 'cloud mask flag ascending', &
       & 'cloud_binary_mask', flag_val, nflag, 'clear cloudy', &
       &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')
  CALL nc_defdata_byte_flag_value(ncid_out, dims_var, 'cmask_desc', vid8l2b_desc, 'cloud mask flag descending', &
       & 'cloud_binary_mask', flag_val, nflag, 'clear cloudy', &
       &  short_int_fill_value,scale,offset,vmin,vmax,verbose,1,0,'')

  deallocate(flag_val)

  !------------------------- cloud mask uncertainty -------------------------------
  scale=scale_100
  offset=0.0
  vmin=0.
  vmax=100. / scale_100
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var, 'cmask_asc_unc', vid8l2be_asc, 'cloud mask uncertainty ascending', &
       & 'cloud_binary_mask standard_error','%', nint_fill_value,verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')
  CALL nc_defdata_stint_scale_float(ncid_out, dims_var,'cmask_desc_unc', vid8l2be_desc, 'cloud mask uncertainty descending', &
       & 'cloud_binary_mask standard_error','%',nint_fill_value,verbose,scale,offset,nint(vmin, stint),nint(vmax, stint),0,0,'')


  !--------------------------- qc flag --------------------------
  !s_input_dummy='0:unused 1:COT_out_of_bounds 2:CER_out_of_bounds 3:CTP_out_of_bounds 4:CCT_out_of_bounds 5:STEMP_out_of_bounds 6:no_convergence 7:cost_too_large'
  s_input_dummy='cost_too_large OT_out_of_range ER_out_of_range CTP_out_of_range STEMP_out_of_range ' // &
       '0_63_um_legacy_channel_used 0_86_um_legacy_channel_used 1_61_um_legacy_channel_used 3_74_um_legacy_channel_used ' // &
       '10_8_um_legacy_channel_used 12_0_um_legacy_channel_used'

  scale=1
  offset=0
  vmin=0
  vmax=2048
  nflag=11
  allocate(stint_flag_val(nflag))
  stint_flag_val(1) = int(1,kind=stint)
  stint_flag_val(2) = int(2,kind=stint)
  stint_flag_val(3) = int(4,kind=stint)
  stint_flag_val(4) = int(8,kind=stint)
  stint_flag_val(5) = int(16,kind=stint)
  stint_flag_val(6) = int(32,kind=stint)
  stint_flag_val(7) = int(64,kind=stint)
  stint_flag_val(8) = int(128,kind=stint)
  stint_flag_val(9) = int(256,kind=stint)
  stint_flag_val(10) = int(512,kind=stint)
  stint_flag_val(11) = int(1024,kind=stint)

  !bit_flag_vals = '1b 2b 4b 8b 16b 32b 64b 128b 256b 512b 1024b'

  CALL nc_defdata_stint_flag_value(ncid_out, dims_var, 'qcflag_asc', &
       & vidqcl2b_asc, 'quality control flag ascending',&
       & 'quality_control_flag', '1', stint_flag_val, nflag, trim(adjustl(s_input_dummy)),&
       & nint_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')
  CALL nc_defdata_stint_flag_value(ncid_out, dims_var, 'qcflag_desc', &
       & vidqcl2b_desc, 'quality control flag descending', &
       & 'quality_control_flag', '1', stint_flag_val, nflag, trim(adjustl(s_input_dummy)), &
       & nint_fill_value,scale,offset,vmin,vmax,verbose,0,0,'')

  deallocate(stint_flag_val)

  ! npoints
  !   CALL nc_write_L3_lint(ncid_out,'npoints_l2b_asc',vid1pl2b_asc,l2b_macro_micro_asc%counter(:,:),1,ncols,1,nrows, verbose)
  !   CALL nc_write_L3_lint(ncid_out,'npoints_l2b_desc',vid1pl2b_desc,l2b_macro_micro_desc%counter(:,:),1,ncols,1,nrows, verbose)
  
  !-------------------- time ---------------------------
  CALL nc_write_L3_lint(ncid_out,'time_asc',vid0l2b_asc,l2b_macro_micro_asc%time(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_lint(ncid_out,'time_desc',vid0l2b_desc,l2b_macro_micro_desc%time(:,:),1,ncols,1,nrows, verbose)

  !---------------------- ctt ---------------------------
  CALL nc_write_L3_float(ncid_out,'ctt_asc',vid1l2b_asc,l2b_macro_micro_asc%ctt(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_asc_unc',vid1l2be_asc,&
       & l2b_macro_micro_asc%ctt_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_desc',vid1l2b_desc, &
       & l2b_macro_micro_desc%ctt(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_desc_unc',vid1l2be_desc, &
       & l2b_macro_micro_desc%ctt_error(:,:),1,ncols,1,nrows, verbose)

  !------------------ ctt_corrected ---------------------
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_asc',vid1l2b_corr_asc,l2b_macro_micro_asc%ctt_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_asc_unc',vid1l2be_corr_asc,&
       & l2b_macro_micro_asc%ctt_corrected_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_desc',vid1l2b_corr_desc, &
       & l2b_macro_micro_desc%ctt_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctt_corrected_desc_unc',vid1l2be_corr_desc, &
       & l2b_macro_micro_desc%ctt_corrected_error(:,:),1,ncols,1,nrows, verbose)


  !-------------------------- cth -------------------------
  CALL nc_write_L3_float(ncid_out,'cth_asc',vidcthl2b_asc,l2b_macro_micro_asc%cth(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_asc_unc',vidcthl2be_asc, &
       & l2b_macro_micro_asc%cth_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_desc',vidcthl2b_desc,l2b_macro_micro_desc%cth(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_desc_unc',vidcthl2be_desc, &
       & l2b_macro_micro_desc%cth_error(:,:),1,ncols,1,nrows, verbose)


  !---------------------- cth_corrected ------------------
  CALL nc_write_L3_float(ncid_out,'cth_corrected_asc',vidcthcorrl2b_asc,l2b_macro_micro_asc%cth_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_asc_unc',vidcthcorrl2be_asc, &
       & l2b_macro_micro_asc%cth_corrected_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_desc',vidcthcorrl2b_desc,l2b_macro_micro_desc%cth_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cth_corrected_desc_unc',vidcthcorrl2be_desc, &
       & l2b_macro_micro_desc%cth_corrected_error(:,:),1,ncols,1,nrows, verbose)


  !--------------------------- ctp -----------------------
  CALL nc_write_L3_float(ncid_out,'ctp_asc',vid3l2b_asc,l2b_macro_micro_asc%ctp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_asc_unc',vid3l2be_asc, &
       & l2b_macro_micro_asc%ctp_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_desc',vid3l2b_desc,l2b_macro_micro_desc%ctp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_desc_unc',vid3l2be_desc, &
       & l2b_macro_micro_desc%ctp_error(:,:),1,ncols,1,nrows, verbose)

  !------------------------ ctp_corrected ---------------------------
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_asc',vid3l2b_corr_asc,l2b_macro_micro_asc%ctp_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_asc_unc',vid3l2be_corr_asc, &
       & l2b_macro_micro_asc%ctp_corrected_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_desc',vid3l2b_corr_desc,l2b_macro_micro_desc%ctp_corrected(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'ctp_corrected_desc_unc',vid3l2be_corr_desc, &
       & l2b_macro_micro_desc%ctp_corrected_error(:,:),1,ncols,1,nrows, verbose)

  !-------------------------- cc_total ------------------------------
  CALL  nc_write_L3_byte_flag(ncid_out,'cmask_asc',vid8l2b_asc,&
       & l2b_macro_micro_asc%cct(:,:),1,ncols,1,nrows, verbose)
  CALL  nc_write_L3_byte_flag(ncid_out,'cmask_desc',vid8l2b_desc,&
       & l2b_macro_micro_desc%cct(:,:),1,ncols,1,nrows, verbose)

  !----------------------- cc_total_uncertainty --------------------------
  CALL nc_write_L3_float(ncid_out,'cmask_asc_unc',vid8l2be_asc,l2b_macro_micro_asc%cct_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cmask_desc_unc',vid8l2be_desc,l2b_macro_micro_desc%cct_error(:,:),1,ncols,1,nrows, verbose)

  !--------------------------- cph ----------------------------
  CALL  nc_write_L3_byte_flag(ncid_out,'cph_asc',vid4l2b_asc,&
       & l2b_macro_micro_asc%cty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_byte_flag(ncid_out,'cph_desc',vid4l2b_desc,&
       & l2b_macro_micro_desc%cty(:,:),1,ncols,1,nrows, verbose)

  ! ! ann_phase
  ! CALL  nc_write_L3_byte_flag(ncid_out,'ann_phase_asc',vidannphase_asc,&
  !      & l2b_macro_micro_asc%ann_phase(:,:),1,ncols,1,nrows, verbose)
  ! CALL nc_write_L3_byte_flag(ncid_out,'ann_phase_desc',vidannphase_desc,&
  !      & l2b_macro_micro_desc%ann_phase(:,:),1,ncols,1,nrows, verbose)
  
  !------------------------ illum ------------------------
  CALL  nc_write_L3_byte_flag(ncid_out,'illum_asc',vidilluml2b_asc,&
       & l2b_macro_micro_asc%illum(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_byte_flag(ncid_out,'illum_desc',vidilluml2b_desc,&
       & l2b_macro_micro_desc%illum(:,:),1,ncols,1,nrows, verbose)

  !---------------------- cloud type ----------------------
  CALL  nc_write_L3_byte_flag(ncid_out,'cty_asc',vidcld_type_l2b_asc,&
       & l2b_macro_micro_asc%cld_type(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_byte_flag(ncid_out,'cty_desc',vidcld_type_l2b_desc,&
       & l2b_macro_micro_desc%cld_type(:,:),1,ncols,1,nrows, verbose)

  !--------------------------- cer -------------------------
  CALL nc_write_L3_float(ncid_out,'cer_asc',vid5l2b_asc,&
       & l2b_macro_micro_asc%ref(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_asc_unc',vid5l2be_asc, &
       & l2b_macro_micro_asc%ref_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_desc',vid5l2b_desc,&
       & l2b_macro_micro_desc%ref(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cer_desc_unc',vid5l2be_desc,&
       & l2b_macro_micro_desc%ref_error(:,:),1,ncols,1,nrows, verbose)

  !-------------------------- stemp ----------------------------
  CALL nc_write_L3_float(ncid_out,'stemp_asc',vidstemp_l2b_asc,&
       & l2b_macro_micro_asc%stemp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_asc_unc',vidstemp_l2be_asc, &
       & l2b_macro_micro_asc%stemp_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_desc',vidstemp_l2b_desc,&
       & l2b_macro_micro_desc%stemp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'stemp_desc_unc',vidstemp_l2be_desc, &
       & l2b_macro_micro_desc%stemp_error(:,:),1,ncols,1,nrows, verbose)

  ! ------------------------ MODIS BT -----------------------------
  if (llocal) then
     CALL nc_write_L3_float(ncid_out,'bt_nir037_asc',vidbt37_l2b_asc,&
          & l2b_macro_micro_asc%BT37(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'bt_nir037_desc',vidbt37_l2b_desc,&
          & l2b_macro_micro_desc%BT37(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'bt_tir108_asc',vidbt11_l2b_asc,&
          & l2b_macro_micro_asc%BT11(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'bt_tir108_desc',vidbt11_l2b_desc,&
          & l2b_macro_micro_desc%BT11(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'bt_tir120_asc',vidbt12_l2b_asc,&
          & l2b_macro_micro_asc%BT12(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'bt_tir120_desc',vidbt12_l2b_desc,&
          & l2b_macro_micro_desc%BT12(:,:),1,ncols,1,nrows, verbose)
  endif

  !----------------------- satellite zenith angle ----------------------------
  ! do scaling here, because unscaled values were needed for pixel sampling
  where (l2b_macro_micro_asc%satzen .ne. (-1.) * real_fill_value)
     l2b_macro_micro_asc%satzen = l2b_macro_micro_asc%satzen / scale_100
  elsewhere
     l2b_macro_micro_asc%satzen = real_fill_value
  endwhere

  where (l2b_macro_micro_desc%satzen .ne. (-1.) * real_fill_value)
     l2b_macro_micro_desc%satzen = l2b_macro_micro_desc%satzen / scale_100
  elsewhere
     l2b_macro_micro_desc%satzen = real_fill_value
  endwhere

  CALL nc_write_L3_float(ncid_out,'satzen_asc',vidsatzen_l2b_asc,&
       & l2b_macro_micro_asc%satzen(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'satzen_desc',vidsatzen_l2b_desc,&
       & l2b_macro_micro_desc%satzen(:,:),1,ncols,1,nrows, verbose)

  !----------------------- solar zenith angle ----------------------------
  CALL nc_write_L3_float(ncid_out,'solzen_asc',vidsolzen_l2b_asc,&
       & l2b_macro_micro_asc%solzen(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'solzen_desc',vidsolzen_l2b_desc,&
       & l2b_macro_micro_desc%solzen(:,:),1,ncols,1,nrows, verbose)

  !-------------------- relative azimuth angle -----------------------------
  CALL nc_write_L3_float(ncid_out,'relazi_asc',vidrelazi_l2b_asc,&
       & l2b_macro_micro_asc%relazi(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'relazi_desc',vidrelazi_l2b_desc,&
       & l2b_macro_micro_desc%relazi(:,:),1,ncols,1,nrows, verbose)

  !--------------------------- cot -------------------------------
  CALL nc_write_L3_float(ncid_out,'cot_asc',vid6l2b_asc, &
       & l2b_macro_micro_asc%cot(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_asc_unc',vid6l2be_asc, &
       & l2b_macro_micro_asc%cot_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_desc',vid6l2b_desc,&
       & l2b_macro_micro_desc%cot(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cot_desc_unc',vid6l2be_desc,&
       & l2b_macro_micro_desc%cot_error(:,:),1,ncols,1,nrows, verbose)

  !------------------------- toa -----------------------------
  if (proc_toa) then
     CALL nc_write_L3_float(ncid_out,'toa_lwup_asc',vidtoa_lwupl2b_asc, &
          & l2b_macro_micro_asc%toa_lwup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_clr_asc',vidtoa_lwup_clrl2b_asc, &
          & l2b_macro_micro_asc%toa_lwup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_asc',vidtoa_swupl2b_asc, &
          & l2b_macro_micro_asc%toa_swup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_clr_asc',vidtoa_swup_clrl2b_asc, &
          & l2b_macro_micro_asc%toa_swup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swdn_asc',vidtoa_swdnl2b_asc, &
          & l2b_macro_micro_asc%toa_swdn(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_desc',vidtoa_lwupl2b_desc, &
          & l2b_macro_micro_desc%toa_lwup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_lwup_clr_desc',vidtoa_lwup_clrl2b_desc, &
          & l2b_macro_micro_desc%toa_lwup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_desc',vidtoa_swupl2b_desc, &
          & l2b_macro_micro_desc%toa_swup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swup_clr_desc',vidtoa_swup_clrl2b_desc, &
          & l2b_macro_micro_desc%toa_swup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'toa_swdn_desc',vidtoa_swdnl2b_desc, &
          & l2b_macro_micro_desc%toa_swdn(:,:),1,ncols,1,nrows, verbose)

     !----------------------- boa --------------------------
     CALL nc_write_L3_float(ncid_out,'boa_swup_asc',vidboa_swupl2b_asc, &
          & l2b_macro_micro_asc%boa_swup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swup_clr_asc',vidboa_swup_clrl2b_asc, &
          & l2b_macro_micro_asc%boa_swup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup_asc',vidboa_lwupl2b_asc, &
          & l2b_macro_micro_asc%boa_lwup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup_clr_asc',vidboa_lwup_clrl2b_asc, &
          & l2b_macro_micro_asc%boa_lwup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn_asc',vidboa_swdnl2b_asc, &
          & l2b_macro_micro_asc%boa_swdn(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn_clr_asc',vidboa_swdn_clrl2b_asc, &
          & l2b_macro_micro_asc%boa_swdn_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn_asc',vidboa_lwdnl2b_asc, &
          & l2b_macro_micro_asc%boa_lwdn(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn_clr_asc',vidboa_lwdn_clrl2b_asc, &
          & l2b_macro_micro_asc%boa_lwdn_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_dif_asc',vidboa_par_difl2b_asc, &
          & l2b_macro_micro_asc%boa_par_dif(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_tot_asc',vidboa_par_totl2b_asc, &
          & l2b_macro_micro_asc%boa_par_tot(:,:),1,ncols,1,nrows, verbose)

     CALL nc_write_L3_float(ncid_out,'boa_swup_desc',vidboa_swupl2b_desc, &
          & l2b_macro_micro_desc%boa_swup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swup_clr_desc',vidboa_swup_clrl2b_desc, &
          & l2b_macro_micro_desc%boa_swup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup_desc',vidboa_lwupl2b_desc, &
          & l2b_macro_micro_desc%boa_lwup(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwup_clr_desc',vidboa_lwup_clrl2b_desc, &
          & l2b_macro_micro_desc%boa_lwup_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn_desc',vidboa_swdnl2b_desc, &
          & l2b_macro_micro_desc%boa_swdn(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_swdn_clr_desc',vidboa_swdn_clrl2b_desc, &
          & l2b_macro_micro_desc%boa_swdn_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn_desc',vidboa_lwdnl2b_desc, &
          & l2b_macro_micro_desc%boa_lwdn(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_lwdn_clr_desc',vidboa_lwdn_clrl2b_desc, &
          & l2b_macro_micro_desc%boa_lwdn_clr(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_dif_desc',vidboa_par_difl2b_desc, &
          & l2b_macro_micro_desc%boa_par_dif(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'boa_par_tot_desc',vidboa_par_totl2b_desc, &
          & l2b_macro_micro_desc%boa_par_tot(:,:),1,ncols,1,nrows, verbose)
  endif


  !----------------------- cccot_pre ---------------------------
  CALL nc_write_L3_float(ncid_out,'cccot_asc',vidcccot_prel2b_asc, &
       & l2b_macro_micro_asc%cccot_pre(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cccot_desc',vidcccot_prel2b_desc,&
       & l2b_macro_micro_desc%cccot_pre(:,:),1,ncols,1,nrows, verbose)
  
  !------------------- ann_phase_uncertainty -------------------------
  CALL nc_write_L3_float(ncid_out,'cph_asc_unc',vidannphase_unc_asc, &
       & l2b_macro_micro_asc%ann_phase_uncertainty(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_desc_unc',vidannphase_unc_desc, &
       & l2b_macro_micro_desc%ann_phase_uncertainty(:,:),1,ncols,1,nrows, verbose)
  
  !--------------------- ann_phase_interim ---------------------------
  CALL nc_write_L3_float(ncid_out,'cph_interim_asc',vidcphcot_asc, &
       & l2b_macro_micro_asc%cphcot(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cph_interim_desc',vidcphcot_desc, &
       & l2b_macro_micro_desc%cphcot(:,:),1,ncols,1,nrows, verbose)
  
  !-------------------------- cwp ------------------------------
  CALL nc_write_L3_float(ncid_out,'cwp_asc',vid7l2b_asc, &
       & l2b_macro_micro_asc%cwp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cwp_asc_unc',vid7l2be_asc, &
       & l2b_macro_micro_asc%cwp_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cwp_desc',vid7l2b_desc,&
       & l2b_macro_micro_desc%cwp(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cwp_desc_unc',vid7l2be_desc,&
       & l2b_macro_micro_desc%cwp_error(:,:),1,ncols,1,nrows, verbose)

  !---------------------- cloud_albedo1 --------------------------------
  CALL nc_write_L3_float(ncid_out,'cla_vis006_asc',vid9l2b_asc, &
       & l2b_macro_micro_asc%cloud_albedo1(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_asc_unc',vid9l2be_asc, &
       & l2b_macro_micro_asc%cloud_albedo1_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_desc',vid9l2b_desc,&
       & l2b_macro_micro_desc%cloud_albedo1(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis006_desc_unc',vid9l2be_desc,&
       & l2b_macro_micro_desc%cloud_albedo1_error(:,:),1,ncols,1,nrows, verbose)

  !--------------------- cloud_albedo2 ----------------------------
  CALL nc_write_L3_float(ncid_out,'cla_vis008_asc',vid10l2b_asc, &
       & l2b_macro_micro_asc%cloud_albedo2(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_asc_unc',vid10l2be_asc, &
       & l2b_macro_micro_asc%cloud_albedo2_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_desc',vid10l2b_desc,&
       & l2b_macro_micro_desc%cloud_albedo2(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cla_vis008_desc_unc',vid10l2be_desc,&
       & l2b_macro_micro_desc%cloud_albedo2_error(:,:),1,ncols,1,nrows, verbose)

  !-------------------- MODIS reflectance ------------------------
  if (llocal) then
     CALL nc_write_L3_float(ncid_out,'refl_vis006_asc',vidrefl06_l2b_asc, &
          & l2b_macro_micro_asc%reflectance06(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'refl_vis006_desc',vidrefl06_l2b_desc, &
          & l2b_macro_micro_desc%reflectance06(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'refl_vis008_asc',vidrefl08_l2b_asc, &
          & l2b_macro_micro_asc%reflectance08(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'refl_vis008_desc',vidrefl08_l2b_desc, &
          & l2b_macro_micro_desc%reflectance08(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'refl_vis016_asc',vidrefl16_l2b_asc, &
          & l2b_macro_micro_asc%reflectance16(:,:),1,ncols,1,nrows, verbose)
     CALL nc_write_L3_float(ncid_out,'refl_vis016_desc',vidrefl16_l2b_desc, &
          & l2b_macro_micro_desc%reflectance16(:,:),1,ncols,1,nrows, verbose)
  endif


  !-------------------------- cee -----------------------------
  CALL nc_write_L3_float(ncid_out,'cee_asc',vidceel2b_asc, &
       & l2b_macro_micro_asc%cee(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_asc_unc',vidceel2be_asc, &
       & l2b_macro_micro_asc%cee_error(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_desc',vidceel2b_desc,&
       & l2b_macro_micro_desc%cee(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_float(ncid_out,'cee_desc_unc',vidceel2be_desc,&
       & l2b_macro_micro_desc%cee_error(:,:),1,ncols,1,nrows, verbose)

  !------------------------ qcflag -----------------------------
  CALL nc_write_L3_stint_flag(ncid_out,'qcflag_asc',vidqcl2b_asc,l2b_macro_micro_asc%qcflag(:,:),1,ncols,1,nrows, verbose)
  CALL nc_write_L3_stint_flag(ncid_out,'qcflag_desc',vidqcl2b_desc,l2b_macro_micro_desc%qcflag(:,:),1,ncols,1,nrows, verbose)

  write(*,*) "before closing NetCDF file ", trim(adjustl(L3_outputpath_and_file))

  !close the file
  CALL nc_close(ncid_out,trim(adjustl(L3_outputpath_and_file)), verbose)
