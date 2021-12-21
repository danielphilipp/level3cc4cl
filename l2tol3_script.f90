! Name: l2tol3
!
!
! Purpose: Read L2 retrieval information and produce L3a/b/c statistical information and L2b composite
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
!2012/02/10 MJ includes reading of ORAC uncertainties, phase and quality flag.
!2012/02/13 MJ includes computation of uncertainty related properties, 2D histograms with additional bins and L2b branch for ORAC.
!2012/02/15 MJ includes logarithmic mean of cot.
!2012/02/17 MJ updates 2D histogram bin structure, corrected two bugs.
!2012/04/16 MJ updates filenaming,file attributes, variable names etc. according to DS-WG suggestions.
!2012/07/31 MJ implements priority sampling for L2b wrt SATZA
!2012/07/31 MJ implements ascending and descending node sampling  for L2b. Two fields of each parameter are therefore now written
!                   in the l2b output file.
!2012/08/27 MJ implements reading of L2 time information and writing of time in L2b product.
!2012/12/18 MJ corrects some minior bugs.
!2012/12/19 MJ implements variable gridspacing and horizontal subsetting.
!2012/12/19 MJ changes selection criteria for pixel wrt averaging and sampling.
!2012/12/20 MJ fixes bug wrt filter array.
!2013/01/15 MJ implements corrections according to DS-WG suggestions
!2013/01/15 MJ excludes pixels with invalid uncertainties from being used for averages etc.
!2013/01/17 MJ implements read of uncertainties of ctt and cth.
!2013/01/18 RL includes reading of FAME-C L2 data
!2013/01/23 MJ fixes potential bug in cct_std computation.
!2013/02/22 MJ fixes bug in netcdf write.
!2013/02/27 MJ includes additional cccot variable in l2b output.
!2013/04/12 CC added variables ice/liq for cot and ref
!2013/06/11 MJ implemented writing of netcdf-4 output files.
!2013/09/13 MJ fixed several problems with seperating ascending and descending nodes for L2b.
!2013/09/20 MJ changes some details of sampling for L2b pixel, selects higher quality pixels.
!2013/09/20 MJ changes computation of cloud cover l3 product: cloud cover now in sync with macro variables.
!2013/09/20 MJ adds microphysical cloud cover (product is in sync with micro variables)
!2013/09/23 MJ adds liquid and ice cloud fraction to l3 product suite
!2013/09/23 MJ adds illumination to l2b product suite
!2013/09/24 MJ adds daylight and night cloud fraction to l3 product suite
!2013/09/25 MJ re-orders logic and cleans up computation of L3 products.
!2013/09/26 SST and MJ remove bug in computing liq and ice parts of variables.
!2013/09/27 MJ implements additional cloud cover (raw) which does not have any criteria from other variables applied.
!2013/09/30 MJ implements some additional output to track coverage.
!2013/09/30 MJ changes weights from standard deviations to variances to compute error weighted means and standard deviations
!2013/10/01 MJ changes usage of cloud phase for l2b sampling.
!2013/10/02 MJ fixes potential bug with computation of error propagation products.
!2013/10/24 MJ fixes potential bug with allocation/deallocation of L2 input arrays
!2013/10/24 MJ implements experimental "node" mode for reading L2b data.
!                   This enables the code to generate L3 products from L2b products instead of L2 products.
!                   For MODIS data the resuting speed-up is 15x. Differences are subject to investigation but seem to be small.
!2013/10/24 MJ deletes obsolete CMSA branch of reading part of code.
!2013/10/28 MJ removes criteria demanding valid phase for nighttime pixels for L3 averaging.
!2014/02/21 MJ fixed allocation bug for l2 structure.
!2014/05/27 CP declared un defined variables, changed logic of read cslon etc, inserted calgo into input of sub routine
!2014/11/19 OS added variables cccot_pre, cty_pre, cty_post, cct, cct_pre, cct_post,
!  cty_pre_error, cct_error, cct_pre_error cct_post_points_raw, cct_post_points_clear_raw,
!  cct_post_raw + major code cleanup and outsourcing of branches code + removal of bug that caused extent of L3 grid
!  not to be global but rather to cover Switzerland (even though clocal = 'F')
!2015/01/23 SST included 1D histograms for L3c, removed weighted means and errors.
!2015/01/27 SST Now All (1D+2D) histogram bins follow CM SAF (Clara-A2 and Claas) definition
!2015/02/07 SST removed l2tol3_connfig.dat
!2015/02/13 OS and SST added L2B_SUM branch, allowing the parallel computation of daily sums, l3a branch is now
!  used to sum up daily sums and average the monthly mean, l3b/c are prepared for L3S computation
!2015/02/13 SST code clean up
!2015/02/27 OS and SST added flag "l3a_slow": to be set when user wishes to directly produce L3A (i.e. L3C)
!  data without intermediate production of daily sums; this is a much slower approach, especially when averaging
!  MODIS data; further changes: new method for calculating standard deviation, deactivation of remedian routines,
!  added l3_macro%cloudy_points and %cty_mean/std (i.e. cloud type) and night phase, removal of weighted means/standard deviations;
!  further removal of various superfluous variables/clean up, histogram bins now follow clara-a2 and claas definition
!  (i.e. now higher resolved) and addition of new 1D histograms, change in number of command arguments (removed
!  l2tol3configpath, l2tol3configfile, l1closure); grid resolution now only dependent on call arguments;
!  turned on internal compression of NetCDF output data; added error print statements when (de)allocating variables;
!  changed some NetCDF data definitions; removal of L3U postprocessed cloud mask and phase
!  2015/03/11 CP adds cloud albedo
!  2015/03/30 CP adds uncertainty info
!2015/04/23 OS added provisional COT cutoff, solzen, relaz, new l2b sampling approach, and cloud albedo variable IDs
!2015/07/10 OS added some variables related to time, uncertainties, and
!  num_of_orbits; added code to write lists of L2b_sum output files (only
!  executed if cinst = "Deutscher Wetterdienst"); count number of orbits
!  2015/10/20 CP CF compliance and extra comments and add sst
!  2015/12/16 OS changed variable wo to verbose; unc_std only written for L2B_SUM files
!  2015/01/04 CP bug fix removed multiply defined variables vid*cct*
!  2016/01/21 OS added two new ID variables
!  2016/02/18 OS changed time variable scale/offset to double, added new variable IDs, changed CER histogram bins
!  2016/02/23 OS increased time scale to 1000000
!  2016/02/26 OS Added cdm_data_type, naming_authority, and keywords_vocabulary to call arguments
!  2016/02/26 OS changed write_unc_std to write_l2b_sum_variables
!  2016/03/04 OS added COT histogram bin
!  2016/03/18 OS added scaling variables
!  2017/06/22 OS implemented processing of TOA/BOA fluxes and reflectance/BT data contained in postprocessed secondary files;
!    added file readability inquiries; INTERFACE CHANGES: (1) inserted l2info_secondary at argument position 5, which is a file
!    path string that can be empty if no reflectances/BTs are to be processed. file l2info_secondary is a text file that contains
!    a list of input l2 secondary files to be processed; to process refl/BTs, set argument clocal to 'T'; with clocal='T',
!    l2info_secondary is mandatory (2) inserted proc_toa_str at argument position 43; to process TOA/BOA fluxes,
!    set this string argument to '1'
!  2016/06/29 OS added cph variable IDs

! $Id$
! Bugs:
!
!none known

!Possible problems: mix of integer types, lon_grid_i etc if lat or lon are out of bounds, number of parametersin cloud mask and qa mask.
!L3d mode is highly experimental.

PROGRAM l2tol3

  use netcdf

  use vartypes

  use structures

  use make_histograms

  use nc_write_L3

  use nc_write_L3_histograms

  use calender_m

  implicit none

  character (len=paramlength) :: csensor, calgo
  character (len=filelength), allocatable, dimension(:) ::   l2files, l2files_secondary
  character (len=filelength) :: l2Toa, searchString
  character (len=currentlength) :: currentfile, currentfile_secondary, path_and_file
  character (len=500) :: s_input_dummy,comment_prop_unc,comment_corr_unc,comment_mean_unc,comment_std_unc,comment_std_unc_unc,comment_stemp
  character (len=500) :: comment_cth,comment_cth_corrected,comment_cot_log,bit_flag_vals,comment_ctp,comment_ctp_corrected,comment_ctp_log
  character (len=500) :: comment_ctt,comment_ctt_corrected
  character (len=paramlength) :: prodtype, platform
  character (len=currentlength) ::  l2info, l2info_secondary
  character (len=cpathlength) :: outputpath
  character (len=uuid_length) :: uuid_tag
  character (len=l3_outputpath_and_file_length) :: l3_outputpath_and_file
  character (len=description_length) :: reference,history,summary,keywords,comment,license,csource
  character (len=description_length) :: naming_authority,cdm_data_type,keywords_vocabulary,daystart
  character*5 :: node_suffix
  character (len=varlength), allocatable, dimension(:) :: varname_1km
  character(len=inlength) :: cgridx, cgridy,cslon,celon,cslat,celat
  character(len=attribute_length) :: cncver,ccon,cinst,l2cproc,l2cprocver,l3cproc,l3cprocver,contact,website, &
       & exec_time, prod_name,year, month, day, dom, dur, grid_type,project,cfile_version,standard_name_voc,proc_toa_str
  character*1 :: clocal

  integer :: verbose=1
  integer :: lengthPrimary, lengthSearchString
  integer (kind=lint) ::  ivar,idim,jdim,jjdim,min_satzen_line(1)
  integer (kind=lint) ::  nl2files, ind, nfiles, lastfile,ncols, nrows,nnodes,inode
  integer (kind=lint) ::  num_of_orbits
  integer (kind=stint) :: lon_offset, lat_offset, minlon,maxlon,minlat,maxlat, &
       & lat_min, lat_max, lon_min, lon_max, gridx10,gridy10,slon,elon,slat,elat
  integer (kind=lint) :: xdim1km,ydim1km, gridx, gridy
  integer(kind=stint) :: nl2vars_1km,nl2vars_errors_1km,n_val_plus_error=2,n_oe_features=3
  integer(kind=sint), allocatable, dimension(:,:) :: l2var_dummy_byte
  integer(kind=stint), allocatable, dimension(:,:) :: l2var_dummy_stint
  real(kind=sreal), allocatable, dimension(:,:) :: var2d_dummy_float
  !real(kind=dreal), allocatable, dimension(:,:) :: var2d_dummy_double
  integer(kind=lint), allocatable, dimension(:,:) :: var2d_dummy_lint
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_ctp
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_ctt
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_stemp
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_cot
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_cloud_albedo1
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_cloud_albedo2
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_ref
  integer(kind=lint), allocatable, dimension(:,:,:,:) :: var4d_dummy_lint_cwp
  integer(kind=lint), allocatable, dimension(:,:,:,:,:) :: var5d_dummy_lint
  integer(kind=lint) :: asc_counter, desc_counter
  integer :: nflag
  integer(kind=sint),allocatable, dimension(:) :: flag_val
  integer(kind=stint),allocatable, dimension(:) :: stint_flag_val
  integer(kind=lint) :: nl3x,nl3y, n_bins,lon_i,lat_j,maxi,maxj
  integer :: status

  real(kind=sreal) :: scale,offset,vmin,vmax,vmax_sq
  integer(kind=stint) :: vmin_stint, vmax_stint
  real(kind=sreal), allocatable, dimension(:,:) :: lat_1km_raw_2d, lon_1km_raw_2d, sensorzenith_1km_raw_2d
  real(kind=sreal), allocatable, dimension(:,:) :: solarzenith_1km_raw_2d, relazimuth_1km_raw_2d
  real(kind=sreal), allocatable, dimension(:) :: lat_1km_raw_1d, lon_1km_raw_1d
  real(kind=sreal), allocatable, dimension(:,:) :: l2var_dummy
  real(kind=dreal), allocatable, dimension(:,:) :: l2var_dummy_double
  integer(kind=lint) :: l1dvar_dummy

  real(kind=sreal) :: current_ctp
  real(kind=sreal) :: hist_cot(n_hist_cot), hist_ctp(n_hist_ctp)
  real(kind=sreal) :: hist_cot_1d_axis(n_cot_bins+1), hist_ctp_1d_axis(n_ctp_bins+1), hist_ctt_1d_axis(n_ctt_bins+1)
  real(kind=sreal) :: hist_ref_1d_axis(n_ref_bins+1),hist_cwp_1d_axis(n_cwp_bins+1) ,hist_cloud_albedo1_1d_axis(n_cloud_albedo1_bins+1) ,hist_cloud_albedo2_1d_axis(n_cloud_albedo2_bins+1)
  real(kind=sreal) :: hist_cot_bin(n_hist_cot-1), hist_ctp_bin(n_hist_ctp-1)
  real(kind=sreal) :: hist_cot_1d_bin(n_cot_bins),hist_ctp_1d_bin(n_ctp_bins),hist_ctt_1d_bin(n_ctt_bins)
  real(kind=sreal) :: hist_cwp_1d_bin(n_cwp_bins),hist_ref_1d_bin(n_ref_bins), hist_cloud_albedo1_1d_bin(n_cloud_albedo1_bins), hist_cloud_albedo2_1d_bin(n_cloud_albedo2_bins)
  real(kind=sreal) :: cot_max, cot_factor
  real(kind=sreal) :: random_sample

  real(kind=sreal) :: dummy_fill_value
  real(kind=dreal) :: dummy_scale, dummy_offset
  integer(kind=lint) :: dummy_vmin, dummy_vmax

  logical :: lclosure,   lrandom,lasc
  logical :: llocal, write_l2b_sum_variables
  logical :: file_exists
  logical :: proc_toa

  type(l3_micro_struct) :: l3_micro
  type(l3_macro_struct) :: l3_macro
  type(l3_rad_struct) :: l3_rad
  type(l2_input_struct_2d) :: l2_input_2d
  type(l2_input_struct_2d) :: l2b_input_2d
  type(l2b_output_struct_2d) :: l2b_macro_micro_asc,l2b_macro_micro_desc
  type(l3d_micro_struct) :: l3d_micro
  type(l3d_micro_input_struct) :: l3d_micro_input
  type(l3d_macro_struct) :: l3d_macro
  type(l3d_macro_input_struct) ::  l3d_macro_input

  ! --- netcdf related ---

  character(len=varlength) :: cdim1, cdim2
  character(len=varlength), allocatable :: dname(:)
  character(len=varlength) :: name1,name2,name
  character(len=unitlength) :: lon_unit,lat_unit, dummy_unit
  character(len=dummylength) :: cee_11um_input_string, cee_11um_uncertainty_input_string

  integer :: ncdim
  integer :: ncid, ncid_secondary, ncid_out, ierr, ncid_toa

  integer :: vid_toa_swup,vid_toa_lwup, vid_toa_swup_clr,vid_toa_lwup_clr,vid_toa_swdn
  integer :: vid_boa_swup,vid_boa_lwup, vid_boa_swup_clr,vid_boa_lwup_clr
  integer :: vid_boa_swdn,vid_boa_lwdn, vid_boa_swdn_clr,vid_boa_lwdn_clr
  integer :: vid_boa_par_dif,vid_boa_par_tot
  integer :: vid_toa_swup_low,vid_toa_lwup_low,vid_toa_swup_mid,vid_toa_lwup_mid,vid_toa_swup_hig,vid_toa_lwup_hig

  integer :: vid2bm,vid2bs,vid2bwm,vid2bws,vid2be,vid2besq,vid2bes,vid2bmed
  integer :: vid1pmic_clear, vid1pmac_clear, vid1pmic_cloudy, vid1pmac_cloudy,vid1pmac,vid1pmic, vid1pmac_stemp_clear
  integer :: vid1pmic_cloudy_liq, vid1pmic_cloudy_ice, vid1pmac_cct_cloudy, vid1pmac_cct_clear
  integer :: vid1pmic_cct_cloudy, vid1pmic_cct_clear
  integer :: vid1pmac_cct_cloudy_raw, vid1pmac_cct_clear_raw
  !   integer :: vid8m_raw,vid8m_post_raw
  integer :: vid1pmac_cct_day_cloudy,vid1pmac_cct_day_clear
  integer :: vid1pmac_cct_night_cloudy,vid1pmac_cct_night_clear
  integer :: vid1pmac_cct_twl_cloudy,vid1pmac_cct_twl_clear
  integer :: vid1pmac_cph_liq, vid1pmac_cph_ice
  integer :: vid1pmac_cct_low_cloudy, vid1pmac_cct_middle_cloudy, vid1pmac_cct_high_cloudy, vid1pmac_cee
  integer :: vid1pmac_sw, vid1pmac_lw,vid1pmac_sw_low,vid1pmac_lw_low,vid1pmac_sw_mid,vid1pmac_lw_mid,vid1pmac_sw_hig,vid1pmac_lw_hig
  integer :: vid1m,vid2m,vid3m,vid4m,vid5m,vid6m,vid7m,vid7lm,vid7lam,vid7im,vid7iam,vid8m,vid8middle,vid8low,vid8high
  integer :: vid4md,vid4sd,vid4wmd,vid4ws,vid4e,vid4wsd,vid4ed,vid4es,vid4esd
  integer :: vid8mmicro,vid8smicro,vid8mday,vid8mnight,vid8mtwl
  integer :: vid1s,vid2s,vid3s,vid4s,vid5s,vid6s,vid7s,vid7ls,vid7is, vid8s
  integer :: vid9m,vid9lm,vid9im,vid9s,vid9ls,vid9is,vid9es,vid9les,vid9ies,vid9e,vid9le,vid9ie
  integer :: vid10m,vid10lm,vid10im,vid10s,vid10ls,vid10is,vid10es,vid10les,vid10ies,vid10e,vid10le,vid10ie
  integer :: vid13m,vid13lm,vid13im,vid13s,vid13ls,vid13is,vid13es,vid13les,vid13ies,vid13e,vid13le,vid13ie
  integer :: vid14m,vid14lm,vid14im,vid14s,vid14ls,vid14is,vid14es,vid14les,vid14ies,vid14e,vid14le,vid14ie
  integer :: vid15m,vid15lm,vid15im,vid15s,vid15ls,vid15is,vid15es,vid15les,vid15ies,vid15e,vid15le,vid15ie
  integer :: vid16m,vid16lm,vid16im,vid16s,vid16ls,vid16is,vid16es,vid16les,vid16ies,vid16e,vid16le,vid16ie
  integer :: vid17m,vid17lm,vid17im,vid17s,vid17ls,vid17is,vid17es,vid17les,vid17ies,vid17e,vid17le,vid17ie
  integer :: vid18m,vid18lm,vid18im,vid18s,vid18ls,vid18is,vid18es,vid18les,vid18ies,vid18e,vid18le,vid18ie
  integer :: vid19m,vid19lm,vid19im,vid19s,vid19ls,vid19is,vid19es,vid19les,vid19ies,vid19e,vid19le,vid19ie
  integer :: vidtoa_lwupl2b_asc,vidtoa_lwup_clrl2b_asc,vidtoa_swupl2b_asc,vidtoa_swup_clrl2b_asc,vidtoa_swdnl2b_asc
  integer :: vidtoa_lwupl2b_desc,vidtoa_lwup_clrl2b_desc,vidtoa_swupl2b_desc,vidtoa_swup_clrl2b_desc,vidtoa_swdnl2b_desc
  integer :: vidboa_swupl2b_asc,vidboa_swup_clrl2b_asc,vidboa_lwupl2b_asc,vidboa_lwup_clrl2b_asc
  integer :: vidboa_swupl2b_desc,vidboa_swup_clrl2b_desc,vidboa_lwupl2b_desc,vidboa_lwup_clrl2b_desc
  integer :: vidboa_swdnl2b_asc,vidboa_swdn_clrl2b_asc,vidboa_lwdnl2b_asc,vidboa_lwdn_clrl2b_asc
  integer :: vidboa_swdnl2b_desc,vidboa_swdn_clrl2b_desc,vidboa_lwdnl2b_desc,vidboa_lwdn_clrl2b_desc
  integer :: vidboa_par_difl2b_asc,vidboa_par_totl2b_asc,vidboa_par_difl2b_desc,vidboa_par_totl2b_desc

  !   integer :: vid1med,vid2med,vid3med,vid5med,vid6med,vid7med
  !   integer :: vid9med, vid10med, vid11med, vid12med
  integer :: vid1wm,vid2wm,vid3wm,vid4wm,vid5wm,vid6wm,vid7wm,vid7lwm,vid7iwm, vid8wm
  integer :: vid9wm, vid10wm, vid11wm, vid12wm, vid13wm, vid14wm, vid15wm, vid16wm, vid17wm, vid18wm, vid19wm
  integer :: vid1ws,vid2ws,vid3ws,vid5ws,vid6ws,vid7ws,vid7lws,vid7iws, vid8ws
  integer :: vid9ws, vid10ws, vid11ws, vid12ws, vid13ws, vid14ws, vid15ws, vid16ws, vid17ws, vid18ws, vid19ws
  !   integer :: vid1esq,vid2esq,vid3esq,vid5esq,vid6esq,vid7esq,vid7lesq,vid7iesq, vid8esq
  !   integer :: vid9esq, vid10esq, vid11esq, vid12esq
  integer :: vid3log, vid6log
  integer :: vid1e,vid2e,vid3e,vid5e,vid6e,vid7e,vid7le,vid7ie, vid8e
  integer :: vid1es,vid2es,vid3es,vid5es,vid6es,vid7es,vid7les,vid7ies, vid8es
  integer :: vid1h,vid1h_cot,vid1h_ctp,vid1h_ctt,vid1h_cwp,vid1h_ref,vid1h_cloud_albedo1,vid1h_cloud_albedo2
  integer :: vid11m,vid12m,vid1h_stemp

  !cee IDs
  !L3U
  integer :: vidceel2b_asc,vidceel2be_asc,vidceel2b_desc,vidceel2be_desc
  !L3C
  integer :: vid_cee_m,vid_cee_s,vid_cee_wm,vid_cee_ws,vid_cee_e,vid_cee_es

  integer ::  vid11s, vid12s
  integer ::  vid11es, vid12es
  integer ::  vid11e, vid12e
  integer :: vid1rad,vid1taucon,vid1taucompare,vid1ppab,vid1redfac,vid1varpara
  integer :: vid0l2b_asc,vid1l2b_asc,vid1l2b_corr_asc,vid2l2b_asc,vid3l2b_asc,vid3l2b_corr_asc,&
       & vid4l2b_asc,vidcty_postl2b_asc,vid5l2b_asc,vid6l2b_asc,vid7l2b_asc,&
       & vid8l2b_asc,vid8l2be_asc,vid1pl2b_asc,vidqcl2b_asc,&
       & vidcccotl2b_asc,vidcccotl2b_desc,vidcccot_prel2b_asc, &
       & vidcccot_prel2b_desc,vidilluml2b_asc,vidilluml2b_desc, &
       & vidcct_postl2b_asc, vidcct_postl2b_desc ,vid9l2b_asc, &
       & vid10l2b_asc,vid9l2b_desc,vid10l2b_desc, &
       & vidcthl2b_asc, vidcthl2be_asc, vidcthl2b_desc, vidcthl2be_desc, &
       & vidcthcorrl2b_asc, vidcthcorrl2be_asc, vidcthcorrl2b_desc, vidcthcorrl2be_desc, &
       & vidannphase_asc, vidannphase_desc, vidannphase_unc_asc, vidannphase_unc_desc, &
       & vidcphcot_asc, vidcphcot_desc
  integer :: vid1l2be_asc,vid1l2be_corr_asc,vid2l2be_asc,vid3l2be_asc,vid3l2be_corr_asc,&
       & vid5l2be_asc,vid6l2be_asc,vid7l2be_asc,vid9l2be_asc,vid10l2be_asc
  integer :: vid0l2b_desc,vid1l2b_desc,vid1l2b_corr_desc,vid2l2b_desc,vid3l2b_desc,vid3l2b_corr_desc,&
       & vid4l2b_desc,vidcty_postl2b_desc,vid5l2b_desc,vid6l2b_desc,vid7l2b_desc, vid8l2b_desc,vid8l2be_desc,vid1pl2b_desc,vidqcl2b_desc
  integer :: vid1l2be_desc,vid1l2be_corr_desc,vid2l2be_desc,vid3l2be_desc,vid3l2be_corr_desc,&
       & vid5l2be_desc,vid6l2be_desc,vid7l2be_desc,vid9l2be_desc,vid10l2be_desc
  integer :: vidcld_type_l2b_asc, vidcld_type_l2b_desc, vidstemp_l2b_asc, vidstemp_l2be_asc
  integer :: vidstemp_l2b_desc, vidstemp_l2be_desc
  integer :: vidsatzen_l2b_asc, vidsatzen_l2b_desc
  integer :: vidsolzen_l2b_asc, vidsolzen_l2b_desc, vidrelazi_l2b_asc, vidrelazi_l2b_desc
  integer :: vidbt37_l2b_asc, vidbt37_l2b_desc, vidbt11_l2b_asc, vidbt11_l2b_desc, vidbt12_l2b_asc, vidbt12_l2b_desc
  integer :: vidrefl06_l2b_asc, vidrefl06_l2b_desc, vidrefl08_l2b_asc, vidrefl08_l2b_desc, vidrefl16_l2b_asc, vidrefl16_l2b_desc

  integer :: ndim,nvar,nattr,dummyint
  integer, allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)
  integer :: dims_var(3),dims_var_hist2d(6),dims_var_phase(3), &
       & dims_var_histcot1d(5),dims_var_histctp1d(5),dims_var_histctt1d(5),dims_var_histref1d(5),dims_var_histcwp1d(5), &
       & dims_var_histcloud_albedo11d(5), dims_var_histcloud_albedo21d(5)
  ! --- end netcdf related ---

  !---- new l2b sampling related -------
  integer:: p, q, pmax_lon, pmax_lat, i_incr, j_incr
  real(kind=dreal) :: lon_dist1,lat_dist1,lon_dist2,lat_dist2,gridx_dist,gridy_dist
  real(kind=dreal) :: time_start, time_stop, yy, mm, dd
  real(kind=dreal) :: time_scale=1.0d0/1000000.0d0
  real(kind=sreal) :: scale_100=1./100.,scale_1000=1./1000.,scale_10000=1./10000.,scale_ctp=1./50.
  real(kind=sreal) :: offset_ctp=600.,offset_temp=100.,offset_zero=0.
  real(kind=sreal) :: lon1,lon2
  integer(kind=lint) :: iidim
  logical :: found_node
  !---- end new l2b sampling related ---

  real(kind=dreal) :: cpu_start_time, cpu_finish_time
  integer(kind=lint) :: sys_count_start, sys_count_rate, sys_count_finish, sys_count_start_program, sys_count_finish_program
  integer :: cut_off, idom, foo
  character(len=8) :: yyyymmdd, cday, cplatform
  character(len=1024) :: L2b_sum_file, L2b_sum_output_directory, cut_string, L3_output_directory, path, generic_suite_directory, L2B_SUM_ncfile_list, list_folder, list_file_full_path

  !MST
  real(kind=dreal) :: sol_lat, t0, t1, coszenith, icoszenith, izenith, angcorr, irad1, irad_fact
  real(kind=dreal) :: ltime, tmp_mst, tslope, sslope, path1, ipath
  integer(kind=lint) :: jcount
  real(kind=dreal) :: jrad_boa_swdn, jrad_boa_swdn_clr, jrad_boa_swup, jrad_boa_swup_clr,jrad_toa_swdn, jrad_toa_swup, jrad_toa_swup_clr
  real(kind=dreal) :: ext_boa_swdn, ext_boa_swdn_clr, alb_boa_sw, alb_boa_sw_clr, alb_toa_sw, alb_toa_sw_clr
  real(kind=dreal) :: ialb_boa_sw, ialb_boa_sw_clr, ialb_toa_sw, ialb_toa_sw_clr
  real(kind=dreal) :: dcc_raw,dcc_toa_lwup,dcc_toa_lwup_clr,dcc_boa_lwup,dcc_boa_lwup_clr
  integer(kind=2) :: iday,imonth,iyear,idoy
  integer :: idilon
  
  EXTERNAL funcs

  write(*,*) "l2tol3script started"

  ! be more verbose
  verbose = 0

  !set number of nodes for "normal" processing
  nnodes=1

  !turn random sampling of permanently
  lrandom=.false.

  ! Hardwired boundaries and bins of histograms
  ! new use clara-a2 and claas definition
  ! 2d histogram
  hist_cot=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, 9.4, 15.0, 23.0, 41.0, 60.0, 80.0, 100. /)
  !  hist_cloud_albedo=(/0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.9, 1.0 /)
  hist_ctp=(/ 1.0, 90.0, 180.0, 245.0, 310.0, 375.0, 440.0, 500.0, 560.0, 620.0, 680.0, 740.0, 800.0, 875.0, 950.0, 1100. /)
  hist_cot_bin=hist_cot(1:n_hist_cot-1)*0.5+hist_cot(2:n_hist_cot)*0.5
  !  hist_cloud_albedo_bin=hist_cloud_albedo(1:n_hist_cloud_albedo-1)*0.5+hist_cloud_albedo(2:n_hist_cloud_albedo)*0.5
  hist_ctp_bin=hist_ctp(1:n_hist_ctp-1)*0.5+hist_ctp(2:n_hist_ctp)*0.5

  !1d histograms
  hist_cot_1d_axis=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, 9.4, 15.0, 23.0, 41.0, 60.0, 80.0, 99.99, 1000. /)
  hist_cot_1d_bin=hist_cot_1d_axis(1:n_cot_bins)*0.5+hist_cot_1d_axis(2:n_cot_bins+1)*0.5

  hist_cloud_albedo1_1d_axis=(/0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.9, 1.0 /)
  hist_cloud_albedo1_1d_bin=hist_cloud_albedo1_1d_axis(1:n_cloud_albedo1_bins)*0.5+hist_cloud_albedo1_1d_axis(2:n_cloud_albedo1_bins+1)*0.5

  hist_cloud_albedo2_1d_axis=(/0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75,0.8, 0.9, 1.0 /)
  hist_cloud_albedo2_1d_bin=hist_cloud_albedo2_1d_axis(1:n_cloud_albedo2_bins)*0.5+hist_cloud_albedo2_1d_axis(2:n_cloud_albedo2_bins+1)*0.5

  hist_ctp_1d_axis=(/ 1.0, 90.0, 180.0, 245.0, 310.0, 375.0, 440.0, 500.0, 560.0, 620.0, 680.0, 740.0, 800.0, 875.0, 950.0, 1100.0 /)
  hist_ctp_1d_bin=hist_ctp_1d_axis(1:n_ctp_bins)*0.5+hist_ctp_1d_axis(2:n_ctp_bins+1)*0.5

  hist_ctt_1d_axis=(/ 200.0, 210.0, 220.0, 230.0, 235.0, 240.0, 245.0, 250.0, 255.0, 260.0, 265.0, 270.0, 280.0, 290.0, 300.0, 310.0, 350.0 /)
  hist_ctt_1d_bin=hist_ctt_1d_axis(1:n_ctt_bins)*0.5+hist_ctt_1d_axis(2:n_ctt_bins+1)*0.5

  hist_cwp_1d_axis=(/ 0.0, 5.0, 10.0, 20.0, 35.0, 50.0, 75.0, 100.0, 150.0, 200.0, 300.0, 500.0, 1000.0, 2000.0, 100000.0 /)
  hist_cwp_1d_bin=hist_cwp_1d_axis(1:n_cwp_bins)*0.5+hist_cwp_1d_axis(2:n_cwp_bins+1)*0.5

  hist_ref_1d_axis=(/ 0.0, 3.0, 6.0, 9.0, 12.0, 15.0, 20.0, 25.0, 30.0, 40.0, 60.0, 80.0 /)
  hist_ref_1d_bin=hist_ref_1d_axis(1:n_ref_bins)*0.5+hist_ref_1d_axis(2:n_ref_bins+1)*0.5

  ! Read variables from command line call which was executed by surrounding script.
  ! ---------------------------------

  CALL GET_COMMAND_ARGUMENT(1,prodtype)
  prodtype=trim(adjustl(prodtype))
  CALL GET_COMMAND_ARGUMENT(2,csensor)
  csensor=trim(adjustl(csensor))
  CALL GET_COMMAND_ARGUMENT(3,calgo)
  calgo=trim(adjustl(calgo))
  CALL GET_COMMAND_ARGUMENT(4,l2info)
  l2info=trim(adjustl(l2info))
  CALL GET_COMMAND_ARGUMENT(5,l2info_secondary)
  l2info_secondary=trim(adjustl(l2info_secondary))
  CALL GET_COMMAND_ARGUMENT(6,l3_outputpath_and_file)
  l3_outputpath_and_file=trim(adjustl(l3_outputpath_and_file))
  CALL GET_COMMAND_ARGUMENT(7,cgridx)
  CALL GET_COMMAND_ARGUMENT(8,cgridy)
  CALL GET_COMMAND_ARGUMENT(9,uuid_tag)
  CALL GET_COMMAND_ARGUMENT(10,platform)
  CALL GET_COMMAND_ARGUMENT(11,exec_time)
  CALL GET_COMMAND_ARGUMENT(12,prod_name)
  CALL GET_COMMAND_ARGUMENT(13,year)
  CALL GET_COMMAND_ARGUMENT(14,month)
  CALL GET_COMMAND_ARGUMENT(15,day)
  CALL GET_COMMAND_ARGUMENT(16,cncver)
  CALL GET_COMMAND_ARGUMENT(17,ccon)
  CALL GET_COMMAND_ARGUMENT(18,cinst)
  CALL GET_COMMAND_ARGUMENT(19,l2cproc)
  CALL GET_COMMAND_ARGUMENT(20,l2cprocver)
  CALL GET_COMMAND_ARGUMENT(21,l3cproc)
  CALL GET_COMMAND_ARGUMENT(22,l3cprocver)
  CALL GET_COMMAND_ARGUMENT(23,contact)
  CALL GET_COMMAND_ARGUMENT(24,website)
  CALL GET_COMMAND_ARGUMENT(25,grid_type)
  CALL GET_COMMAND_ARGUMENT(26,reference)
  CALL GET_COMMAND_ARGUMENT(27,history)
  CALL GET_COMMAND_ARGUMENT(28,summary)
  CALL GET_COMMAND_ARGUMENT(29,keywords)
  CALL GET_COMMAND_ARGUMENT(30,comment)
  CALL GET_COMMAND_ARGUMENT(31,project)
  CALL GET_COMMAND_ARGUMENT(32,cfile_version)
  CALL GET_COMMAND_ARGUMENT(33,csource)
  CALL GET_COMMAND_ARGUMENT(34,dom)
  CALL GET_COMMAND_ARGUMENT(35,dur)
  CALL GET_COMMAND_ARGUMENT(36,license)
  CALL GET_COMMAND_ARGUMENT(37,standard_name_voc)
  CALL GET_COMMAND_ARGUMENT(38,clocal)
  CALL GET_COMMAND_ARGUMENT(39,cslon)
  CALL GET_COMMAND_ARGUMENT(40,celon)
  CALL GET_COMMAND_ARGUMENT(41,cslat)
  CALL GET_COMMAND_ARGUMENT(42,celat)
  CALL GET_COMMAND_ARGUMENT(43,proc_toa_str)
  CALL GET_COMMAND_ARGUMENT(44,cdm_data_type)
  CALL GET_COMMAND_ARGUMENT(45,naming_authority)
  CALL GET_COMMAND_ARGUMENT(46,keywords_vocabulary)

  !MST===
  read(day,*) iday
  read(month,*) imonth
  read(year,*) iyear
  call GREG2DOY(iyear, imonth, iday, idoy)
  !===
  
  !if (verbose.eq.1)  then
     write(*,*) 'prodtype = ',prodtype
     write(*,*) 'csensor = ',csensor
     write(*,*) "calgo = ", calgo
     write(*,*) 'l2info = ',trim(adjustl(l2info))
     write(*,*) 'l2info_secondary = ',trim(adjustl(l2info_secondary))
     write(*,*) 'l3_outputpath_and_file = ',trim(adjustl(l3_outputpath_and_file))
     write(*,*) "cgridx = ", cgridx
     write(*,*) "cgridy = ", cgridy
     write(*,*) "uuid_tag = ", uuid_tag
     write(*,*) "platform = ", platform
     write(*,*) "exec_time = ", exec_time
     write(*,*) "prod_name = ", prod_name
     write(*,*) "year = ", year
     write(*,*) "month = ", month
     write(*,*) "day = ", day
     write(*,*) "cncver = ", cncver
     write(*,*) "ccon = ", ccon
     write(*,*) "cinst = ", cinst
     write(*,*) "l2cproc = ", l2cproc
     write(*,*) "l2cprocver = ", l2cprocver
     write(*,*) "l3cproc = ", l3cproc
     write(*,*) "l3cprocver = ", l3cprocver
     write(*,*) "contact = ", contact
     write(*,*) "website = ", website
     write(*,*) "grid_type = ", grid_type
     write(*,*) "reference = ", trim(adjustl(reference))
     write(*,*) "history = ", trim(adjustl(history))
     write(*,*) "summary = ", trim(adjustl(summary))
     write(*,*) "keywords = ", trim(adjustl(keywords))
     write(*,*) "comment = ", trim(adjustl(comment))
     write(*,*) 'project = ',project
     write(*,*) "cfile_version = ", cfile_version
     write(*,*) "csource = ", trim(adjustl(csource))
     write(*,*) "dom = ", dom
     write(*,*) "dur = ", dur
     write(*,*) "license = ", trim(adjustl(license))
     write(*,*) "standard_name_voc = ", standard_name_voc
     write(*,*) "clocal = ", clocal
     write(*,*) "cslon = ", cslon
     write(*,*) "celon = ", celon
     write(*,*) "cslat = ", cslat
     write(*,*) "celat = ", celat
     write(*,*) "proc_toa_str = ",  proc_toa_str
     write(*,*) "cdm_data_type = ", cdm_data_type
     write(*,*) "naming_authority = ", naming_authority
     write(*,*) "keywords_vocabulary = ", keywords_vocabulary

  !endif
  
  if (proc_toa_str(:1) == "1") then
     proc_toa = .true.
  else
     proc_toa = .false.
  endif

  if ( (cinst .eq. "Deutscher Wetterdienst") .and. (trim(adjustl(prodtype)) .eq. "l2b_sum") ) then

     ! cut off L2B_sum file name
     cut_off = index(trim(adjustl(l3_outputpath_and_file)),'/',BACK=.true.)
     L2b_sum_file = trim(adjustl(l3_outputpath_and_file(cut_off+1:len_trim(l3_outputpath_and_file))))
     L2b_sum_file = L2b_sum_file(9:len_trim(L2b_sum_file))
     cut_string = trim(adjustl(l3_outputpath_and_file(1:cut_off-1)))

     ! cut off L2B_sum output folder
     cut_off = index(trim(adjustl(cut_string)),'/',BACK=.true.)
     L2B_sum_output_directory = trim(adjustl(cut_string(cut_off+1:len_trim(cut_string))))
     L2b_sum_output_directory = L2b_sum_output_directory(9:len_trim(L2b_sum_output_directory))
     L3_output_directory = trim(adjustl(cut_string(1:cut_off)))

     ! cut off generic output folder path
     cut_off = index(trim(adjustl(cut_string)),'ECFlow_CC4CL_proc',BACK=.true.)
     cut_off = cut_off + len_trim('ECFlow_CC4CL_proc')
     generic_suite_directory = trim(adjustl(cut_string(1:cut_off-1)))

     ! convert dom from character to integer
     read(dom, '(I2)') idom

     ! write list file only if day = 01 to avoid redundant writing
     if ( day(1:2) == "01" ) then

        write(*,*) "writing list of L2B_SUM NetCDF output paths and files for subsequent L3C processing"

        ! for NOAA, remove hyphen from platform name, e.g. NOAA-18 => NOAA18
        if ( platform(1:4) == "NOAA" ) then
           cplatform = platform(1:4) // platform(6:7)
        else
           cplatform = platform
        endif

        ! build temp_list_l2files path and create list file name
        L2B_SUM_ncfile_list = "L2B_SUM_ncfile_list_l3a_" // trim(adjustl(csensor)) &
             // "_" // trim(adjustl(cplatform)) // "_" // trim(adjustl(year)) // "_" // trim(adjustl(month)) // ".txt"
        list_folder = trim(adjustl(generic_suite_directory)) // "/temp_list_l2files/"
        list_file_full_path = trim(adjustl(list_folder)) // trim(adjustl(L2B_SUM_ncfile_list))

        ! DP
        !open(11,file=trim(adjustl(list_file_full_path)), status='replace')

        ! write L2B_SUM file path for each day to list file
        do foo = 1, idom

           ! convert foo from integer to character
           write( cday, '(I2)' ) foo
           if (foo .lt. 10) cday = "0" // trim(adjustl(cday))
           yyyymmdd = trim(adjustl(year)) // trim(adjustl(month)) // trim(adjustl(cday))

           ! build final output path and write to file
           path = """" // trim(adjustl(L3_output_directory)) // trim(adjustl(yyyymmdd)) &
                // trim(adjustl(L2B_sum_output_directory)) // "/" // trim(adjustl(yyyymmdd)) &
                // trim(adjustl(L2b_sum_file)) // """"
           ! DP
           !write(11,*) trim(adjustl(path))

        enddo
        ! DP
        !close(11)

     endif

  endif

  write(*,*) cslon, celon, cslat, cslon

  read(cgridx, '(I3)') gridx
  read(cgridy, '(I3)') gridy
  read(cslon, '(I4)') slon
  read(celon, '(I4)') elon
  read(cslat, '(I4)') slat
  read(celat, '(I4)') elat

  ! If = true: closure (radiative averaging) is carried out
  lclosure=.false.

  if(clocal .eq. 'T') llocal=.true.
  if(clocal .eq. 'F') llocal=.false.

  !this is only for the global L2b and L3 products.
  !Settings for local products are put in place in the *.ksh script
  if(.not. llocal) then
     write(*,*) 'clocal: false'
     slon=-180
     elon=180
     slat=-90
     elat=90
  endif

  !====
  !MST: set llocal to true to get the reflectances/Tbs as output
  if(trim(adjustl(prodtype)) .eq. 'l2b' .or. trim(adjustl(prodtype)) .eq. 'l2b_sum') llocal=.true.
  
  !====
  
  minlon=slon
  maxlon=elon
  minlat=slat
  maxlat=elat

  !those two give the total coverage of the grid in degress
  lon_offset=abs(slon-elon)
  lat_offset=abs(slat-elat)

  !Open list of L2/L2b_sum files
  !---------------------------------
  write(*,*)'l2info = ',trim(l2info)
  open(30,file=trim(adjustl(l2info)),status='old')
  if (llocal) then
     write(*,*)'l2info_secondary = ',trim(l2info_secondary)
     open(31,file=trim(adjustl(l2info_secondary)),status='old')
  endif
  nfiles = 0
  do
     read(30,*,iostat=ierr) s_input_dummy
     if (ierr/=0) exit
     nfiles = nfiles + 1
  end do
  rewind(30)
  allocate(l2files(nfiles))
  l2files=''
  if (llocal) then
     allocate(l2files_secondary(nfiles))
     l2files_secondary=''
  endif
  !------------------------------------------------------------

  !use this branch if L3a/b/c is required or other one further down if the sampling L2b is required.
  if(trim(adjustl(prodtype)) .eq. 'l2b' .or. trim(adjustl(prodtype)) .eq. 'l3d' ) then

     ! this is currently NEVER set to TRUE; lclosure is set to FALSE above as well
     lclosure=.false.

  endif

  !Build now a global grid for all further use
  !-----------------------------------------------
  nrows=abs((elat-slat)*gridy)
  write(*,*) 'nrows',nrows, 'delgridy',gridy
  ncols=abs((elon-slon)*gridx)
  write(*,*) 'ncols',ncols, 'delgridx',gridx

  ! this is somehow necessary for the compression of the ncdf4 files
  !setting the chunksize for the 2D cot-ctp histogram
  chunksize6d=(/int(ncols,kind=lint),int(nrows,kind=lint),int(n_hist_cot-1,kind=lint),  &
       & int(n_hist_ctp-1,kind=lint),int(n_hist_phase,kind=lint),int(1,kind=lint)/)

  !Branch for L3a/b/c production - probably all refer to the same product (i.e. L3C)?
  if(trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. trim(adjustl(prodtype)) .eq. 'l3a' .or. &
       & trim(adjustl(prodtype)) .eq. 'l3b' .or. trim(adjustl(prodtype)) .eq. 'l3c' .or. &
       & trim(adjustl(prodtype)) .eq. 'l3a_slow') then

     write(*,*) "setting up structures for L3a/b/c or L2B_SUM production"
     call set_l3_macro_struct(l3_macro,ncols,nrows,calgo)
     call set_l3_micro_struct(l3_micro,ncols,nrows,calgo)

     !Branch for L2b composite product
  elseif(trim(adjustl(prodtype)) .eq. 'l2b' ) then

     write(*,*) "setting up structures for L2b production"
     call set_l2b_macro_micro_struct(l2b_macro_micro_asc,ncols,nrows,calgo,llocal)
     call set_l2b_macro_micro_struct(l2b_macro_micro_desc,ncols,nrows,calgo,llocal)

     !Branch for experimental L3d (monthly means from daily means) product
  elseif( trim(adjustl(prodtype)) .eq. 'l3d') then

     write(*,*) "setting up structures for L3d production"
     call set_l3d_macro_struct(l3d_macro,ncols,nrows)
     call set_l3d_micro_struct(l3d_micro,ncols,nrows)

  endif

  !Start loop over files which are to be processed
  !-------------------------------------------------------
  lastfile=nfiles

  if( trim(adjustl(prodtype)) .eq. 'l2b' .or. trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. &
       & trim(adjustl(prodtype)) .eq. 'l3a_slow' .or. trim(adjustl(prodtype)) .eq. 'l3d') then
     num_of_orbits=nfiles
  else
     num_of_orbits=0
  endif

  searchString = "L2_CLOUD-CLD_PRODUCTS"
  lengthSearchString = len(trim(adjustl(searchString)))

  do ind = 1, lastfile

     read(30,*) l2files(ind)
     write(*,*)  'l2files(ind) ', l2files(ind)

     ! primary
     currentfile=trim(adjustl(l2files(ind)))
     write(*,*) 'FILE',ind,'(',nfiles,') ',trim(adjustl(currentfile))
     write(*,*)'currentfile ',currentfile

     inquire(file = trim(adjustl(currentfile)), exist = file_exists, iostat = status)
     if (status == 0) then
        write(*,*) "File ", trim(adjustl(currentfile)), " exists? ", file_exists
        if (.not. file_exists) cycle
     else
        write(*,*) "Inquire error for file ", trim(adjustl(currentfile))
        stop
     endif
     !open L2 input file and read dimension information
     call nc_open(ncid,currentfile,ierr,verbose)
     call nc_info(ncid,ndim,nvar,nattr,verbose)

     ! TOA
     if (proc_toa) then
        lengthPrimary = len(trim(adjustl(currentfile)))
        cut_off = index(currentfile, trim(adjustl(searchString)), BACK=.true.)
        l2Toa = currentfile(1:(cut_off-1)) // "TOA" // currentfile((cut_off+lengthSearchString):lengthPrimary)
        cut_off = index(l2Toa, " ")
        l2Toa = trim(adjustl(l2Toa(1:cut_off-1)))
     
        if (trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. trim(adjustl(prodtype)) .eq. 'l2b') then
           inquire(file = trim(adjustl(l2Toa)), exist = file_exists, iostat = status)
           if (status == 0) then
              write(*,*) "Toa file ", trim(adjustl(l2Toa)), " exists? ", file_exists
              if(.not. file_exists) cycle
           else
              write(*,*) "Inquire error for toa file ", trim(adjustl(l2Toa))
              stop
           endif
           !open L2Toa input file and read dimension information
           call nc_open(ncid_toa,L2Toa,ierr,verbose)
        endif
     endif

     ! secondary
     if (llocal) then
        read(31,*) l2files_secondary(ind)
        write(*,*)  'l2files_secondary(ind) ',l2files_secondary(ind)
        currentfile_secondary=trim(adjustl(l2files_secondary(ind)))
        write(*,*) 'secondary FILE',ind,'(',nfiles,') ',trim(adjustl(currentfile_secondary))
        write(*,*)'currentfile_secondary ',currentfile_secondary
        !open L2 input file and read dimension information
        call nc_open(ncid_secondary,currentfile_secondary,ierr,verbose)
     endif

     allocate(dimids(ndim))
     dimids=0
     allocate(dname(ndim))
     dname=''
     allocate(dimlength(ndim))
     dimlength=0
     allocate(varids(nvar))
     varids=0
     allocate(attrids(nattr))
     attrids=0

     write(*,*)'prodtype ' ,trim(adjustl(prodtype))

     !L3 branch read l2 input files
     if( trim(adjustl(prodtype)) .eq. 'l2b' .or. trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. &
          & trim(adjustl(prodtype)) .eq. 'l3a_slow') then

        if(trim(adjustl(calgo)) .eq. 'ORAC') then

           write(*,*) "reading L2 input data for ORAC branch"
           include "read_L2_input_ORAC.inc"

           ! "node" mode: Reads l2b (L3U) instead of L2 file to produce L3C
        elseif(trim(adjustl(calgo)) .eq. 'ORAC_L2B') then

           write(*,*) "reading L2 input data for ORAC_L2B branch"
           include "read_L2_input_ORAC_L2B.inc"

        elseif(trim(adjustl(calgo)) .eq. 'FAME-C') then

           write(*,*) "reading L2 input data for FAME-C branch"
           include "read_L2_input_FAME-C.inc"

        endif

        !L3d branch
     elseif(trim(adjustl(prodtype)) .eq. 'l3d') then

        write(*,*) "reading L2 input data for L3d branch"
        include "read_L2_input_L3d.inc"

     elseif( trim(adjustl(prodtype)) .eq. 'l3a' .or. trim(adjustl(prodtype)) .eq. 'l3b' .or. &
          & trim(adjustl(prodtype)) .eq. 'l3c' ) then

        write(*,*) "reading L2B_SUM input data for L3a/b/c branch and add to micro/macro - structures"
        include "read_L2B_SUM_ORAC.inc"

     endif

     !close input file
     call nc_close(ncid,currentfile,verbose)
     if (llocal) call nc_close(ncid_secondary,currentfile_secondary,verbose)
     if (trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. trim(adjustl(prodtype)) .eq. 'l2b') then
        if (proc_toa) then
           call nc_close(ncid_toa,L2Toa,verbose)
        endif
     endif
     !L3 branch
     if ( trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. trim(adjustl(prodtype)) .eq. 'l3a_slow' ) then

        write(*,*) "L2B_SUM: prepare L3a/b/c averaging (L3C/S(sensor family)/S(all))"
        include "prepare_L3abc_averaging.inc"

        !make the composite based on unbiased random algorithm or with minimal satzen
        !(random part is no longer maintained as of September 2013)
     elseif(trim(adjustl(prodtype)) .eq. 'l2b' ) then
        
        write(*,*) "start L2b sampling (L3U)"
        include "L2b_sampling.inc"

     endif

     write(*,*) "end of L3 sampling/averaging"

     deallocate(dimids)
     deallocate(dname)
     deallocate(dimlength)
     deallocate(varids)
     deallocate(attrids)

     !deallocate L2 input arrays
     if( ( trim(adjustl(prodtype)) .eq. 'l2b_sum' .or. trim(adjustl(prodtype)) .eq. 'l2b' .or. &
          & trim(adjustl(prodtype)) .eq. 'l3a_slow')  .and. trim(adjustl(calgo)) .ne. 'ORAC_L2B' ) then

        write(*,*) "deallocate L2 input arrays"
        call unset_l2_input_struct_2d(l2_input_2d,calgo,llocal)

     endif

     !end of the loop over the files
  enddo

  deallocate(l2files)

  write(*,*) "reading of L2 files finished, closing inventory file"
  close(30)
  if (llocal) close(31)

  if(trim(adjustl(prodtype)) .eq. 'l2b_sum' ) then

     ! write the sums to output netcdf file
     write(*,*) "write L2B_SUM to output"
     write_l2b_sum_variables = .true.
     include "write_L3abc_data.inc"

  elseif(trim(adjustl(prodtype)) .eq. 'l3a' .or. trim(adjustl(prodtype)) .eq. 'l3b' .or. &
       & trim(adjustl(prodtype)) .eq. 'l3c' .or. trim(adjustl(prodtype)) .eq. 'l3a_slow' ) then

     !---------------------------------------------
     !---------------------------------------------
     !Loop now over the complete L3 grid and do the final steps for the averaging.
     !---------------------------------------------
     !---------------------------------------------
     write(*,*) "looping over the complete L3 grid (only L3a/b/c)"
     include "L3abc_averaging.inc"

     !include code that writes the output to netcdf file
     write(*,*) "write L3a/b/c (=L3C/S/S) output"
     write_l2b_sum_variables = .false.
     include "write_L3abc_data.inc"

     !experimental L3d product
  elseif( trim(adjustl(prodtype)) .eq. 'l3d' ) then

     write(*,*) "looping over the complete L3 grid (only L3d)"
     include "L3d_averaging.inc"

  elseif( trim(adjustl(prodtype)) .eq. 'l2b' ) then

     write(*,*) "write L2b (= L3U) output"
     include "write_L2b_data.inc"

  endif

end PROGRAM l2tol3
