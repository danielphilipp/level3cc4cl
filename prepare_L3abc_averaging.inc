  ! Name: prepare_L3abc_averaging.inc
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
  !2015-03-30 CP add uncertainty info
  !2015/04/23 OS implemented COT cutoff at 100, scaling CWP accordingly; possibly to be replaced with weighted means approach
  !2015-07-07 CP add corrected cloud top height
  !2015/07/10 OS avoid counting fill values for cct; some clean up
  !2015/07/10 CP adds in post flag
  !2015/07/25 CP bug fix phase was set to pavolinis flag not cloud phase liq/ice
  !2015/09/02 OS undo previous bug fix by CP; variable cty refers to Pavolonis phase
  !2015/10/27 CP add in stemp
  !2015/12/16 OS added cloud albedo and cth error availability to averaging constraints;
  !              updated convergence/max cost requirements according to
  !              changes in qcflag
  !2016/02/18 OS added CTT_corrected, CTP_corrected, CEE
  !2016/02/26 OS added CEE counter
  !2017/06/22 OS Added toa/boa, nretr_clear, and stemp_cloudy/clear
  !2017/06/29 OS Added cph alias cty
  !2018/01/02 MST: Including fix for toa_swup fluxes in twilight conditions based on reflectance data
  !2019/09/12 DP: Do not apply radiation flux DCC if sensor is SEVIRI (geostationary)
  !               due to high temporal sampling rate and good representation of the DC
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known

  !start normal averaging
  !begin with gridding of the present files
  !on equal angle grid

  maxi=lon_offset*gridx
  maxj=lat_offset*gridy

  do idim=1,xdim1km
     do jdim=1,ydim1km

        !look if geolocation information exist, if not do nothing and go to next L2 pixel
        if(l2_input_2d%lat(idim,jdim) .ge. (real_fill_value+1.0) .and. &
             & l2_input_2d%lon(idim,jdim) .ge. (real_fill_value+1.0) .and. &
             & l2_input_2d%lat(idim,jdim) .ge. float(slat) .and. &
             & l2_input_2d%lat(idim,jdim) .le. float(elat) .and. &
             & l2_input_2d%lon(idim,jdim) .ge. float(slon) .and. &
             & l2_input_2d%lon(idim,jdim) .le. float(elon) ) then

           !find grid cell coordinates into which L2 pixel falls
           lon_i=int((l2_input_2d%lon(idim,jdim)+(-1.0)*slon)*gridx)+1
           lat_j=int((l2_input_2d%lat(idim,jdim)+(-1.0)*slat)*gridy)+1
           if(lon_i .gt. maxi) lon_i=1
           if(lat_j .gt. maxj) lat_j=maxj

        else

           lon_i=long_int_fill_value
           lat_j=long_int_fill_value

           cycle

        endif

        !loop over nodes
        do inode=1,nnodes
        if(l2_input_2d%cct(idim,jdim,inode) .ge. 0 .and. l2_input_2d%cct(idim,jdim,inode) .le. 1.0) then ! if valid CMA 
           !just use this counter to register coverage of raw overpass pixels
           l3_macro%points(lon_i,lat_j)=l3_macro%points(lon_i,lat_j)+1
           if(l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint) l3_micro%points(lon_i,lat_j)=l3_micro%points(lon_i,lat_j)+1

           ! start of cct
           !cloud cover without any criteria to the parameters applied (this is NOT the official cloud cover)
           if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5) then

              l3_macro%cct_points_raw(lon_i,lat_j)=l3_macro%cct_points_raw(lon_i,lat_j)+1

           elseif( l2_input_2d%cct(idim,jdim,inode) .le. 0.5 .and. l2_input_2d%cct(idim,jdim,inode) .ge. 0.0) then

              l3_macro%cct_points_clear_raw(lon_i,lat_j)=l3_macro%cct_points_clear_raw(lon_i,lat_j)+1

           endif

           !TOA, withour any citeria to parameters nor daylight
           if (proc_toa) then

              jcount=0
              
              if( l2_input_2d%toa_swdn(idim,jdim,inode) .ge. 0. .and. &
                   l2_input_2d%toa_swup(idim,jdim,inode) .ge. 0. .and. l2_input_2d%toa_swup_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%toa_lwup(idim,jdim,inode) .ge. 0. .and. l2_input_2d%toa_lwup_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%boa_swup(idim,jdim,inode) .ge. 0. .and. l2_input_2d%boa_swup_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%boa_lwup(idim,jdim,inode) .ge. 0. .and. l2_input_2d%boa_lwup_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%boa_swdn(idim,jdim,inode) .ge. 0. .and. l2_input_2d%boa_swdn_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%boa_lwdn(idim,jdim,inode) .ge. 0. .and. l2_input_2d%boa_lwdn_clr(idim,jdim,inode) .ge. 0. .and. &
                   & l2_input_2d%boa_par_dif(idim,jdim,inode) .ge. 0. .and. l2_input_2d%boa_par_tot(idim,jdim,inode) .ge. 0. ) then
!                  & l2_input_2d%cct(idim,jdim,inode) .lt. 0.5 ) then

                 if( (l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                      (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. &
                      l2_input_2d%solzen(idim,jdim) .lt. 89. ) .or. &
                     csensor .eq. "SEVIRI") then

!                   if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint .and. l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
!                     l2_input_2d%reflectance06(idim,jdim,inode) .gt. 0. .and. l2_input_2d%reflectance06(idim,jdim,inode) .lt. 1.) then
!                     !approximate TOA_SWUP fluxes in cloudy twi-light conditions as the cloud properties derived in these conditions are just the FG values, thus not reliable
!                     tmp_mst=l2_input_2d%toa_swup(idim,jdim,inode)
!                     l2_input_2d%toa_swup(idim,jdim,inode)=l2_input_2d%reflectance06(idim,jdim,inode)*100.*16. !*9.07416+114.467  !*15.
!                     !print*,l2_input_2d%boa_swdn(idim,jdim,inode),tmp_mst,l2_input_2d%toa_swup(idim,jdim,inode)
!                     l2_input_2d%boa_swdn(idim,jdim,inode)=l2_input_2d%boa_swdn(idim,jdim,inode)*min(5.,max(0.2,tmp_mst/l2_input_2d%toa_swup(idim,jdim,inode)))
!                   endif

                   t0        = (90.-l2_input_2d%lat(idim,jdim))*3.14/180.
                   sol_lat   = -cos(d2r*((idoy+10.)*360./365.))*23.5  !21.4408  for 2008/7/15
                   t1        = (90.-sol_lat)*3.14/180
                   coszenith = cos(t0)*cos(t1)+sin(t0)*sin(t1)*cos(0.)

                   path1 = 1.0*35./sqrt(1224.*(cos(d2r*l2_input_2d%solzen(idim,jdim)))**2.+1)
        
                   !Caluclate TOA to BOA extinction
                   ext_boa_swdn     = alog(max(0.05,min(1.,l2_input_2d%boa_swdn(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode))))*(-1.0)
                   ext_boa_swdn_clr = alog(max(0.05,min(1.,l2_input_2d%boa_swdn_clr(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode))))*(-1.0)

                   if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint) then
                     ext_boa_swdn     = ext_boa_swdn + 0.05*cos(d2r*l2_input_2d%lat(idim,jdim))
                     ext_boa_swdn_clr = ext_boa_swdn_clr + 0.05*cos(d2r*l2_input_2d%lat(idim,jdim))
                   else
                     ext_boa_swdn     = ext_boa_swdn + 0.02 !*cos(d2r*l2_input_2d%lat(idim,jdim))
                     ext_boa_swdn_clr = ext_boa_swdn_clr + 0.02 !*cos(d2r*l2_input_2d%lat(idim,jdim))
                   endif
                   
!print*,l2_input_2d%solzen(idim,jdim)
!print*,0.05/cos(d2r*l2_input_2d%solzen(idim,jdim))
!print*, l2_input_2d%boa_swdn(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode)
!print*,l2_input_2d%boa_swdn_clr(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode)
!print*,ext_boa_swdn,log(max(0.05,min(1.,l2_input_2d%boa_swdn(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode))))*(-1.0)
!print*,ext_boa_swdn_clr,log(max(0.05,min(1.,l2_input_2d%boa_swdn_clr(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode))))*(-1.0)
!stop

                   jrad_boa_swdn=0.
                   jrad_boa_swdn_clr=0.
                   jrad_boa_swup=0.
                   jrad_boa_swup_clr=0.
                   jrad_toa_swdn=0.
                   jrad_toa_swup=0.
                   jrad_toa_swup_clr=0.
                   alb_boa_sw     = max(0.0,min(1.0,l2_input_2d%boa_swup(idim,jdim,inode)/l2_input_2d%boa_swdn(idim,jdim,inode)))
                   alb_boa_sw_clr = max(0.0,min(1.0,l2_input_2d%boa_swup_clr(idim,jdim,inode)/l2_input_2d%boa_swdn_clr(idim,jdim,inode)))
                   alb_toa_sw     = max(0.0,min(1.0,l2_input_2d%toa_swup(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode)))
                   alb_toa_sw_clr = max(0.0,min(1.0,l2_input_2d%toa_swup_clr(idim,jdim,inode)/l2_input_2d%toa_swdn(idim,jdim,inode)))
                                      

                   if (csensor .ne. "SEVIRI") then
                    if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint .and. l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
                      l2_input_2d%reflectance06(idim,jdim,inode) .gt. 0. .and. l2_input_2d%reflectance06(idim,jdim,inode) .lt. 1.) then
                      !approximate TOA_SWUP fluxes in cloudy twi-light conditions as the cloud properties derived in these conditions are just the FG values, thus not reliable
                      tmp_mst=l2_input_2d%toa_swup(idim,jdim,inode)
                      l2_input_2d%toa_swup(idim,jdim,inode)=l2_input_2d%reflectance06(idim,jdim,inode)*100.*16. !*9.07416+114.467  !*15.
                      !print*,l2_input_2d%boa_swdn(idim,jdim,inode),tmp_mst,l2_input_2d%toa_swup(idim,jdim,inode)
                      l2_input_2d%boa_swdn(idim,jdim,inode)=l2_input_2d%boa_swdn(idim,jdim,inode)*min(5.,max(0.2,tmp_mst/l2_input_2d%toa_swup(idim,jdim,inode)))
                    endif
                   endif

                   jcount=0
                   if (csensor .ne. "SEVIRI") then
                    do idilon=1,359,2
                        icoszenith = cos(t0)*cos(t1)+sin(t0)*sin(t1)*cos(idilon*3.14/180.)
                        izenith    = acos(d2r*icoszenith)
                        ipath = 1.0*35./sqrt(1224.*(cos(d2r*izenith))**2.+1)
                   
                        irad1 = max(0., l2_input_2d%toa_swdn(idim,jdim,inode)/cos(l2_input_2d%solzen(idim,jdim)*3.14/180.)*cos(izenith*3.14/180.))
                        !irad_fact = min( 10.,max( 0.,sin((90.-l2_input_2d%solzen(idim,jdim))*3.14/180.)/sin((90.-izenith)*3.14/180.) ) )
                        irad_fact = ipath/path1 !min( 10.,max( 0.5,sin((90.-l2_input_2d%solzen(idim,jdim))*3.14/180.)/sin((90.-izenith)*3.14/180.) ) )
                        irad_fact = (irad_fact+1.0)*0.5

                        !BOA
                        sslope=(-0.00002)
                        if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint) sslope=0.00000
                        ialb_boa_sw     = max(0.0,min(1.0,sslope*izenith**2.+(alb_boa_sw-sslope*(l2_input_2d%solzen(idim,jdim)**2.))))
                        ialb_boa_sw_clr = max(0.0,min(1.0,sslope*izenith**2.+(alb_boa_sw_clr-sslope*(l2_input_2d%solzen(idim,jdim)**2.))))
                                                               
                        !boa_swdn
                        jrad_boa_swdn = jrad_boa_swdn + max(0., irad1*exp((-1.0)*ext_boa_swdn*irad_fact))

                        !boa_swdn_clr
                        jrad_boa_swdn_clr = jrad_boa_swdn_clr + max(0., irad1*exp((-1.0)*ext_boa_swdn_clr*irad_fact))

                        !boa_swup
                        jrad_boa_swup = jrad_boa_swup + max(0., irad1*exp((-1.0)*ext_boa_swdn*irad_fact)) * ialb_boa_sw

                        !boa_swup_clr
                        jrad_boa_swup_clr = jrad_boa_swup_clr + max(0., irad1*exp((-1.0)*ext_boa_swdn_clr*irad_fact)) * ialb_boa_sw_clr 
                                          
                        !TOA
                        tslope=0.00002
                        if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint) tslope=0.00002
                        if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint .and. l2_input_2d%cct(idim,jdim,inode) .lt. 0.5) tslope = 0.00000
                        ialb_toa_sw     = max(0.0,min(1.0,tslope*izenith**2.+(alb_toa_sw-tslope*(l2_input_2d%solzen(idim,jdim)**2.))))
                        if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint) tslope = 0.00000
                        ialb_toa_sw_clr = max(0.0,min(1.0,tslope*izenith**2.+(alb_toa_sw_clr-tslope*(l2_input_2d%solzen(idim,jdim)**2.))))
                               
                        !toa_swdn
                        jrad_toa_swdn = jrad_toa_swdn + irad1

                        !toa_swup
                        !jrad_toa_swup = jrad_toa_swup + max(0., l2_input_2d%toa_swup(idim,jdim,inode)/cos(d2r*l2_input_2d%solzen(idim,jdim))*cos(d2r*izenith))
                        jrad_toa_swup = jrad_toa_swup + max(0., irad1*ialb_toa_sw)
                                          
                        !toa_swup_clr
                        !jrad_toa_swup_clr = jrad_toa_swup_clr + max(0., l2_input_2d%toa_swup_clr(idim,jdim,inode)/cos(d2r*l2_input_2d%solzen(idim,jdim))*cos(izenith*d2r))
                        jrad_toa_swup_clr = jrad_toa_swup_clr + max(0., irad1*ialb_toa_sw_clr)

                        jcount = jcount+1
                    enddo

                    l3_macro%boa_swdn_mean(lon_i,lat_j)=l3_macro%boa_swdn_mean(lon_i,lat_j) + jrad_boa_swdn/jcount
                    l3_macro%boa_swup_mean(lon_i,lat_j)=l3_macro%boa_swup_mean(lon_i,lat_j) + jrad_boa_swup/jcount
                    l3_macro%boa_swdn_clr_mean(lon_i,lat_j)=l3_macro%boa_swdn_clr_mean(lon_i,lat_j) + jrad_boa_swdn_clr/jcount
                    l3_macro%boa_swup_clr_mean(lon_i,lat_j)=l3_macro%boa_swup_clr_mean(lon_i,lat_j) + jrad_boa_swup_clr/jcount
                    l3_macro%toa_swdn_mean(lon_i,lat_j)=l3_macro%toa_swdn_mean(lon_i,lat_j) + jrad_toa_swdn/jcount
                    l3_macro%toa_swup_mean(lon_i,lat_j)=l3_macro%toa_swup_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                    l3_macro%toa_swup_clr_mean(lon_i,lat_j)=l3_macro%toa_swup_clr_mean(lon_i,lat_j) + jrad_toa_swup_clr/jcount

                    ! scale the PAR values using the modified BOA SWDN
                    l3_macro%boa_par_dif_mean(lon_i,lat_j)=l3_macro%boa_par_dif_mean(lon_i,lat_j) + l2_input_2d%boa_par_dif(idim,jdim,inode) * &
                                                          (jrad_boa_swdn/jcount)/l2_input_2d%boa_swdn(idim,jdim,inode)
                    l3_macro%boa_par_tot_mean(lon_i,lat_j)=l3_macro%boa_par_tot_mean(lon_i,lat_j) + l2_input_2d%boa_par_tot(idim,jdim,inode) * &
                                                          (jrad_boa_swdn/jcount)/l2_input_2d%boa_swdn(idim,jdim,inode)

                    l3_macro%rad_points_sw(lon_i, lat_j) = l3_macro%rad_points_sw(lon_i, lat_j) + 1
                  else !if sensor is SEVIRI
                    jrad_toa_swup=l2_input_2d%toa_swup(idim,jdim,inode)
                    l3_macro%boa_swdn_mean(lon_i,lat_j)=l3_macro%boa_swdn_mean(lon_i,lat_j) + l2_input_2d%boa_swdn(idim,jdim,inode)
                    l3_macro%boa_swup_mean(lon_i,lat_j)=l3_macro%boa_swup_mean(lon_i,lat_j) + l2_input_2d%boa_swup(idim,jdim,inode)
                    l3_macro%boa_swdn_clr_mean(lon_i,lat_j)=l3_macro%boa_swdn_clr_mean(lon_i,lat_j) + l2_input_2d%boa_swdn_clr(idim,jdim,inode)
                    l3_macro%boa_swup_clr_mean(lon_i,lat_j)=l3_macro%boa_swup_clr_mean(lon_i,lat_j) + l2_input_2d%boa_swup_clr(idim,jdim,inode)
                    l3_macro%toa_swdn_mean(lon_i,lat_j)=l3_macro%toa_swdn_mean(lon_i,lat_j) + l2_input_2d%toa_swdn(idim,jdim,inode)
                    l3_macro%toa_swup_mean(lon_i,lat_j)=l3_macro%toa_swup_mean(lon_i,lat_j) + l2_input_2d%toa_swup(idim,jdim,inode)
                    l3_macro%toa_swup_clr_mean(lon_i,lat_j)=l3_macro%toa_swup_clr_mean(lon_i,lat_j) + l2_input_2d%toa_swup_clr(idim,jdim,inode)
                    l3_macro%rad_points_sw(lon_i, lat_j) = l3_macro%rad_points_sw(lon_i, lat_j) + 1
                    jcount = 1
                  endif !if (csensor .ne. SEVIRI)
                 endif

                 !====
                 !Diurnal cycle correction for LW fluxes
                 
                 dcc_boa_lwup     = 1.0
                 dcc_boa_lwup_clr = 1.0
                 dcc_toa_lwup     = 1.0
                 dcc_toa_lwup_clr = 1.0
                 
                 if (csensor .ne. "SEVIRI") then
                    !this approach could be revised at some point
                    time_start   = ( 367.0d0 * iyear - aint((7.0d0*(iyear+aint((imonth + 9.0d0)/12.0d0)))/4.0d0) + aint((275.0d0*imonth)/9.0d0) + iday + 1721013.5d0)
                    ltime        = (l2_input_2d%time(idim,jdim,inode)-time_start)*24.+l2_input_2d%lon(idim,jdim)/180.*12.
                    if(ltime .lt. 0.)  ltime=ltime+24.
                    if(ltime .gt. 24.) ltime=ltime-24.

                                                                                
                    if(l2_input_2d%lsflag(idim,jdim,inode) .eq. 1_sint) then
                        dcc_raw = sin(d2r*(ltime/24.*3.14*2.*180./3.14 + 55.))*cos(d2r*izenith)
                        dcc_boa_lwup     = dcc_raw*0.14+1.0
                        dcc_boa_lwup_clr = dcc_raw*0.14+1.0
                        dcc_toa_lwup     = dcc_raw*0.14*0.5+1.0
                        dcc_toa_lwup_clr = dcc_raw*0.14*0.5+1.0
                    endif
                 endif !if (csensor .ne. SEVIRI)

                 l3_macro%toa_lwup_mean(lon_i,lat_j)=l3_macro%toa_lwup_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)*dcc_toa_lwup
                 l3_macro%toa_lwup_clr_mean(lon_i,lat_j)=l3_macro%toa_lwup_clr_mean(lon_i,lat_j)+l2_input_2d%toa_lwup_clr(idim,jdim,inode)*dcc_toa_lwup_clr

                 l3_macro%boa_lwup_mean(lon_i,lat_j)=l3_macro%boa_lwup_mean(lon_i,lat_j)+l2_input_2d%boa_lwup(idim,jdim,inode)*dcc_boa_lwup
                 l3_macro%boa_lwup_clr_mean(lon_i,lat_j)=l3_macro%boa_lwup_clr_mean(lon_i,lat_j)+l2_input_2d%boa_lwup_clr(idim,jdim,inode)*dcc_boa_lwup_clr
                 l3_macro%boa_lwdn_mean(lon_i,lat_j)=l3_macro%boa_lwdn_mean(lon_i,lat_j)+l2_input_2d%boa_lwdn(idim,jdim,inode)
                 l3_macro%boa_lwdn_clr_mean(lon_i,lat_j)=l3_macro%boa_lwdn_clr_mean(lon_i,lat_j)+l2_input_2d%boa_lwdn_clr(idim,jdim,inode)

                 l3_macro%rad_points_lw(lon_i, lat_j) = l3_macro%rad_points_lw(lon_i, lat_j) + 1

              else
                 ! no need to explicitely add 0s
              endif
           endif
           !==================== END OF DCC  =========================

           if ( l2_input_2d%cct(idim,jdim,inode) .ge. filter_micro .and. &
                l2_input_2d%cct(idim,jdim,inode) .le. 1.0 .and.  &
                l2_input_2d%cct_error(idim,jdim,inode) .ge. filter_micro .and. &
                l2_input_2d%cct_error(idim,jdim,inode) .le. 100.) then

              call compute_stats_stage1(l2_input_2d%cct(idim,jdim,inode),l2_input_2d%cct_error(idim,jdim,inode),&
                   & l3_macro%cct_mean(lon_i,lat_j),l3_macro%cct_std(lon_i,lat_j),&
                   & l3_macro%cct_mean_error(lon_i,lat_j),l3_macro%cct_std_error(lon_i,lat_j))

           endif

           !pixel is daylight pixel
           if(l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint) then

              ! stapel moved outside of criteria if loop

              if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5) then
                 !day fraction
                 l3_macro%cct_points_day(lon_i,lat_j)=l3_macro%cct_points_day(lon_i,lat_j)+1
              elseif (l2_input_2d%cct(idim,jdim,inode) .le. 0.5 .and. l2_input_2d%cct(idim,jdim,inode) .ge. 0.0 ) then
                 !day clear fraction
                 l3_macro%cct_points_clear_day(lon_i,lat_j)=l3_macro%cct_points_clear_day(lon_i,lat_j)+1
              endif

              !if there is cloud and ...
              !if there are results for all quantities ...
              !and quality criteria are fulfilled ...
              !proceed with averaging them
              if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
                   & l2_input_2d%ctt(idim,jdim,inode) .gt. filter_micro .and. &
                   & l2_input_2d%cth(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%cloud_albedo1(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%cloud_albedo2(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%cloud_albedo1_error(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%cloud_albedo2_error(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%ctp(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%ctp_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%ctt_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%cth_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%cot(idim,jdim,inode) .gt. filter_micro .and. &
                   & l2_input_2d%ref(idim,jdim,inode) .gt. filter_micro .and. &
                   & l2_input_2d%cwp(idim,jdim,inode) .gt. filter_micro .and. &
                   & l2_input_2d%cot_error(idim,jdim,inode) .ge. filter_micro .and. &
                   & l2_input_2d%ref_error(idim,jdim,inode) .ge. filter_micro .and. &
                   & l2_input_2d%cwp_error(idim,jdim,inode) .ge. filter_micro .and. &
                   & l2_input_2d%cty(idim,jdim,inode) .gt. filter_micro+0.5    .and. & !0:clear,1 liq., 2 ice !for ORAC
                   & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. & !convergence reached
                   & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) ) then  !cost within bounds

                 !set filter arrays to 1 (this seems to be obsolete)
                 !                 l2_input_2d%l2_filter_array_2d_macro(idim,jdim)=1
                 !                 l2_input_2d%l2_filter_array_2d_micro(idim,jdim)=1

                 !this is the cloud fraction computation,
                 !make cloud cover compliant with property computation

                 !sum up cloudy points
                 l3_macro%cct_points(lon_i,lat_j)=l3_macro%cct_points(lon_i,lat_j)+1

                 !increase counter of macrophysically valid pixels
                 l3_macro%cloudy_points(lon_i,lat_j)=l3_macro%cloudy_points(lon_i,lat_j)+1

                 !Use ctp information to produce cloud cover for
                 !low/medium/high clouds cloud cover
                 current_ctp=l2_input_2d%ctp(idim,jdim,inode)


                 !bin points to high/middle/low pressure areas (defined in vartypes)
                 if(current_ctp .le. ctp_high) then
                    l3_macro%cct_high_points(lon_i,lat_j)=l3_macro%cct_high_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                          (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                             l3_macro%toa_swup_hig_mean(lon_i,lat_j)=l3_macro%toa_swup_hig_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                             l3_macro%rad_points_sw_hig(lon_i, lat_j) = l3_macro%rad_points_sw_hig(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_hig_mean(lon_i,lat_j)=l3_macro%toa_lwup_hig_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_hig(lon_i, lat_j) = l3_macro%rad_points_lw_hig(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 if(current_ctp .gt. ctp_high .and. current_ctp .le. ctp_middle ) then
                    l3_macro%cct_middle_points(lon_i,lat_j)=l3_macro%cct_middle_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                          (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                            l3_macro%toa_swup_mid_mean(lon_i,lat_j)=l3_macro%toa_swup_mid_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                            l3_macro%rad_points_sw_mid(lon_i, lat_j) = l3_macro%rad_points_sw_mid(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_mid_mean(lon_i,lat_j)=l3_macro%toa_lwup_mid_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_mid(lon_i, lat_j) = l3_macro%rad_points_lw_mid(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 if(current_ctp .gt. ctp_middle ) then
                    l3_macro%cct_low_points(lon_i,lat_j)=l3_macro%cct_low_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                          (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                             l3_macro%toa_swup_low_mean(lon_i,lat_j)=l3_macro%toa_swup_low_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                             l3_macro%rad_points_sw_low(lon_i, lat_j) = l3_macro%rad_points_sw_low(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_low_mean(lon_i,lat_j)=l3_macro%toa_lwup_low_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_low(lon_i, lat_j) = l3_macro%rad_points_lw_low(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 !end of cct computation


                 !############
                 !averaging
                 !############

                 !                  ! compute remedian statistics (i.e. calculate median for data subsets to save memory)
                 !                  call compute_remedian_stage1_macro(l2_input_2d,l3_macro,lon_i,lat_j,idim,jdim,inode)

                 !sum the macro variables now
                 !logarithmic ctp average
                 l3_macro%ctp_log_mean(lon_i,lat_j)=l3_macro%ctp_log_mean(lon_i,lat_j)+&
                      & log(l2_input_2d%ctp(idim,jdim,inode))

                 !arithmetric averages

                 !ctt
                 call compute_stats_stage1(l2_input_2d%ctt(idim,jdim,inode),l2_input_2d%ctt_error(idim,jdim,inode),&
                      & l3_macro%ctt_mean(lon_i,lat_j),l3_macro%ctt_std(lon_i,lat_j),&
                      & l3_macro%ctt_mean_error(lon_i,lat_j),l3_macro%ctt_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%ctt_prop_uncertainty(lon_i,lat_j),l3_macro%ctt_correlated_uncertainty(lon_i,lat_j))

                 !ctt_corrected
                 call compute_stats_stage1(l2_input_2d%ctt_corrected(idim,jdim,inode),l2_input_2d%ctt_corrected_error(idim,jdim,inode),&
                      & l3_macro%ctt_corrected_mean(lon_i,lat_j),l3_macro%ctt_corrected_std(lon_i,lat_j),&
                      & l3_macro%ctt_corrected_mean_error(lon_i,lat_j),l3_macro%ctt_corrected_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%ctt_corrected_prop_uncertainty(lon_i,lat_j),l3_macro%ctt_correlated_uncertainty(lon_i,lat_j))

                 !stemp
                 call compute_stats_stage1(l2_input_2d%stemp(idim,jdim,inode),l2_input_2d%stemp_error(idim,jdim,inode),&
                      & l3_macro%stemp_mean(lon_i,lat_j),l3_macro%stemp_std(lon_i,lat_j),&
                      & l3_macro%stemp_mean_error(lon_i,lat_j),l3_macro%stemp_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%stemp_prop_uncertainty(lon_i,lat_j),l3_macro%stemp_correlated_uncertainty(lon_i,lat_j))


                 !cth
                 call compute_stats_stage1(l2_input_2d%cth(idim,jdim,inode),l2_input_2d%cth_error(idim,jdim,inode),&
                      & l3_macro%cth_mean(lon_i,lat_j),l3_macro%cth_std(lon_i,lat_j),&
                      & l3_macro%cth_mean_error(lon_i,lat_j),l3_macro%cth_std_error(lon_i,lat_j))!, &
                 !                                        & l3_macro%cth_prop_uncertainty(lon_i,lat_j),l3_macro%cth_correlated_uncertainty(lon_i,lat_j))



                 !cth_corrected
                 call compute_stats_stage1(l2_input_2d%cth_corrected(idim,jdim,inode),l2_input_2d%cth_corrected_error(idim,jdim,inode),&
                      & l3_macro%cth_corrected_mean(lon_i,lat_j),l3_macro%cth_corrected_std(lon_i,lat_j),&
                      & l3_macro%cth_corrected_mean_error(lon_i,lat_j),l3_macro%cth_corrected_std_error(lon_i,lat_j), &
                      & l3_macro%cth_corrected_prop_uncertainty(lon_i,lat_j),l3_macro%cth_corrected_correlated_uncertainty(lon_i,lat_j))
                 !                       & l3_macro%cth_corrected_v1(lon_i,lat_j),l3_macro%cth_corrected_v2(lon_i,lat_j))

                 !ctp
                 call compute_stats_stage1(l2_input_2d%ctp(idim,jdim,inode),l2_input_2d%ctp_error(idim,jdim,inode),&
                      & l3_macro%ctp_mean(lon_i,lat_j),l3_macro%ctp_std(lon_i,lat_j),&
                      & l3_macro%ctp_mean_error(lon_i,lat_j),l3_macro%ctp_std_error(lon_i,lat_j))!, &
                 !                                        & l3_macro%ctp_prop_uncertainty(lon_i,lat_j),l3_macro%ctp_correlated_uncertainty(lon_i,lat_j))

                 !ctp_corrected
                 call compute_stats_stage1(l2_input_2d%ctp_corrected(idim,jdim,inode),l2_input_2d%ctp_corrected_error(idim,jdim,inode),&
                      & l3_macro%ctp_corrected_mean(lon_i,lat_j),l3_macro%ctp_corrected_std(lon_i,lat_j),&
                      & l3_macro%ctp_corrected_mean_error(lon_i,lat_j),l3_macro%ctp_corrected_std_error(lon_i,lat_j))!, &
                 !finished with macro variables
                 !sum the micro variables now

                 !sum up cloudy points (technically identical to the one at (*) but different in the clear)
                 l3_micro%cct_points(lon_i,lat_j)=l3_micro%cct_points(lon_i,lat_j)+1
                 l3_micro%cloudy_points(lon_i,lat_j)=l3_micro%cloudy_points(lon_i,lat_j)+1


                 ! compute remedian statistics (i.e. calculate median for data subsets to save memory)
                 !                  call compute_remedian_stage1_micro(l2_input_2d,l3_micro,lon_i,lat_j,idim,jdim,inode)

                 !cot

                 ! cut off COT at 100
                 cot_max = 100.
                 if ( l2_input_2d%cot(idim,jdim,inode) .gt. cot_max ) then
                    cot_factor = cot_max / l2_input_2d%cot(idim,jdim,inode)
                    l2_input_2d%cot(idim,jdim,inode) = l2_input_2d%cot(idim,jdim,inode) * cot_factor
                 endif

                 call compute_stats_stage1(l2_input_2d%cot(idim,jdim,inode),l2_input_2d%cot_error(idim,jdim,inode),&
                      & l3_micro%cot_mean(lon_i,lat_j),l3_micro%cot_std(lon_i,lat_j),&
                      & l3_micro%cot_mean_error(lon_i,lat_j),l3_micro%cot_std_error(lon_i,lat_j))!, &
                 !                                       & l3_micro%cot_prop_uncertainty(lon_i,lat_j),l3_micro%cot_correlated_uncertainty(lon_i,lat_j))

                 !logarithmic cot average
                 l3_micro%cot_log_mean(lon_i,lat_j)=l3_micro%cot_log_mean(lon_i,lat_j)+&
                      & log(l2_input_2d%cot(idim,jdim,inode))

                 !ref
                 call compute_stats_stage1(l2_input_2d%ref(idim,jdim,inode),l2_input_2d%ref_error(idim,jdim,inode),&
                      & l3_micro%ref_mean(lon_i,lat_j),l3_micro%ref_std(lon_i,lat_j),&
                      & l3_micro%ref_mean_error(lon_i,lat_j),l3_micro%ref_std_error(lon_i,lat_j))!,&
                 !                                       & l3_micro%ref_prop_uncertainty(lon_i,lat_j),l3_micro%ref_correlated_uncertainty(lon_i,lat_j))

                 !cloud_albedo1
                 call compute_stats_stage1(l2_input_2d%cloud_albedo1(idim,jdim,inode),l2_input_2d%cloud_albedo1_error(idim,jdim,inode),&
                      & l3_micro%cloud_albedo1_mean(lon_i,lat_j),l3_micro%cloud_albedo1_std(lon_i,lat_j),&
                      & l3_micro%cloud_albedo1_mean_error(lon_i,lat_j),l3_micro%cloud_albedo1_std_error(lon_i,lat_j))!,&
                 !                                       & l3_micro%cloud_albedo1_prop_uncertainty(lon_i,lat_j),l3_micro%cloud_albedo1_correlated_uncertainty(lon_i,lat_j))

                 !cloud_albedo2
                 call compute_stats_stage1(l2_input_2d%cloud_albedo2(idim,jdim,inode),l2_input_2d%cloud_albedo2_error(idim,jdim,inode),&
                      & l3_micro%cloud_albedo2_mean(lon_i,lat_j),l3_micro%cloud_albedo2_std(lon_i,lat_j),&
                      & l3_micro%cloud_albedo2_mean_error(lon_i,lat_j),l3_micro%cloud_albedo2_std_error(lon_i,lat_j))!,&
                 !                                       & l3_micro%cloud_albedo2_prop_uncertainty(lon_i,lat_j),l3_micro%cloud_albedo2_correlated_uncertainty(lon_i,lat_j))

                 !cee
                 if ((l2_input_2d%cee(idim,jdim,inode) .gt. 0.) .and. (l2_input_2d%cee_error(idim,jdim,inode) .gt. 0.)) then
                    ! counter for cloud effective emissivity (CEE)
                    ! this is necessary because CEE is a derived product and not available for all retrieved pixels
                    l3_macro%cee_points(lon_i, lat_j) = l3_macro%cee_points(lon_i, lat_j) + 1
                    call compute_stats_stage1(l2_input_2d%cee(idim,jdim,inode),l2_input_2d%cee_error(idim,jdim,inode),&
                         & l3_macro%cee_mean(lon_i,lat_j),l3_macro%cee_std(lon_i,lat_j),&
                         & l3_macro%cee_mean_error(lon_i,lat_j),l3_macro%cee_std_error(lon_i,lat_j))
                 endif

                 if ( l2_input_2d%cot(idim,jdim,inode) .gt. cot_max ) then
                    l2_input_2d%cwp(idim,jdim,inode) = l2_input_2d%cwp(idim,jdim,inode) * cot_factor
                 endif

                 !cwp
                 call compute_stats_stage1(l2_input_2d%cwp(idim,jdim,inode),l2_input_2d%cwp_error(idim,jdim,inode),&
                      & l3_micro%cwp_mean(lon_i,lat_j),l3_micro%cwp_std(lon_i,lat_j),&
                      & l3_micro%cwp_mean_error(lon_i,lat_j),l3_micro%cwp_std_error(lon_i,lat_j))!,&
                 !                                       & l3_micro%cwp_prop_uncertainty(lon_i,lat_j),l3_micro%cwp_correlated_uncertainty(lon_i,lat_j))


                 !sum liquid and ice instances
                 if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0 !FOR ORAC

                    l3_micro%cloudy_points_liq(lon_i,lat_j)=l3_micro%cloudy_points_liq(lon_i,lat_j)+1
                    l3_macro%cloudy_points_cph_liq(lon_i,lat_j)=l3_macro%cloudy_points_cph_liq(lon_i,lat_j)+1

                    ! cph
                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode),l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_macro%cty_mean(lon_i,lat_j),l3_macro%cty_std(lon_i,lat_j),&
                         & l3_macro%cty_mean_error(lon_i,lat_j),l3_macro%cty_std_error(lon_i,lat_j))!,&

                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode),l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_micro%cty_mean(lon_i,lat_j),l3_micro%cty_std(lon_i,lat_j),&
                         & l3_micro%cty_mean_error(lon_i,lat_j),l3_micro%cty_std_error(lon_i,lat_j))!,&

                    !lwp
                    call compute_stats_stage1(l2_input_2d%cwp(idim,jdim,inode),l2_input_2d%cwp_error(idim,jdim,inode),&
                         & l3_micro%cwp_mean_liq(lon_i,lat_j),l3_micro%cwp_std_liq(lon_i,lat_j),&
                         & l3_micro%cwp_mean_error_liq(lon_i,lat_j),l3_micro%cwp_std_error_liq(lon_i,lat_j))!,&
                    !                                             & l3_micro%cwp_prop_uncertainty_liq(lon_i,lat_j),l3_micro%cwp_correlated_uncertainty_liq(lon_i,lat_j))

                    !ref liquid
                    call compute_stats_stage1(l2_input_2d%ref(idim,jdim,inode),l2_input_2d%ref_error(idim,jdim,inode),&
                         & l3_micro%ref_mean_liq(lon_i,lat_j),l3_micro%ref_std_liq(lon_i,lat_j),&
                         & l3_micro%ref_mean_error_liq(lon_i,lat_j),l3_micro%ref_std_error_liq(lon_i,lat_j))!,&
                    !                                             & l3_micro%ref_prop_uncertainty_liq(lon_i,lat_j),l3_micro%ref_correlated_uncertainty_liq(lon_i,lat_j))

                    !cloud_albedo1 liquid
                    call compute_stats_stage1(l2_input_2d%cloud_albedo1(idim,jdim,inode),l2_input_2d%cloud_albedo1_error(idim,jdim,inode),&
                         & l3_micro%cloud_albedo1_mean_liq(lon_i,lat_j),l3_micro%cloud_albedo1_std_liq(lon_i,lat_j),&
                         & l3_micro%cloud_albedo1_mean_error_liq(lon_i,lat_j),l3_micro%cloud_albedo1_std_error_liq(lon_i,lat_j))!,&
                    !                                             & l3_micro%cloud_albedo1_prop_uncertainty_liq(lon_i,lat_j),l3_micro%cloud_albedo1_correlated_uncertainty_liq(lon_i,lat_j))

                    !cloud_albedo1 liquid
                    call compute_stats_stage1(l2_input_2d%cloud_albedo2(idim,jdim,inode),l2_input_2d%cloud_albedo2_error(idim,jdim,inode),&
                         & l3_micro%cloud_albedo2_mean_liq(lon_i,lat_j),l3_micro%cloud_albedo2_std_liq(lon_i,lat_j),&
                         & l3_micro%cloud_albedo2_mean_error_liq(lon_i,lat_j),l3_micro%cloud_albedo2_std_error_liq(lon_i,lat_j))!,&
                    !                                             & l3_micro%cloud_albedo2_prop_uncertainty_liq(lon_i,lat_j),l3_micro%cloud_albedo2_correlated_uncertainty_liq(lon_i,lat_j))

                    !cot liquid
                    call compute_stats_stage1(l2_input_2d%cot(idim,jdim,inode),l2_input_2d%cot_error(idim,jdim,inode),&
                         & l3_micro%cot_mean_liq(lon_i,lat_j),l3_micro%cot_std_liq(lon_i,lat_j),&
                         & l3_micro%cot_mean_error_liq(lon_i,lat_j),l3_micro%cot_std_error_liq(lon_i,lat_j))!,&
                    !                                             & l3_micro%cot_prop_uncertainty_liq(lon_i,lat_j),l3_micro%cot_correlated_uncertainty_liq(lon_i,lat_j))


                 elseif(l2_input_2d%cty(idim,jdim,inode) .gt. 1.5 ) then ! 2 ice for orac

                    l3_micro%cloudy_points_ice(lon_i,lat_j)=l3_micro%cloudy_points_ice(lon_i,lat_j)+1
                    l3_macro%cloudy_points_cph_ice(lon_i,lat_j)=l3_macro%cloudy_points_cph_ice(lon_i,lat_j)+1

                    ! cph
                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode)-2.,l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_macro%cty_mean(lon_i,lat_j),l3_macro%cty_std(lon_i,lat_j),&
                         & l3_macro%cty_mean_error(lon_i,lat_j),l3_macro%cty_std_error(lon_i,lat_j))!,&

                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode)-2.,l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_micro%cty_mean(lon_i,lat_j),l3_micro%cty_std(lon_i,lat_j),&
                         & l3_micro%cty_mean_error(lon_i,lat_j),l3_micro%cty_std_error(lon_i,lat_j))!,&

                    !iwp
                    call compute_stats_stage1(l2_input_2d%cwp(idim,jdim,inode),l2_input_2d%cwp_error(idim,jdim,inode),&
                         & l3_micro%cwp_mean_ice(lon_i,lat_j),l3_micro%cwp_std_ice(lon_i,lat_j),&
                         & l3_micro%cwp_mean_error_ice(lon_i,lat_j),l3_micro%cwp_std_error_ice(lon_i,lat_j))!,&
                    !                                             & l3_micro%cwp_prop_uncertainty_ice(lon_i,lat_j),l3_micro%cwp_correlated_uncertainty_ice(lon_i,lat_j))

                    !ref ice
                    call compute_stats_stage1(l2_input_2d%ref(idim,jdim,inode),l2_input_2d%ref_error(idim,jdim,inode),&
                         & l3_micro%ref_mean_ice(lon_i,lat_j),l3_micro%ref_std_ice(lon_i,lat_j),&
                         & l3_micro%ref_mean_error_ice(lon_i,lat_j),l3_micro%ref_std_error_ice(lon_i,lat_j))!,&
                    !                                             & l3_micro%ref_prop_uncertainty_ice(lon_i,lat_j),l3_micro%ref_correlated_uncertainty_ice(lon_i,lat_j))

                    !cloud_albedo1 ice
                    call compute_stats_stage1(l2_input_2d%cloud_albedo1(idim,jdim,inode),l2_input_2d%cloud_albedo1_error(idim,jdim,inode),&
                         & l3_micro%cloud_albedo1_mean_ice(lon_i,lat_j),l3_micro%cloud_albedo1_std_ice(lon_i,lat_j),&
                         & l3_micro%cloud_albedo1_mean_error_ice(lon_i,lat_j),l3_micro%cloud_albedo1_std_error_ice(lon_i,lat_j))!,&
                    !                                             & l3_micro%cloud_albedo1_prop_uncertainty_ice(lon_i,lat_j),l3_micro%cloud_albedo1_correlated_uncertainty_ice(lon_i,lat_j))

                    !cloud_albedo2 ice
                    call compute_stats_stage1(l2_input_2d%cloud_albedo2(idim,jdim,inode),l2_input_2d%cloud_albedo2_error(idim,jdim,inode),&
                         & l3_micro%cloud_albedo2_mean_ice(lon_i,lat_j),l3_micro%cloud_albedo2_std_ice(lon_i,lat_j),&
                         & l3_micro%cloud_albedo2_mean_error_ice(lon_i,lat_j),l3_micro%cloud_albedo2_std_error_ice(lon_i,lat_j))!,&
                    !                                             & l3_micro%cloud_albedo2_prop_uncertainty_ice(lon_i,lat_j),l3_micro%cloud_albedo2_correlated_uncertainty_ice(lon_i,lat_j))

                    !cot ice
                    call compute_stats_stage1(l2_input_2d%cot(idim,jdim,inode),l2_input_2d%cot_error(idim,jdim,inode),&
                         & l3_micro%cot_mean_ice(lon_i,lat_j),l3_micro%cot_std_ice(lon_i,lat_j),&
                         & l3_micro%cot_mean_error_ice(lon_i,lat_j),l3_micro%cot_std_error_ice(lon_i,lat_j))!,&
                    !                                             & l3_micro%cot_prop_uncertainty_ice(lon_i,lat_j),l3_micro%cot_correlated_uncertainty_ice(lon_i,lat_j))

                 endif

                 !make iscpp-like 7x6 2D histogram
                 call make_2d_histogram_cot_ctp( hist_cot, hist_ctp, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_cot_ctp_phase)

                 ! 1D histos
                 call make_1d_histogram_cot( hist_cot_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_cot_phase)

                 call make_1d_histogram_ref( hist_ref_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_ref_phase)

                 call make_1d_histogram_cloud_albedo1( hist_cloud_albedo1_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_cloud_albedo1_phase)

                 call make_1d_histogram_cloud_albedo2( hist_cloud_albedo2_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_cloud_albedo2_phase)

                 call make_1d_histogram_cwp( hist_cwp_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_micro%histogram_cwp_phase)

                 call make_1d_histogram_ctp( hist_ctp_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_macro%histogram_ctp_phase)

                 call make_1d_histogram_ctt( hist_ctt_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_macro%histogram_ctt_phase)

                 !if there are no valid results or there is no cloud (or no cloud mask information) ...
              else

                 !Pixel is clear (no cloud but not "unknown (fill value)")
                 if(l2_input_2d%cct(idim,jdim,inode) .le. 0.5 .and. l2_input_2d%cct(idim,jdim,inode) .ge. 0.0 ) then

                    !micro and macro clear points for cloud cover
                    l3_macro%cct_points_clear(lon_i,lat_j)=l3_macro%cct_points_clear(lon_i,lat_j)+1
                    l3_micro%cct_points_clear(lon_i,lat_j)=l3_micro%cct_points_clear(lon_i,lat_j)+1

                    !if (converged or cost or stemp not fill_value) then
                    if( .not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. &  !convergence reached
                         & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) .and. &  !cost within bounds
                         & l2_input_2d%stemp(idim,jdim,inode) .gt. filter_micro) then

                       l3_macro%stemp_clear_points(lon_i,lat_j)=l3_macro%stemp_clear_points(lon_i,lat_j)+1
                       call compute_stats_stage1(l2_input_2d%stemp(idim,jdim,inode),l2_input_2d%stemp_error(idim,jdim,inode),&
                            & l3_macro%stemp_clear_mean(lon_i,lat_j),l3_macro%stemp_clear_std(lon_i,lat_j),&
                            & l3_macro%stemp_clear_mean_error(lon_i,lat_j),l3_macro%stemp_clear_std_error(lon_i,lat_j))

                    endif

                 endif

                 !all the rest(no cloud and unknown)
                 l3_macro%clear_points(lon_i,lat_j)=l3_macro%clear_points(lon_i,lat_j)+1
                 !                 l2_input_2d%l2_filter_array_2d_macro(idim,jdim)=0

                 l3_micro%clear_points(lon_i,lat_j)=l3_micro%clear_points(lon_i,lat_j)+1
                 !                 l2_input_2d%l2_filter_array_2d_micro(idim,jdim)=0

              endif

              !this is twilight or night (no micro variables avail.)
           elseif(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 3_sint) then

              ! stapel moved outside of criteria if-loop
              if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5) then
                 !sum twl and night counter seperately
                 if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) then
                    l3_macro%cct_points_twl(lon_i,lat_j)=l3_macro%cct_points_twl(lon_i,lat_j)+1
                 endif
                 if(l2_input_2d%illum(idim,jdim,inode) .eq. 3_sint) l3_macro%cct_points_night(lon_i,lat_j)=l3_macro%cct_points_night(lon_i,lat_j)+1
              elseif (l2_input_2d%cct(idim,jdim,inode) .le. 0.5 .and. l2_input_2d%cct(idim,jdim,inode) .ge. 0.0 ) then
                 !day clear fraction
                 !twilight and night clear fraction
                 if(l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) l3_macro%cct_points_clear_twl(lon_i,lat_j)=&
                      & l3_macro%cct_points_clear_twl(lon_i,lat_j)+1
                 if(l2_input_2d%illum(idim,jdim,inode) .eq. 3_sint) l3_macro%cct_points_clear_night(lon_i,lat_j)=&
                      & l3_macro%cct_points_clear_night(lon_i,lat_j)+1
              endif

              !if there is cloud and ...
              !if there are results for all macro quantities ...
              !and quality criteria are fulfilled ...
              !proceed with averaging them
              if( l2_input_2d%cct(idim,jdim,inode) .gt. 0.5 .and. &
                   & l2_input_2d%ctt(idim,jdim,inode) .gt. filter_micro .and. &
                   & l2_input_2d%cth(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%ctp(idim,jdim,inode)  .gt. filter_micro .and. &
                   & l2_input_2d%ctp_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%ctt_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%cth_error(idim,jdim,inode)  .ge. filter_micro .and. &
                   & l2_input_2d%cty(idim,jdim,inode) .gt. filter_micro+0.5 .and. & !0:clear,1 liq., 2 ice !for ORAC
                   & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. &  !convergence reached
                   & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) ) then  !cost within bounds

                 !this is the cloud fraction computation,
                 !make cloud cover compliant with property computation

                 !sum up cloudy points
                 l3_macro%cct_points(lon_i,lat_j)=l3_macro%cct_points(lon_i,lat_j)+1

                 !increase counter of macrophysically valid pixels
                 l3_macro%cloudy_points(lon_i,lat_j)=l3_macro%cloudy_points(lon_i,lat_j)+1

                 !Use ctp information to produce cloud cover for
                 !low/medium/high clouds cloud cover

                 current_ctp=l2_input_2d%ctp(idim,jdim,inode)


                 !bin points to high/middle/low pressure areas (defined in vartypes)
                 if(current_ctp .le. ctp_high) then
                    l3_macro%cct_high_points(lon_i,lat_j)=l3_macro%cct_high_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                          (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                             l3_macro%toa_swup_hig_mean(lon_i,lat_j)=l3_macro%toa_swup_hig_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                             l3_macro%rad_points_sw_hig(lon_i, lat_j) = l3_macro%rad_points_sw_hig(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_hig_mean(lon_i,lat_j)=l3_macro%toa_lwup_hig_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_hig(lon_i, lat_j) = l3_macro%rad_points_lw_hig(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 if(current_ctp .gt. ctp_high .and. current_ctp .le. ctp_middle ) then
                    l3_macro%cct_middle_points(lon_i,lat_j)=l3_macro%cct_middle_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                           (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                             l3_macro%toa_swup_mid_mean(lon_i,lat_j)=l3_macro%toa_swup_mid_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                             l3_macro%rad_points_sw_mid(lon_i, lat_j) = l3_macro%rad_points_sw_mid(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_mid_mean(lon_i,lat_j)=l3_macro%toa_lwup_mid_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_mid(lon_i, lat_j) = l3_macro%rad_points_lw_mid(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 if(current_ctp .gt. ctp_middle ) then
                    l3_macro%cct_low_points(lon_i,lat_j)=l3_macro%cct_low_points(lon_i,lat_j)+1
                    if (proc_toa) then
                       if(l2_input_2d%toa_swdn(idim,jdim,inode) .gt. 0. .and. l2_input_2d%boa_swdn(idim,jdim,inode) .gt. 0. .and. &
                           (l2_input_2d%illum(idim,jdim,inode) .eq. 1_sint .or. l2_input_2d%illum(idim,jdim,inode) .eq. 2_sint) .and. jcount .gt. 0) then
                              l3_macro%toa_swup_low_mean(lon_i,lat_j)=l3_macro%toa_swup_low_mean(lon_i,lat_j) + jrad_toa_swup/jcount
                              l3_macro%rad_points_sw_low(lon_i, lat_j) = l3_macro%rad_points_sw_low(lon_i, lat_j) + 1
                       endif
                       if(l2_input_2d%toa_lwup(idim,jdim,inode) .gt. 0) then
                          l3_macro%toa_lwup_low_mean(lon_i,lat_j)=l3_macro%toa_lwup_low_mean(lon_i,lat_j)+l2_input_2d%toa_lwup(idim,jdim,inode)
                          l3_macro%rad_points_lw_low(lon_i, lat_j) = l3_macro%rad_points_lw_low(lon_i, lat_j) + 1
                       endif
                    endif
                 endif
                 !end of cct computation



                 !new do phase also for night
                 !sum liquid and ice instances
                 if(l2_input_2d%cty(idim,jdim,inode) .ge. 0.5 .and. l2_input_2d%cty(idim,jdim,inode) .lt. 1.5  ) then !liquid: 1.0 !FOR ORAC
                    l3_macro%cloudy_points_cph_liq(lon_i,lat_j)=l3_macro%cloudy_points_cph_liq(lon_i,lat_j)+1
                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode),l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_macro%cty_mean(lon_i,lat_j),l3_macro%cty_std(lon_i,lat_j),&
                         & l3_macro%cty_mean_error(lon_i,lat_j),l3_macro%cty_std_error(lon_i,lat_j))!,&

                 elseif(l2_input_2d%cty(idim,jdim,inode) .gt. 1.5 ) then !2 ice for orac
                    l3_macro%cloudy_points_cph_ice(lon_i,lat_j)=l3_macro%cloudy_points_cph_ice(lon_i,lat_j)+1
                    call compute_stats_stage1(l2_input_2d%cty(idim,jdim,inode)-2.,l2_input_2d%cty_error(idim,jdim,inode),&
                         & l3_macro%cty_mean(lon_i,lat_j),l3_macro%cty_std(lon_i,lat_j),&
                         & l3_macro%cty_mean_error(lon_i,lat_j),l3_macro%cty_std_error(lon_i,lat_j))!,&

                 endif


                 !############
                 !averaging
                 !############

                 ! compute remedian statistics (i.e. calculate median for data subsets to save memory)
                 !                  call compute_remedian_stage1_macro(l2_input_2d,l3_macro,lon_i,lat_j,idim,jdim,inode)

                 !sum the macro variables now
                 !logarithmic ctp average
                 l3_macro%ctp_log_mean(lon_i,lat_j)=l3_macro%ctp_log_mean(lon_i,lat_j)+log(l2_input_2d%ctp(idim,jdim,inode))

                 !arithmetric averages

                 !ctt
                 call compute_stats_stage1(l2_input_2d%ctt(idim,jdim,inode),l2_input_2d%ctt_error(idim,jdim,inode),&
                      & l3_macro%ctt_mean(lon_i,lat_j),l3_macro%ctt_std(lon_i,lat_j),&
                      & l3_macro%ctt_mean_error(lon_i,lat_j),l3_macro%ctt_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%ctt_prop_uncertainty(lon_i,lat_j),l3_macro%ctt_correlated_uncertainty(lon_i,lat_j))

                 !ctt_corrected
                 call compute_stats_stage1(l2_input_2d%ctt_corrected(idim,jdim,inode),l2_input_2d%ctt_corrected_error(idim,jdim,inode),&
                      & l3_macro%ctt_corrected_mean(lon_i,lat_j),l3_macro%ctt_corrected_std(lon_i,lat_j),&
                      & l3_macro%ctt_corrected_mean_error(lon_i,lat_j),l3_macro%ctt_corrected_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%ctt_corrected_prop_uncertainty(lon_i,lat_j),l3_macro%ctt_correlated_uncertainty(lon_i,lat_j))

                 !stemp
                 call compute_stats_stage1(l2_input_2d%stemp(idim,jdim,inode),l2_input_2d%stemp_error(idim,jdim,inode),&
                      & l3_macro%stemp_mean(lon_i,lat_j),l3_macro%stemp_std(lon_i,lat_j),&
                      & l3_macro%stemp_mean_error(lon_i,lat_j),l3_macro%stemp_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%stemp_prop_uncertainty(lon_i,lat_j),l3_macro%stemp_correlated_uncertainty(lon_i,lat_j))

                 !cth
                 call compute_stats_stage1(l2_input_2d%cth(idim,jdim,inode),l2_input_2d%cth_error(idim,jdim,inode),&
                      & l3_macro%cth_mean(lon_i,lat_j),l3_macro%cth_std(lon_i,lat_j),&
                      & l3_macro%cth_mean_error(lon_i,lat_j),l3_macro%cth_std_error(lon_i,lat_j))!,&
                 !                                      & l3_macro%cth_prop_uncertainty(lon_i,lat_j),l3_macro%cth_correlated_uncertainty(lon_i,lat_j))

                 !cth_corrected
                 call compute_stats_stage1(l2_input_2d%cth_corrected(idim,jdim,inode),l2_input_2d%cth_corrected_error(idim,jdim,inode),&
                      & l3_macro%cth_corrected_mean(lon_i,lat_j),l3_macro%cth_corrected_std(lon_i,lat_j),&
                      & l3_macro%cth_corrected_mean_error(lon_i,lat_j),l3_macro%cth_corrected_std_error(lon_i,lat_j),&
                      & l3_macro%cth_corrected_prop_uncertainty(lon_i,lat_j),l3_macro%cth_corrected_correlated_uncertainty(lon_i,lat_j))
                 !                       & l3_macro%cth_corrected_v1(lon_i,lat_j),l3_macro%cth_corrected_v2(lon_i,lat_j))




                 !ctp
                 call compute_stats_stage1(l2_input_2d%ctp(idim,jdim,inode),l2_input_2d%ctp_error(idim,jdim,inode),&
                      & l3_macro%ctp_mean(lon_i,lat_j),l3_macro%ctp_std(lon_i,lat_j),&
                      & l3_macro%ctp_mean_error(lon_i,lat_j),l3_macro%ctp_std_error(lon_i,lat_j))!,&
                 !                                        & l3_macro%ctp_prop_uncertainty(lon_i,lat_j),l3_macro%ctp_correlated_uncertainty(lon_i,lat_j))

                 !ctp_corrected
                 call compute_stats_stage1(l2_input_2d%ctp_corrected(idim,jdim,inode),l2_input_2d%ctp_corrected_error(idim,jdim,inode),&
                      & l3_macro%ctp_corrected_mean(lon_i,lat_j),l3_macro%ctp_corrected_std(lon_i,lat_j),&
                      & l3_macro%ctp_corrected_mean_error(lon_i,lat_j),l3_macro%ctp_corrected_std_error(lon_i,lat_j))!,&

                 !cee
                 if ((l2_input_2d%cee(idim,jdim,inode) .gt. 0.) .and. (l2_input_2d%cee_error(idim,jdim,inode) .gt. 0.)) then
                    ! counter for cloud effective emissivity (CEE)
                    ! this is necessary because CEE is a derived product and not available for all retrieved pixels
                    l3_macro%cee_points(lon_i, lat_j) = l3_macro%cee_points(lon_i, lat_j) + 1
                    call compute_stats_stage1(l2_input_2d%cee(idim,jdim,inode),l2_input_2d%cee_error(idim,jdim,inode),&
                         & l3_macro%cee_mean(lon_i,lat_j),l3_macro%cee_std(lon_i,lat_j),&
                         & l3_macro%cee_mean_error(lon_i,lat_j),l3_macro%cee_std_error(lon_i,lat_j))
                 endif


                 ! 1D histos
                 call make_1d_histogram_ctp( hist_ctp_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_macro%histogram_ctp_phase)

                 call make_1d_histogram_ctt( hist_ctt_1d_axis, &
                      & lon_i,lat_j,idim,jdim,inode,l2_input_2d,l3_macro%histogram_ctt_phase)

                 !if there are no valid results or there is no cloud (or no cloud mask information)
              else

                 !Pixel is clear (no cloud but not "unknown (fill value)")
                 if(l2_input_2d%cct(idim,jdim,inode) .le. 0.5 .and. l2_input_2d%cct(idim,jdim,inode) .ge. 0.0 ) then
                    l3_macro%cct_points_clear(lon_i,lat_j)=l3_macro%cct_points_clear(lon_i,lat_j)+1

                    !if (converged or cost or stemp not fill_value) then
                    if(.not. btest(l2_input_2d%qcflag(idim,jdim,inode),0) .and. &  !convergence reached
                       & .not. btest(l2_input_2d%qcflag(idim,jdim,inode),1) .and. &  !cost within bounds
                       & l2_input_2d%stemp(idim,jdim,inode) .gt. filter_micro) then

                       l3_macro%stemp_clear_points(lon_i,lat_j)=l3_macro%stemp_clear_points(lon_i,lat_j)+1
                       call compute_stats_stage1(l2_input_2d%stemp(idim,jdim,inode),l2_input_2d%stemp_error(idim,jdim,inode),&
                            & l3_macro%stemp_clear_mean(lon_i,lat_j),l3_macro%stemp_clear_std(lon_i,lat_j),&
                            & l3_macro%stemp_clear_mean_error(lon_i,lat_j),l3_macro%stemp_clear_std_error(lon_i,lat_j))

                    endif
                 endif

                 !all the rest(no cloud and unknown)
                 l3_macro%clear_points(lon_i,lat_j)=l3_macro%clear_points(lon_i,lat_j)+1
                 !                 l2_input_2d%l2_filter_array_2d_macro(idim,jdim)=0


              endif

           endif
        endif ! if valid cma
        enddo

     enddo

  enddo
