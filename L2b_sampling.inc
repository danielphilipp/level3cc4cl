  ! Name: L2b_sampling.inc
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
  !2015/04/23 OS+SteSta implemented fix to prevent that data of subsequent day are processed as well for L3U;
  !             new sampling approach that resamples pixels in case pixel size is larger than grid box at high latitudes
  !2015/07/10 OS+SteSta update to grid box filling and time stop calculation
  !2015/12/16 OS added variable time to geoloc if loop; cleanup
  !2016/02/18 OS ascending/descending node separation now done via across-track longitude differences
  !2016/03/18 OS removed print statement
  !2017/06/22 OS bug fixing grid box sampling calculations
  !2017/10/11 SSt bug fix. Avoid dividing by Zero if gridx_dist equals Zero.
  !
  ! $Id$
  !
  ! Bugs:
  !
  !none known

  !set inode to 1.
  !this setting has to be chosen to use the same L2 input structure for both the l3 and l2b section of the code


  nnodes=1
  inode=nnodes

  !call random_seed
  read(year,*) yy
  read(month,*) mm
  read(day,*) dd

  ! time_stop in julianday (adds simply plus 1 to processed day)
  ! prevents that data of subsequent day are processed as well for L3U
  time_start = ( 367.0d0 * yy - aint((7.0d0*(yy+aint((mm + 9.0d0)/12.0d0)))/4.0d0) + aint((275.0d0*mm)/9.0d0) + dd + 1721013.5d0)
  time_stop  = time_start +1.0d0

  maxi=lon_offset*gridx
  maxj=lat_offset*gridy
  gridy_dist = 1./real(gridy,kind=dreal)*d2r ! grid distance in Rad in Y-direction, this is always the same 
                                             ! because meridians are great circles. While gridx_dist depends
                                             ! on latitude
  lasc=.true.

  do jdim=1,ydim1km
     found_node=.true.

     !determine pixel with minimum satzen in the current line (this should be the (most) nadir pixel)
     min_satzen_line(1)=-1
     min_satzen_line=minloc(l2_input_2d%satzen(:,jdim), &
                     mask=l2_input_2d%satzen(:,jdim) .ge. (real_fill_value+1.0))
     if(min_satzen_line(1) .lt. 1) cycle

     !check on which side of the orbit we are (ascending or descending) with the above determined pixel
     do iidim=1, min((min_satzen_line(1)-1),(xdim1km-min_satzen_line(1)))
        lon1 = l2_input_2d%lon(min_satzen_line(1)-iidim,jdim)
        lon2 = l2_input_2d%lon(min_satzen_line(1)+iidim,jdim)
        if( lon1 .ge. (real_fill_value+1.0) .and. lon2 .ge. (real_fill_value+1.0) ) exit
     enddo

     if( lon1 .ge. (real_fill_value+1.0) .and. lon2 .ge. (real_fill_value+1.0) ) then
        if ( (lon1 .lt. -170.) .and. (lon2 .gt. 170.) ) then
           lon1 = 360. + lon1
        elseif ( (lon1 .gt. 170.) .and. (lon2 .lt. -170.) ) then
           lon2 = 360. + lon2
        endif
        if (lon1 .lt. lon2) then
           !descending
           lasc=.false.
        elseif (lon1 .gt. lon2 ) then
           !ascending
           lasc=.true.
        elseif (lon1 .eq. lon2) then
           !found_node=.false.
        endif
     else
        found_node=.false.
     endif
     
     do idim=1,xdim1km
        !look if geolocation information exists
        if(l2_input_2d%lat(idim,jdim)  .ge. (real_fill_value+1.0) .and. &
           l2_input_2d%lon(idim,jdim)  .ge. (real_fill_value+1.0) .and. &
           l2_input_2d%time(idim,jdim,inode) .lt. time_stop .and. &
           l2_input_2d%lat(idim,jdim)  .ge. float(slat) .and. &
           l2_input_2d%lat(idim,jdim)  .le. float(elat) .and. &
           l2_input_2d%lon(idim,jdim)  .ge. float(slon) .and. &
           l2_input_2d%lon(idim,jdim)  .le. float(elon) .and. (found_node) ) then
           if ( idim .lt. xdim1km ) then
              i_incr = 1
           else
              i_incr =-1
           endif
           if ( jdim .lt. ydim1km ) then
              j_incr = 1
           else
              j_incr =-1
           endif
           
           !sensor longitude distance between 2 neighbouring pixels in rad!
           lon_dist1 = acos( max(min((sin(l2_input_2d%lat(idim,jdim)*d2r)**2.)+(cos(l2_input_2d%lat(idim,jdim)*d2r)**2.) * &
                        cos((l2_input_2d%lon(idim,jdim)-l2_input_2d%lon(idim+i_incr,jdim))*d2r),1.),-1.))
           lon_dist2 = acos( max(min((sin(l2_input_2d%lat(idim,jdim)*d2r)**2.)+(cos(l2_input_2d%lat(idim,jdim)*d2r)**2.) * &
                        cos((l2_input_2d%lon(idim,jdim)-l2_input_2d%lon(idim,jdim+j_incr))*d2r),1.),-1.))
           ! sensor latitude distance between 2 neighbouring pixels in rad!
           lat_dist1 = acos( max(min( sin(l2_input_2d%lat(idim,jdim)*d2r)*sin(l2_input_2d%lat(idim+i_incr,jdim)*d2r) + &
                        cos(l2_input_2d%lat(idim,jdim)*d2r)*cos(l2_input_2d%lat(idim+i_incr,jdim)*d2r),1.),-1.))
           lat_dist2 = acos( max(min( sin(l2_input_2d%lat(idim,jdim)*d2r)*sin(l2_input_2d%lat(idim,jdim+j_incr)*d2r) + &
                        cos(l2_input_2d%lat(idim,jdim)*d2r)*cos(l2_input_2d%lat(idim,jdim+j_incr)*d2r),1.),-1.))
 
           ! cycle if big lon/lat jumps appear in the data that are not caused by fillvalues, fillvalues are treated later
           if ( ( lon_dist1 .gt. 1.0d-2 ) .and. ( l2_input_2d%lon(idim+i_incr,jdim) .ne. real_fill_value ) ) cycle
           if ( ( lon_dist2 .gt. 1.0d-2 ) .and. ( l2_input_2d%lon(idim,jdim+j_incr) .ne. real_fill_value ) ) cycle
           if ( ( lat_dist1 .gt. 1.0d-2 ) .and. ( l2_input_2d%lat(idim+i_incr,jdim) .ne. real_fill_value ) ) cycle
           if ( ( lat_dist2 .gt. 1.0d-2 ) .and. ( l2_input_2d%lat(idim,jdim+j_incr) .ne. real_fill_value ) ) cycle

           !Calculate the Grid x distance in rad depending on Latitude!
           gridx_dist = 2.*pi*cos(l2_input_2d%lat(idim,jdim)*d2r) / real(maxi,kind=dreal)
 
           ! If the grid distance is smaller, than the width of a sensor pixel, we have to perform a filling.
           pmax_lon=0
           pmax_lat=0
           !avoid dividing by Zero, gridx and gridy dist should always be
           !greater 0 , except for gridx_dist at exactly 90° or -90° Latitude
           if ( gridx_dist .gt. 0.0d0 ) pmax_lon=int((lon_dist1 + lon_dist2)/gridx_dist)
           if ( gridy_dist .gt. 0.0d0 ) pmax_lat=int((lat_dist1 + lat_dist2)/gridy_dist)

           ! set pmax to zero if incremented lon/lat is fillvalue
           if ( ( l2_input_2d%lon(idim+i_incr,jdim) .eq. real_fill_value ) .or. &
                ( l2_input_2d%lon(idim,jdim+j_incr) .eq. real_fill_value ) ) pmax_lon = 0 
           if ( ( l2_input_2d%lat(idim+i_incr,jdim) .eq. real_fill_value ) .or. &
                ( l2_input_2d%lat(idim,jdim+j_incr) .eq. real_fill_value ) ) pmax_lat = 0

           ! dont allow pmax to be greater than grid           
           pmax_lon = min( pmax_lon, maxi )
           pmax_lat = min( pmax_lat, 10 )

           do p = floor(-1.0*pmax_lon/2),ceiling(1.0*pmax_lon/2)
              do q = ceiling(-1.0*pmax_lat/2),ceiling(1.0*pmax_lat/2)

                 lon_i = int((l2_input_2d%lon(idim,jdim)+(-1.0)*slon)*gridx) +1 + p
                 lat_j = int((l2_input_2d%lat(idim,jdim)+(-1.0)*slat)*gridy) +1 + q

                 if (.not. llocal) then ! global area
                    if ( lon_i .gt. maxi) lon_i = lon_i - maxi
                    if ( lon_i .lt. 1) lon_i = maxi + lon_i
                    if ((lat_j .gt. maxj) .or. (lat_j .lt. 1) ) cycle
                 else
                    if ( lon_i .gt. maxi) cycle
                    if ( lon_i .lt. 1) cycle
                    if ((lat_j .gt. maxj) .or. (lat_j .lt. 1) ) cycle
                 endif
             
                 include "do_L2b_sampling.inc"

              enddo ! q
           enddo ! p
        endif
     enddo
  enddo
