! Name: nc_defdata.f90
!
!
! Purpose: File contains subroutines define netcdf variables.
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
! 2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
! 2013/06/11 MJ implemented code to enable writing of netcdf-4 output files.
! 2015/07/10 OS+SteSta added SR nc_defdata_time and nc_defdata_stint_scale_float
! 2015/10/22 CP CF compliance update added comment to netcdf definition
! 2015/12/16 OS update for var_name check compliancy with new L3 naming
! 2016/01/21 OS vmin and vmax now intent in
! 2016/02/18 OS time variable definition changed from short to float
! 2016/02/23 OS time variable definition changed from float to long integer
! 2016/02/26 OS type change of time vmin/vmax
! 2016/03/07 OS in SR defdata_stint_flag_values, NetCDF attribute name changed to "flag_masks"
! 2016/03/18 OS changed type of vmin/vmax for scaled variables
! 2017/06/22 OS added var_unit where missing

! $Id$
!
! Bugs:
!
!none known

SUBROUTINE nc_defdata_float(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_unit,var_fill,wo,scale,offset,vmin,vmax,sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname, var_com
  REAL ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid,&
       & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then 
     write(*,*) 'err def var float',trim(adjustl(var_name)),dims,vid,compress_level_float,shuffle_float
     stop 
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'1'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_float


SUBROUTINE nc_defdata_double(ncid,dims,var_name,vid,&
     & var_lname,var_sname,var_unit,var_fill,wo,scale,offset,vmin,vmax,sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname, var_com
  real(kind=8) ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE, dims, vid, &
       & deflate_level=compress_level_double,shuffle=shuffle_double)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var double',trim(adjustl(var_name))
     stop 
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  IF ( var_name(1:4) .NE. 'time' ) then
     ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
     IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
     ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
     ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
     ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
     ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  endif
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'2'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_double


SUBROUTINE nc_defdata_lint(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_unit, var_fill, wo, scale,offset,vmin,vmax, sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project.
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname, var_com
  INTEGER ::  var_fill, scale, offset
  INTEGER(kind = lint),INTENT(in) :: vmin, vmax

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '
  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
       & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var lint',trim(adjustl(var_name))
     stop 
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  IF ( ( var_name(1:4) .NE. 'nobs' ) .or. ( var_name(1:4) .NE. 'nret' ) ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
     ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
     ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
     ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  ENDIF
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'3'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_lint

SUBROUTINE nc_defdata_nint(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_unit,var_fill,wo,scale,offset,vmin,vmax, sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname,var_com
  INTEGER ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
       & deflate_level=compress_level_nint,shuffle=shuffle_nint)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var nint', trim(adjustl(var_name))
     stop 
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'4'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_nint

SUBROUTINE nc_defdata_time(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_calendar,var_unit,var_fill,wo,scale,offset,vmin,vmax, sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit,var_sname,var_calendar,var_com
  REAL(kind=dreal) :: scale, offset
  INTEGER(KIND=lint) ::  var_fill, vmin, vmax
!  REAL(kind=sreal) :: var_fill

!
! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
       & deflate_level=compress_level_lint,shuffle=shuffle_lint)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var stint', trim(adjustl(var_name))
     stop
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'calendar' ,var_calendar )
  IF (ierr.NE.NF90_NOERR) stop 'error def var calendar'
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_time


SUBROUTINE nc_defdata_stint(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_unit,var_fill,wo,scale,offset,vmin,vmax, sname_flag,com_flag,var_com)
  !sstapelb removed xtype, nflag
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname, var_com
  INTEGER(KIND=stint) ::  var_fill, scale, offset, vmin, vmax

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
       & deflate_level=compress_level_stint,shuffle=shuffle_stint)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var stint', trim(adjustl(var_name))
     stop
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif
  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'4'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_stint

SUBROUTINE nc_defdata_stint_scale_float(ncid,dims,var_name,vid,var_lname,var_sname, &
           & var_unit,var_fill,wo,scale,offset,vmin,vmax, sname_flag,com_flag,var_com)

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit,var_sname, var_com
  INTEGER(KIND=stint) ::  var_fill, vmin, vmax
  REAL(kind=sreal) :: scale, offset


! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
       & deflate_level=compress_level_stint,shuffle=shuffle_stint)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var stint', trim(adjustl(var_name))
     stop
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ierr = NF90_PUT_ATT(ncid, vid, 'scale_factor', scale )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var scale'
  ierr = NF90_PUT_ATT(ncid, vid, 'add_offset', offset )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def offset'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_min', vmin )
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmin'
  ierr = NF90_PUT_ATT(ncid, vid, 'valid_max', vmax)
  IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def vmax'
  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_stint_scale_float


SUBROUTINE nc_defdata_byte_flag_value(ncid,dims,var_name,vid,var_lname,var_sname, &
     & flag_val,nflag,var_mean,var_fill,scale, offset, vmin, vmax,wo, sname_flag,com_flag,var_com)

!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !   
  ! Modifications Log:    
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, nflag , sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname,var_mean, var_com
  integer(kind=sint) ::  scale, offset, vmin, vmax
  integer(kind=sint) :: var_fill, flag_val(nflag)

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr!, ivar_fill
!   CHARACTER(len=100) :: flag_mea
!   INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn!,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
!   var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid, &
       & deflate_level=compress_level_byte,shuffle=shuffle_byte)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var byte flag', trim(adjustl(var_name))
     stop
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'

  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'flag_values' ,flag_val)
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, 'flag_meanings' ,var_mean )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'5'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_byte_flag_value


SUBROUTINE nc_defdata_stint_flag_value(ncid,dims,var_name,vid,var_lname,var_sname,var_unit,flag_val,nflag,&
      & var_mean,var_fill,scale,offset,vmin,vmax,wo,sname_flag,com_flag,var_com)


!
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  !
  ! Created on:          05/12/10
  !                      by Matthias Jerg, DWD/KU22
  !                      (matthias.jerg@dwd.de)
  !                      based on code provided by Nadine Schneider (nadine.schneider@dwd.de).
  !
  !
  ! Modifications Log:
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use netcdf

  use vartypes

  IMPLICIT NONE

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo, nflag , sname_flag, com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_sname,var_mean, var_com, var_unit
  integer(kind=stint) ::  scale, offset, vmin, vmax
  integer(kind=stint) :: var_fill,flag_val(nflag)

! Output
  INTEGER :: vid

! Local
  INTEGER :: ierr!, ivar_fill
!   CHARACTER(len=100) :: flag_mea
!   INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn!,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  !var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_SHORT, dims, vid, &
  & deflate_level=compress_level_stint_flag,shuffle=shuffle_stint_flag)!, chunksizes=chunksize3d)
  if (ierr.NE.NF90_NOERR) then
     write(*,*) 'err def var stint flag',trim(adjustl(var_name))
     stop
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'

  if(sname_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  endif

  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit)
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'
  
  ierr = NF90_PUT_ATT(ncid, vid, 'flag_masks' ,flag_val)
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, 'flag_meanings' ,var_mean )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR)  write(*,*) 'error def var_com'
  endif

  ierr = NF90_ENDDEF(ncid)
!   write(*,*)'5'
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_stint_flag_value
