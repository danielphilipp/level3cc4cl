! Name: nc_defdata_hist.f90
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
!2012/02/03 Matthias Jerg cleans out prototype code to prepare repository upload.
!2013/06/11 MJ implemented code to enable writing of netcdf-4 output files.
!2014/11/19 Oliver Sus: minor editing
!2015/02/27 SST: clean up
!2015/04/23 OS: some minor type changes
!2015/10/23 CP: added comment option
!
! $Id$
!
! Bugs:
!
!none known

SUBROUTINE nc_defdata_hist2d(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,wo,nflag,com_flag,var_com)

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
  ! Modifications Log:    
  ! 2015/07/20 CP bug fix
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(in) :: ncid, dims(6),  wo,nflag,com_flag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname, var_com
  integer(kind=lint) ::  var_fill

  ! Output
  INTEGER :: vid

  ! Local
  INTEGER(kind=stint) :: ierr
  !   CHARACTER(len=100) :: flag_mea
  !   INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
       & deflate_level=compress_level_lint,shuffle=shuffle_lint)!,chunksizes=chunksize6d)
  if (ierr.NE.NF90_NOERR) stop 'err def var int a'

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
  if(nflag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  else
     ierr = NF90_PUT_ATT(ncid, vid, 'name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var name'
  endif
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'

  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR) stop 'error def var comment'
  endif
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_hist2d


SUBROUTINE nc_defdata_hist1d(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,wo,nflag,com_flag,var_com)

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
  use vartypes

  use netcdf

  IMPLICIT NONE
 
!  INCLUDE 'netcdf.inc'

! Input
  INTEGER,INTENT(in) :: ncid, dims(5),  wo,com_flag,nflag
  CHARACTER(len=*),INTENT(in) :: var_name, var_lname, var_unit, var_sname,var_com
  integer(kind=lint) ::  var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER(kind=stint) :: ierr!, ivar_fill
  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '
write(*,*)'var_name',var_name
write(*,*)'vid',vid
write(*,*)'ncdi',ncid
write(*,*)'dims',dims
write(*,*)'compress_level_lint',compress_level_lint
write(*,*)'shuffle_lint',shuffle_lint
write(*,*)'chunksize6d',chunksize6d
  ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
         & deflate_level=compress_level_lint,shuffle=shuffle_lint)
  if (ierr.NE.NF90_NOERR) stop 'err def var int b'

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'

  if(nflag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'
  else
     ierr = NF90_PUT_ATT(ncid, vid, 'name', var_sname)
     IF (ierr.NE.NF90_NOERR) stop 'error def var name'
  endif

 
  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

!  ivar_fill= int(var_fill,kind=stint)
 
  ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
  IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR) stop 'error def var comment'
  endif

 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_hist1d



SUBROUTINE nc_defdata_hist1d_3dim(ncid,dims,var_name,vid,var_lname,var_sname, &
     var_unit,var_fill,xtype,wo,com_flag,var_com)


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

  use vartypes

  use netcdf

  IMPLICIT NONE
 
!  INCLUDE 'netcdf.inc'

! Input
  INTEGER,INTENT(in) :: ncid, dims(3),  wo,com_flag
  CHARACTER(len=*),INTENT(in) :: xtype, var_name, var_lname, var_unit, var_sname, var_com
  integer(kind=lint) ::  var_fill

! Output
  INTEGER :: vid
 
! Local
  INTEGER :: ierr, ivar_fill
!   CHARACTER(len=100) :: flag_mea
!   INTEGER :: flag_val(7),  flag_mn

  integer :: var_ln, var_sn,var_un

  var_ln=len_trim(adjustl(var_lname))
  var_sn=len_trim(adjustl(var_sname))
  var_un=len_trim(adjustl(var_unit))

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

!  ierr = NF90_DEF_VAR (ncid, 'time', NF90_INT2, tdim, tid)

  IF (xtype(1:10) .EQ. 'NF90_FLOAT') THEN
     ierr = NF90_DEF_VAR(ncid, var_name, NF90_FLOAT, dims, vid, &
          & deflate_level=compress_level_float,shuffle=shuffle_float)!, chunksizes=chunksize3d)
     if (ierr.NE.NF90_NOERR) stop 'err def var float'
  ELSEIF (xtype(1:8) .EQ. 'NF90_INT') THEN
     ierr = NF90_DEF_VAR(ncid, var_name, NF90_INT, dims, vid, &
          & deflate_level=compress_level_double)!, chunksizes=chunksize3d)
     if (ierr.NE.NF90_NOERR) stop 'err def var int c'
  ELSEIF (xtype(1:9) .EQ. 'NF90_BYTE') THEN
     ierr = NF90_DEF_VAR(ncid, var_name, NF90_BYTE, dims, vid, &
          & deflate_level=compress_level_double)!, chunksizes=chunksize3d)
     if (ierr.NE.NF90_NOERR) stop 'err def var byte'
  ENDIF

!  ierr = NF90_PUT_ATT(ncid, xid, 'long_name', 'longitude')

  ierr = NF90_PUT_ATT(ncid, vid, 'long_name', var_lname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var long_name'
 
!!$  IF (var_name(1:4) .EQ. 'nobs' .OR. var_name(1:4) .EQ.'sfc_') THEN
!!$     goto 20
!!$  ENDIF



  ierr = NF90_PUT_ATT(ncid, vid, 'standard_name', var_sname)
  IF (ierr.NE.NF90_NOERR) stop 'error def var standard_name'

! 20 continue

!!$  IF (var_name(1:4) .EQ. 'sfc_') THEN
!!$
!!$     flag_mea ="land ocean clouds ssmi_scattering sea_ice coast sun_glint"
!!$     flag_val(1) = 0
!!$     flag_val(2) = 1
!!$     flag_val(3) = 2
!!$     flag_val(4) = 3
!!$     flag_val(5) = 4 
!!$     flag_val(6) = 5
!!$     flag_val(7) = 6
!!$     
!!$     flag_mn = LEN_TRIM(flag_mea)
!!$     
!!$     ierr = NF_PUT_ATT_TEXT(ncid, vid, 'flag_meanings', flag_mn, flag_mea)
!!$     IF (ierr.NE.NF_NOERR) stop 'error def var flag_meaning'
!!$     
!!$     ierr = NF_PUT_ATT_INT(ncid, vid, 'flag_values', NF_BYTE, 7, flag_val)
!!$     IF (ierr.NE.NF_NOERR) stop 'error def var flag_values'
!!$     
!!$  ENDIF

  ierr = NF90_PUT_ATT(ncid, vid, 'units' ,var_unit )
  IF (ierr.NE.NF90_NOERR) stop 'error def var units'

  IF (xtype(1:10) .EQ. 'NF90_FLOAT') THEN
     ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', var_fill )
     IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ELSEIF (xtype(1:8) .EQ. 'NF90_INT') THEN
     ivar_fill= int(var_fill)
     ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', ivar_fill )
     IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ELSEIF (xtype(1:9) .EQ. 'NF90_BYTE') THEN
     ivar_fill= int(var_fill)
     ierr = NF90_PUT_ATT(ncid, vid, '_FillValue', ivar_fill )
     IF (ierr.NE.NF90_NOERR) stop 'error def var FillValue'
  ENDIF

  if(com_flag .eq. 1 ) then
     ierr = NF90_PUT_ATT(ncid, vid, 'comment', var_com)
     IF (ierr.NE.NF90_NOERR) stop 'error def var comment'
  endif
  
  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '
 
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined variable: ', trim(var_name)
  ENDIF

  RETURN

END SUBROUTINE nc_defdata_hist1d_3dim
