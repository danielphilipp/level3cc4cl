MODULE nc_write_L3

CONTAINS

SUBROUTINE nc_write_L3_float(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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
  ! SST, 2015/02/27: now placed within module, removal of superfluous subroutines
  ! OS,  2015/12/16: some cleanup
  ! OS,  2016/03/18: print variable name when writing fails
  !
  ! Applied SPRs:
  !
  !-----------------------------------------------------------------------
  !
  ! Declarations:
  !
  !---------------------------------

! ---------------------------------

  use vartypes

  use netcdf

IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) ::  ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  real(kind=sreal) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3),stride(3)

  start = 1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1 ! time
  ierr = NF90_PUT_VAR(ncid, vid,  v, start, counter,stride)
  if (ierr.NE.NF90_NOERR) then 
     write(*,*) 'err write v: ', trim(adjustl(var_name))
     stop 
  endif

!  RETURN
END SUBROUTINE nc_write_L3_float

SUBROUTINE nc_write_L3_double(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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

! ---------------------------------

  use vartypes

  use netcdf

IMPLICIT NONE

! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) ::  ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  real(kind=dreal) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3),stride(3)

  start=1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1 ! time

  ierr = NF90_PUT_VAR(ncid, vid,  v, start, counter,stride)

  if (ierr.NE.NF90_NOERR) stop 'err write v' 

  IF (wo.EQ.1) THEN
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

!  RETURN
END SUBROUTINE nc_write_L3_double

SUBROUTINE nc_write_L3_lint(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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

! ---------------------------------

  use vartypes

  use netcdf

IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) ::  ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  integer(kind=lint) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3),stride(3)

  start=1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1 ! time

  ierr = NF90_PUT_VAR(ncid, vid, v,start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

 ! RETURN
END SUBROUTINE nc_write_L3_lint



SUBROUTINE nc_write_L3_stint(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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

! ---------------------------------

  use vartypes

  use netcdf

IMPLICIT NONE
 
! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) ::  ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  integer(kind=stint) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3),stride(3)

  start=1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v,start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_stint


SUBROUTINE nc_write_L3_byte_flag(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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

! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  !integer(KIND=sint),DIMENSION(ix:nx,iy:ny,1),INTENT(in) :: v
  integer(kind=sint) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3),stride(3)

  start=1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v,start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

 ! RETURN
END SUBROUTINE nc_write_L3_byte_flag


SUBROUTINE nc_write_L3_stint_flag(ncid,var_name,vid,v,ix,nx,iy,ny,wo)
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

! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  integer(KIND=stint),DIMENSION(ix:nx,iy:ny,1),INTENT(in) :: v
  integer(kind=stint) v(:,:)

! Local
  INTEGER :: ierr, start(3), counter(3), stride(3)

  start=1
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v,start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

!  RETURN
END SUBROUTINE nc_write_L3_stint_flag

END MODULE nc_write_L3
