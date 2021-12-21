SUBROUTINE nc_close( ncid,fname,wo )


! Description:
!
! Closes netcdf-file.
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_close.f90
!
! Created on:          25/06/10
!
! Last Modified on:    August 5, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!   
! Modifications Log:        
! 2015/02/27, SST: added error message when closing input file fails
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use netcdf

  IMPLICIT NONE

   ! Input
   INTEGER,INTENT(IN) ::  ncid,wo
   CHARACTER(LEN=*) :: fname
   ! Local
   INTEGER ::  ierr

   ierr = NF90_CLOSE(ncid)
   IF (ierr.NE.NF90_NOERR) THEN
      write(*,*) 'path and file:', fname
      stop 'error closing input file'
   ENDIF

   IF (wo.EQ.1) THEN
      write(*,*) '---------------------------------------------'
      write(*,*) '  '
      write(*,*) 'closed file: ', fname
      write(*,*) '  '
      write(*,*) '---------------------------------------------'
   ENDIF
 
  RETURN

END SUBROUTINE nc_close


 
