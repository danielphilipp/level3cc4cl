! Name: nc_open.f90
!
!
! Purpose: File contains subroutines to open netcdf file.
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
! $Id$
!
! Bugs:
!
!none known

SUBROUTINE nc_open(ncid,fname,ierr,wo)
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

  USE netcdf


  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

 ! Input
  INTEGER :: wo
  CHARACTER(LEN=*) :: fname

  ! Output
  INTEGER :: ncid, ierr

  write(*,*) 'FNAME: ', trim(adjustl(fname))
  ierr = nf90_open(path=trim(adjustl(fname)), mode=nf90_nowrite, ncid=ncid)       !open file
   IF (ierr.NE.NF90_NOERR) THEN
      write(*,*) 'path and file:', fname
      stop 'error open input file'
   ENDIF

   IF (wo.EQ.1) THEN
      write(*,*) '---------------------------------------------'
      write(*,*) '  '
      write(*,*) 'open file: ', fname
      write(*,*) '  '
      write(*,*) '---------------------------------------------'
   ENDIF

  RETURN
END SUBROUTINE nc_open
