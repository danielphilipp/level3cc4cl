! Name: nc_dim_info.f90
!
!
! Purpose: File contains subroutines to access dimension information of netcdf file.
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

SUBROUTINE nc_info(ncid,ndim,nvar,nattr,wo)
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
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

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  
  ! Output
  INTEGER :: ndim, nvar, nattr
  
  ! Local
  INTEGER :: ierr

  ! End of header ----------------------------------------------------------



  ierr = 0
  
  ierr = nf90_inquire(ncid,ndim,nvar,nattr)  !Amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
     stop 'inq all'
  ENDIF
  
!  write(*,*) ncid,ndim,nvar,nattr
 
  
  RETURN
 
END SUBROUTINE nc_info


SUBROUTINE nc_dim_id(ncid,name,did,wo)
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
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

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  CHARACTER(LEN=*) :: name

  ! Output
  INTEGER :: did!, n

!   integer :: diddummy

  ! Local
  INTEGER :: ierr
!   character :: dummyname*100

  ! End of header ----------------------------------------------------------

  ierr = 0
 
  ierr = NF90_INQ_DIMID(ncid,name,did)  !Amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
     stop 'inq dimid'
  ENDIF

  
  IF (wo.EQ.1) THEN
     write(*,*) did,': ', did
  ENDIF
    
  RETURN
 
END SUBROUTINE nc_dim_id


SUBROUTINE nc_dim_length(ncid,dname,did,n,wo)
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
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

  use vartypes

  IMPLICIT NONE

!   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo
  CHARACTER(LEN=varlength) :: dname

  ! Output
  INTEGER :: did, n

!   integer :: diddummy

  ! Local
  INTEGER :: ierr
!   character :: dummyname*100

  ! End of header ----------------------------------------------------------

  ierr = 0

  ierr = NF90_INQUIRE_DIMENSION(ncid,did,dname,n)    !searches the amount of pixels
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq dimlen'
  ENDIF
  
  IF (wo.EQ.1) THEN
     write(*,*) TRIM(dname),': ', n
  ENDIF
    
  RETURN
 
END SUBROUTINE nc_dim_length
