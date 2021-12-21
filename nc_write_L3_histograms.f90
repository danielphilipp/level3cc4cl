! Name: nc_write_L3_histograms.f90
!
!
! Purpose: File contains subroutines to open and define netcdf file.
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
! 2015/03/20 CP: adds cloud_albedo and header info
!
! $Id$
!
! Bugs:
!
!none known


MODULE nc_write_L3_histograms

CONTAINS

SUBROUTINE nc_write_L3_hist2d(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!
! Modifications Log:
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid, vid,  wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  INTEGER(KIND=lint),DIMENSION(ix:nx,iy:ny,1:n_hist_cot-1,1:n_hist_ctp-1,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:,:)

  ! Local
  INTEGER :: ierr, start(6), counter(6),stride(6)

  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_hist_cot-1
  counter(4) = n_hist_ctp-1
  counter(5) = n_hist_phase
  counter(6) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'

  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist2d


SUBROUTINE nc_write_L3_hist1d_cot(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) :: ncid, vid,  wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  INTEGER(kind=lint),DIMENSION(ix:nx,iy:ny,1:n_cot_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)

  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_cot_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_cot

SUBROUTINE nc_write_L3_hist1d_ctp(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) :: ncid, vid,  wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  INTEGER(KIND=lint),DIMENSION(ix:nx,iy:ny,1:n_ctp_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)
  
  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_ctp_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_ctp


SUBROUTINE nc_write_L3_hist1d_ctt(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

  !INTEGER(kind=lint),DIMENSION(ix:nx,iy:ny,1:n_ctt_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)

  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_ctt_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_ctt


SUBROUTINE nc_write_L3_hist1d_cwp(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  integer(kind=lint),DIMENSION(ix:nx,iy:ny,1:n_cwp_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)

  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_cwp_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_cwp




SUBROUTINE nc_write_L3_hist1d_cloud_albedo(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  integer(kind=lint),DIMENSION(ix:nx,iy:ny,1:n_cloud_albedo_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)

  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_cloud_albedo1_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_cloud_albedo




SUBROUTINE nc_write_L3_hist1d_ref(ncid,var_name,vid,v,ix,nx,iy,ny,wo)

! Description:
!
! Writes data in file FLOAT
! for other data fomat see: nc_write.f90
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_write3d.f90
!
! Created on:          25/06/10
!                      by Nadine Schneider, DWD/KU22
!                      (nadine.schneider@dwd.de)
!
! Last Modified on:    August 12, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      - add BYTE-Format
!   
! Modifications Log:        
!
! Applied SPRs:
!
!-----------------------------------------------------------------------
!
! Declarations:
!

! ---------------------------------

  use vartypes

  use netcdf

  IMPLICIT NONE
 
  ! Input
  INTEGER,INTENT(IN) :: ncid, vid, wo
  INTEGER(kind=lint),INTENT(IN) :: ix, nx, iy, ny
  CHARACTER(LEN=*),INTENT(in) ::  var_name

!  integer(kind=lint),DIMENSION(ix:nx,iy:ny,1:n_ref_bins,1:n_hist_phase,1),INTENT(in) :: v
  integer(kind=lint) v(:,:,:,:)

  ! Local
  INTEGER :: ierr, start(5), counter(5),stride(5)
  
  start=1
  start(1)=ix
  start(2)=iy
  stride=1

  counter(1) = nx
  counter(2) = ny
  counter(3) = n_ref_bins
  counter(4) = n_hist_phase
  counter(5) = 1

  ierr = NF90_PUT_VAR(ncid, vid, v, start, counter, stride)
  if (ierr.NE.NF90_NOERR) stop 'err write v'
  
  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'wrote variable: ', trim(var_name)
  ENDIF

END SUBROUTINE nc_write_L3_hist1d_ref

END MODULE nc_write_L3_histograms
