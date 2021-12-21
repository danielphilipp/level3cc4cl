! Name: nc_read_file.f90
!
!
! Purpose: File contains subroutines to read netcdf files for various variable types.
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
!2012/02/09 Matthias Jerg adds routines for ORAC.
!2014/11/19 Oliver Sus changes format of standard output write statement for variable sf
!2015/02/27 OS replaced double loops with WHERE statements; added SR nc_read_l2b_sum_2d_long_to_long,
!  SR nc_read_l2b_sum_4d_long_to_long, SR nc_read_l2b_sum_4d_short_to_long, SR nc_read_l2b_sum_5d_short_to_long,
!  SR nc_read_l2b_sum_2d_float_to_float
!2015/04/23 OS added SR nc_read_l2b_sum_5d_long_to_long and a few write statements if verbose
!2015/12/16 OS added SR nc_read_l2b_sum_2d_double_to_double
!2017/06/22 OS minor edit
!
! $Id$
!
! Bugs:
!
!none known

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_float(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!did,
  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v(1:n2,1:n1)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n2
  counter(2) = n1
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq a v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF



  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF

  !      write(*,*) fv, vmin,vmax,os,sf
  !      pause

  !       DO i=1,n1                                            !question: outlier /outliar
  !          DO j=1,n2
  !             IF ( v(j,i).EQ.fv .or. (v(j,i)*sf+os) .lt. vmin .or. (v(j,i)*sf+os) .gt. vmax ) THEN
  !                v_out(i,j)=real_fill_value
  !             ELSE
  !                v_out(i,j)=(v(j,i)*sf)+os                !scaling and form to real format
  !             ENDIF
  !          ENDDO
  !       ENDDO

  WHERE ( v .EQ. fv .or. (v * sf + os) .lt. vmin .or. (v * sf + os ) .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_2d_float

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_float_orac(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  !MJOLD      real(kind=sreal) :: v(1:n2,1:n1)
  real(kind=sreal) :: v(1:n1,1:n2)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  !MJOLD      counter(1) = n2
  !MJOLD      counter(2) = n1
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq b v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF



  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF

  !      write(*,*) fv, vmin,vmax,os,sf
  !      pause


  !      DO i=1,n1                                            !question: outlier /outliar
  !         DO j=1,n2
  !            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
  !               v_out(i,j)=real_fill_value
  !            ELSE
  !               v_out(i,j)=(v(i,j)*sf)+os                !scaling and form to real format
  !            ENDIF
  !         ENDDO
  !      ENDDO

  !      write(*,*) "min/max in nc_read_array_2d_float_orac using loop = ", minval(v_out), maxval(v_out)

  WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  if (wo.eq.1) write(*,*) "min/max = ", minval(v_out), maxval(v_out)

  RETURN

END SUBROUTINE nc_read_array_2d_float_orac

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_array_1d_float_orac(ncid,n1,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(1), counter(1),stride(1)!, did
  REAL,PARAMETER :: miss=-9999.
  !MJOLD      real(kind=sreal) :: v(1:n2,1:n1)
  real(kind=sreal) :: v(1:n1)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  counter(1) = n1
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq  c v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF



  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF

  !      write(*,*) fv, vmin,vmax,os,sf
  !      pause

  !       DO i=1,n1                                            !question: outlier /outliar
  !          IF ( v(i) .EQ.fv .or. v(i) .lt. vmin .or. v(i) .gt. vmax ) THEN
  !             v_out(i)=real_fill_value
  !          ELSE
  !             v_out(i)=(v(i)*sf)+os                !scaling and form to real format
  !          ENDIF
  !       ENDDO

  WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_1d_float_orac

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_floatl3d(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v(1:n1,1:n2)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq d v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF



  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=-real_fill_value*real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=real_fill_value*real_fill_value
  ENDIF

  !      write(*,*) fv, vmin,vmax,os,sf
  !      pause

  !       DO i=1,n1                                            !question: outlier /outliar
  !          DO j=1,n2
  !             IF ( v(i,j).EQ.fv .or. (v(i,j)*sf+os) .lt. vmin .or. (v(i,j)*sf+os) .gt. vmax ) THEN
  !                v_out(i,j)=real_fill_value
  !             ELSE
  !                v_out(i,j)=(v(i,j)*sf)+os                !scaling and form to real format
  !             ENDIF
  !          ENDDO
  !       ENDDO

  WHERE ( v .EQ. fv .or. (v * sf + os) .lt. vmin .or. (v * sf + os) .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_2d_floatl3d

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_lon_grid_float(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(1), counter(1),stride(1)!, did
  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v(1:n1)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  counter(1) = n1
  stride(1)=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq e v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF

  do i=1,n1
     do j=1,n2

        v_out(i,j)=v(i)

     enddo
  enddo

  RETURN

END SUBROUTINE nc_read_lon_grid_float

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_lat_grid_float(ncid,n1,n2,cv,v_out,unit,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: fv,os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(1), counter(1),stride(1)!, did
  REAL,PARAMETER :: miss=-9999.
  real(kind=sreal) :: v(1:n1)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  counter(1) = n2
  stride(1)=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq f v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF

  do j=1,n2
     do i=1,n1

        v_out(i,j)=v(j)

     enddo
  enddo

  RETURN

END SUBROUTINE nc_read_lat_grid_float

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_short(ncid,n1,n2,cv,v_out,unit,wo)

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  integer(kind=stint) :: v(1:n2,1:n1),fv

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n2
  counter(2) = n1
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq g v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF


  !   DO i=1,n1                                            !question: outlier /outliar
  !      DO j=1,n2
  !         IF ( v(j,i).EQ.fv .or. (real(v(j,i),kind=sreal)*sf+os) .lt. vmin .or. (real(v(j,i),kind=sreal)*sf+os) .gt. vmax ) THEN
  !            v_out(i,j)=real_fill_value
  !         ELSE
  !            v_out(i,j)=(real(v(j,i),kind=sreal)*sf)+os                !scaling and form to real format
  !         ENDIF
  !      ENDDO
  !   ENDDO

  WHERE ( v .EQ. fv .or. (real(v, kind = sreal) * sf + os) .lt. vmin .or. (real(v, kind = sreal) * sf + os) .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_2d_short

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_short_orac(ncid,n1,n2,cv,v_out,unit,wo)
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: os,sf
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  integer(kind=stint) :: v(1:n1,1:n2),fv,vmin,vmax

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq h v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF

  !      DO i=1,n1                                            !question: outlier /outliar
  !         DO j=1,n2
  !            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
  !               v_out(i,j)=real_fill_value
  !            ELSE
  !               v_out(i,j)=(real(v(i,j),kind=sreal)*sf)+os                !scaling and form to real format
  !            ENDIF
  !         ENDDO
  !      ENDDO

  !     write(*,*) "min/max in nc_read_array_2d_short_orac using loop = ", minval(v_out), maxval(v_out)

  WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  if (wo.eq.1) write(*,*) "min/max = ", minval(v_out), maxval(v_out)  

  RETURN

END SUBROUTINE nc_read_array_2d_short_orac

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_long_orac(ncid,n1,n2,cv,v_out,unit,wo)
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
  !-----------------------------------------------------------------------
  ! This software was developed within the ESA Cloud CCI Project
  ! and is based on routines developed during the
  ! ESA DUE GlobVapour Project. 
  ! Copyright 2011, DWD, All Rights Reserved.
  !-----------------------------------------------------------------------
  !
  ! Unit Name:           nc_create_global_l2.f90
  !
  ! Created on:          2013/01/18
  !                      by Rasmus Lindstrot, FUB
  !                      just a copy from nc_read_array_2d_short_orac with different variable type.
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: os,sf
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, did, i, j,start(2), counter(2),stride(2)
  REAL,PARAMETER :: miss=-9999.
  integer(kind=4) :: v(1:n1,1:n2),fv,vmin,vmax

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq i v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF

  IF (cv.ne.'cc_total') THEN 
     !      DO i=1,n1                                            !question: outlier /outliar
     !         DO j=1,n2
     !            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
     !               v_out(i,j)=real_fill_value
     !            ELSE
     !               v_out(i,j)=(real(v(i,j),kind=sreal)*sf)+os                !scaling and form to real format
     !            ENDIF
     !         ENDDO
     !      ENDDO

     WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
        v_out = real_fill_value
     ELSEWHERE
        v_out = ( real(v, kind = sreal) * sf ) + os
     END WHERE

  ELSE
     !      DO i=1,n1                                            !question: outlier /outliar
     !         DO j=1,n2
     !            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin ) THEN
     !               v_out(i,j)=real_fill_value
     !            ELSE
     !               v_out(i,j)=(real(v(i,j),kind=sreal)*sf)+os                !scaling and form to real format
     !            ENDIF
     !         ENDDO
     !      ENDDO

     WHERE ( v .EQ. fv .or. v .lt. vmin ) 
        v_out = real_fill_value
     ELSEWHERE
        v_out = ( real(v, kind = sreal ) * sf ) + os
     END WHERE

  ENDIF

  RETURN

END SUBROUTINE nc_read_array_2d_long_orac

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_short_to_short_orac(ncid,n1,n2,cv,v_out,unit,wo)
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  !       INTEGER, PARAMETER :: SINGLE = 4
  !       INTEGER, PARAMETER :: DOUBLE = 8
  integer(kind=stint) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  integer(kind=stint) :: os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  !       REAL,PARAMETER :: miss=-9999.
  integer(kind=stint) :: v(1:n1,1:n2),fv

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq j v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1_stint
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0_stint
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=0_stint
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=32000_stint
  ENDIF

  !   DO i=1,n1                                            !question: outlier /outliar
  !      DO j=1,n2
  !         IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
  !            v_out(i,j)=fv
  !         ELSE
  !            v_out(i,j)=v(i,j)*sf+os             
  !         ENDIF
  !      ENDDO
  !   ENDDO

  WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_2d_short_to_short_orac

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_byte(ncid,n1,n2,cv,v_out,unit,wo)
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  real(kind=sreal) :: os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  integer(kind=sint) :: v(1:n2,1:n1),fv

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n2
  counter(2) = n1
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq k v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
  ENDIF

  !   DO i=1,n1                                            !question: outlier /outliar
  !      DO j=1,n2
  !         IF ( v(j,i).EQ.fv .or. (real(v(j,i),kind=sreal)*sf+os) .lt. vmin .or. (real(v(j,i),kind=sreal)*sf+os) .gt. vmax ) THEN
  !            v_out(i,j)=real_fill_value
  !         ELSE
  !            v_out(i,j)=(real(v(j,i),kind=sreal)*sf)+os                !scaling and form to real format
  !         ENDIF
  !      ENDDO
  !   ENDDO

  WHERE ( v .EQ. fv .or. (real(v, kind = sreal) * sf + os) .lt. vmin .or. (real(v, kind = sreal) * sf + os) .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( real(v, kind =sreal) * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_array_2d_byte

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_byte_orac(ncid,n1,n2,cv,v_out,unit,wo)

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  integer(KIND=sint) :: v_out(1:n1,1:n2)    ! Variable (v): read in field 1-n1
  integer(kind=sint) :: os,sf,vmin,vmax
  CHARACTER(LEN=unitlength) :: unit

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)!, did
  REAL,PARAMETER :: miss=-9999.
  integer(kind=sint) :: v(1:n1,1:n2),fv

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  unit=''

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq l v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  !      write(*,*) minval(v),maxval(v)

  ierr = NF90_GET_ATT(ncid, vid, 'units', unit)  !measure range of variable
  IF (wo.EQ.1) THEN
     write(*,*) 'unit: ',TRIM(unit)
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'unit: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: '; write(*,*) sf !write(*,fmt='(f6.2)') sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1_sint
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0_sint
  ENDIF


  ierr = NF90_GET_ATT(ncid, vid, 'valid_min', vmin)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_min: ',vmin
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_min: not defined '
     ENDIF
     vmin=0_sint
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'valid_max', vmax)
  IF (wo.EQ.1) THEN
     write(*,*) 'valid_max: ',vmax
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'valid_max: not defined '
     ENDIF
     vmax=120_sint
  ENDIF


  !      DO i=1,n1                                            !question: outlier /outliar
  !         DO j=1,n2
  !            IF ( v(i,j).EQ.fv .or. v(i,j) .lt. vmin .or. v(i,j) .gt. vmax ) THEN
  !               v_out(i,j)=short_int_fill_value
  !            ELSE
  !               v_out(i,j)=v(i,j)*sf+os                !scaling and form to real format
  !            ENDIF
  !         ENDDO
  !      ENDDO

  !      write(*,*) "min/max in nc_read_array_2d_byte_orac using loop = ", minval(v_out), maxval(v_out)

  WHERE ( v .EQ. fv .or. v .lt. vmin .or. v .gt. vmax ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  if (wo.eq.1) write(*,*) "min/max = ", minval(v_out), maxval(v_out)

  RETURN

END SUBROUTINE nc_read_array_2d_byte_orac

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE nc_read_array_2d_double_orac(ncid,n1,n2,cv,v,wo)
  ! Description:
  !
  ! Creates new netcdf-file. (Output file)
  !
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

  USE vartypes

  IMPLICIT NONE

  !   INCLUDE 'netcdf.inc'

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  !       INTEGER :: n                            ! Dimension of data
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  real(kind=dreal) :: fv!,os,sf,vmin,vmax

  ! Local
  INTEGER :: ierr, vid, start(2), counter(2),stride(2)!, did, i, j,
  REAL,PARAMETER :: miss=-9999.
  !MJOLD      real(kind=sreal) :: v(1:n2,1:n1)
  real(kind=dreal) :: v(1:n1,1:n2)

  ! End of header ----------------------------------------------------------

  !in fortran the inner loop is next to variable
  !e.g. var(n2,n1) n2 is inner loop, n1 is outer loop

  start(1) = 1
  start(2) = 1
  counter(1) = n1
  counter(2) = n2
  !MJOLD      counter(1) = n2
  !MJOLD      counter(2) = n1
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq m v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride) 
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF

END SUBROUTINE nc_read_array_2d_double_orac


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_l2b_sum_2d_long_to_long(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                        ! variable name

  ! Output
  integer(kind=lint) :: v_out(1:n1,1:n2)

  ! Local
  INTEGER :: ierr, vid, start(2), counter(2),stride(2)
  integer(kind=lint) :: v(1:n1,1:n2)
  integer(kind=lint) :: fv, os, sf

  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq n v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv = -999
 ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf = 1
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os = 0
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = long_int_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_2d_long_to_long

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_l2b_sum_4d_long_to_long(ncid,n1,n2,n3,n4,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo                       
  INTEGER(kind=stint),INTENT(IN) :: n1,n2,n3,n4
  CHARACTER(LEN=*) :: cv                              ! variable name

  ! Output
  integer(kind=lint) :: v_out(1:n1,1:n2,1:n3,1:n4)

  ! Local
  INTEGER :: ierr, vid, start(4), counter(4),stride(4)
  integer(kind=lint) :: v(1:n1,1:n2,1:n3,1:n4)
  integer(kind=lint) :: fv, os, sf

  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  counter(3) = n3
  counter(4) = n4
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1,1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv = -999
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf = 1
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os = 0
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = long_int_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_4d_long_to_long

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_l2b_sum_4d_short_to_long(ncid,n1,n2,n3,n4,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo                       
  INTEGER(kind=stint),INTENT(IN) :: n1,n2,n3,n4
  CHARACTER(LEN=*) :: cv                              ! variable name

  ! Output
  integer(kind=lint) :: v_out(1:n1,1:n2,1:n3,1:n4)

  ! Local
  INTEGER :: ierr, vid, start(4), counter(4),stride(4)
  integer(kind=stint) :: v(1:n1,1:n2,1:n3,1:n4)
  integer(kind=stint) :: fv, os, sf

  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  counter(3) = n3
  counter(4) = n4
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1,1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv = -999
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf = 1
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os = 0
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = long_int_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_4d_short_to_long

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_l2b_sum_5d_short_to_long(ncid,n1,n2,n3,n4,n5,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo 
  INTEGER(kind=stint),INTENT(IN) :: n1,n2,n3,n4,n5           ! number of footprints

  CHARACTER(LEN=*) :: cv                                 ! variable name

  ! Output
  integer(kind=lint),INTENT(OUT) :: v_out(1:n1,1:n2,1:n3,1:n4,1:n5)

  ! Local
  INTEGER :: ierr, vid, i, j,start(5), counter(5),stride(5)
  integer(kind=stint) :: v(1:n1,1:n2,1:n3,1:n4,1:n5)
  integer(kind=stint) :: fv, os, sf

  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  counter(3) = n3
  counter(4) = n4
  counter(5) = n5
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1,1,1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv = -999
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf = 1
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os = 0
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = long_int_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_5d_short_to_long

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

SUBROUTINE nc_read_l2b_sum_5d_long_to_long(ncid,n1,n2,n3,n4,n5,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,wo 
  INTEGER(kind=stint),INTENT(IN) :: n1,n2,n3,n4,n5           ! number of footprints

  CHARACTER(LEN=*) :: cv                                 ! variable name

  ! Output
  integer(kind=lint),INTENT(OUT) :: v_out(1:n1,1:n2,1:n3,1:n4,1:n5)

  ! Local
  INTEGER :: ierr, vid, i, j,start(5), counter(5),stride(5)
  integer(kind=lint) :: v(1:n1,1:n2,1:n3,1:n4,1:n5)
  integer(kind=lint) :: fv, os, sf

  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  counter(3) = n3
  counter(4) = n4
  counter(5) = n5
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1,1,1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv = -999
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf = 1
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os = 0
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = long_int_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_5d_long_to_long

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_l2b_sum_2d_float_to_float(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  REAL(KIND=sreal) :: v_out(1:n1,1:n2)

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)
  real(kind=sreal) :: v(1:n1,1:n2),fv,os,sf
  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_2d_float_to_float

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
SUBROUTINE nc_read_l2b_sum_2d_double_to_double(ncid,n1,n2,cv,v_out,wo)

  USE netcdf

  USE vartypes

  IMPLICIT NONE

  ! Input
  INTEGER,INTENT(IN) :: ncid,n1,n2,wo           ! number of footprints
  CHARACTER(LEN=*) :: cv                     ! variable name

  ! Output
  REAL(KIND=dreal) :: v_out(1:n1,1:n2)

  ! Local
  INTEGER :: ierr, vid, i, j,start(2), counter(2),stride(2)
  real(kind=dreal) :: v(1:n1,1:n2),fv,os,sf
  ! End of header ----------------------------------------------------------

  start = 1
  counter(1) = n1
  counter(2) = n2
  stride=1

  ierr = 0

  IF (wo.EQ.1) THEN
     write(*,*) 'read variable: ', cv
  ENDIF

  ierr = NF90_INQ_VARID(ncid, cv, vid)    !get variable ID from variable named cv
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'inq v'
  ENDIF

  ierr = NF90_GET_VAR(ncid, vid,v,start,counter,stride)   !read variable from1-n1 and put into field
  IF (ierr.NE.NF90_NOERR) THEN
     STOP 'get v'
  ENDIF
  IF (wo.EQ.1) THEN
     write(*,*) 'first data value: ', v(1,1)
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, '_FillValue', fv)
  IF (wo.EQ.1) THEN
     write(*,*) '_FillValue: ',fv
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) '_FillValue: not defined '
     ENDIF
     fv=real_fill_value
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'scale_factor', sf)
  IF (wo.EQ.1) THEN
     write(*,*) 'scale_factor: ', sf
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'scale_factor: not defined '
     ENDIF
     sf=1.0
  ENDIF

  ierr = NF90_GET_ATT(ncid, vid, 'add_offset', os)
  IF (wo.EQ.1) THEN
     write(*,*) 'add_offset: ',os
  ENDIF
  IF (ierr.NE.NF90_NOERR) THEN
     IF (wo.EQ.1) THEN
        write(*,*) 'add_offset: not defined '
     ENDIF
     os=0.00
  ENDIF

  WHERE ( v .EQ. fv ) 
     v_out = real_fill_value
  ELSEWHERE
     v_out = ( v * sf ) + os
  END WHERE

  RETURN

END SUBROUTINE nc_read_l2b_sum_2d_double_to_double

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  
