SUBROUTINE nc_global_att_L3(ncid,fname,datetime,st,st_iso,&
     tem,spa,instr, sat, satid, lev, type, yday, daynight, wo, v_prod, proc_v_l2, proc_v_l3)
    !tem,spa,instr, sat, satid, lev, type, yday, daynight, wo)   #TS 2011.04.19

! Description:
!
! For define and write global attributes for L3 - data product.
!
!-----------------------------------------------------------------------
! This software was developed within the ESA DUE GlobVapour Project. 
!
! Copyright 2010, DWD, All Rights Reserved.
!-----------------------------------------------------------------------
!
! Unit Name:           nc_global_att_L3.f90
!
! Created on:          25/06/10
!
! Last Modified on:    August 20, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      November 22, 2010 
!                      / Nadine Schneider, DWD/KU22, nadine.schneider@dwd.de
!                      follow the Metadata Convention Version 1.2
!
! Modifications Log:   19 April 2011
!                      / Theo Steenbergen, DWD/KU22
!                      metadata adapted (title, comment, version, processor with version)
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
 
!INCLUDE 'netcdf.inc'

! Input
  INTEGER,INTENT(IN) :: ncid, wo, yday
  CHARACTER(len=*),INTENT(IN) :: fname, datetime
  CHARACTER(len=*),INTENT(IN) :: st,st_iso, daynight ! system time
  CHARACTER(len=*),INTENT(IN) :: spa, tem, instr, sat, satid, type, lev
  CHARACTER(len=*),INTENT(IN) :: v_prod, proc_v_l2, proc_v_l3   ! #TS 2011.04.19
  
  ! Output
  INTEGER, PARAMETER :: SINGLE = 4
  INTEGER, PARAMETER :: DOUBLE = 8
  
  ! Local
  INTEGER :: ierr
  INTEGER :: nfn, ncon, nfor, ntime, nhis, nins, nftype, ntitle, npar, nvalid
  INTEGER :: nsource, nspa, ntem, nver, nref, ncom, nlev, nproc, nproj, nsel
  INTEGER :: ninstr, nsat, nsatid, ntype
  CHARACTER(len= 75) :: con, for, time,  ins, ftype,  par, valid
  !CHARACTER(len= 75) :: source,  ver, ref, com, proc, proj, sel
  CHARACTER(len= 75) :: source,  ver, ref, com, proj, sel
  CHARACTER(len=256) :: proc
  CHARACTER(len=200) :: his,title
  CHARACTER(len= 4) :: chyday
  
  ! End of header ----------------------------------------------------------
  
  print*, yday

  write(chyday,'(i4)') yday



  !============================================
  ! define special file input
  
  con='CF-1.4'
  for='3.63'                                 !value 3.63 should be written
  time=st                     ! system time
  !  his='created on 23/07/2010 (128) 09:56:12' !system time plus julian day in brackets
  his='created on '//st_iso//' ('//TRIM(chyday)//')' !system time plus julian day in brackets
  ins='Deutscher Wetterdienst (DWD)'
  ftype='product'
  title='ESA CCI Cloud - Cloud properties '//TRIM(tem)//' '//TRIM(type)// & 
       ' from '//TRIM(instr)
  !' from '//TRIM(instr)//'. This data set is a '//daynight//' product.'  #TS 2011.04.19
  par='MISC'
  valid=datetime
  source='satellite observations'
  !ver='1.0'   #TS 2011.04.19
  ver=v_prod
  ref='ESA CCI web site (http://www.esa-cloud-cci.info/)'
  !com='Validated with DDS version 1.0'  #TS 2011.04.19
  com=daynight//' product'
  !proc='GlobVapour WV_SSMI_MERIS processor v1.0 operated in hybrid mode'  #TS 2011.04.19
  proc='ESA CCI Cloud ORAC L2 v' // proc_v_l2 // ' operated in ??? mode; ' //  &
       'ESA CCI Cloud L3: v' // proc_v_l3
  !  proc='GlobVapour TCWV SSM/I L2: 1DVAR v' // proc_v_l2 // ' operated in hybrid mode; ' //  &
  !       'SSM/I L3: v' // proc_v_l3
  proj='rectangular grid'
  !sel='descending orbits, satellite f13 and f14'  #TS 2011.04.19
  sel='various orbits'
  
  nfn=LEN_TRIM(fname)
  ncon=LEN_TRIM(con)
  nfor=LEN_TRIM(for)
  ntime=LEN_TRIM(time)
  nhis=LEN_TRIM(his)
  nins=LEN_TRIM(ins)
  nftype=LEN_TRIM(ftype)
  ntitle=LEN_TRIM(title)
  npar=LEN_TRIM(par)
  nvalid=LEN_TRIM(valid)
  nsource=LEN_TRIM(source)
  nspa=LEN_TRIM(spa)
  ntem=LEN_TRIM(tem)
  nver=LEN_TRIM(ver)
  nref=LEN_TRIM(ref)
  ncom=LEN_TRIM(com)
  nlev=LEN_TRIM(lev)
  nproc=LEN_TRIM(proc)
  nproj=LEN_TRIM(proj)
  ninstr=LEN_TRIM(instr)
  nsat=LEN_TRIM(sat)
  nsatid=LEN_TRIM(satid)
  ntype=LEN_TRIM(type)
  nsel=LEN_TRIM(sel)

!============================================
  
!GLOBAL ATTRIBUTES

  ierr = NF90_REDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error redef '

  !     ierr = NF_SET_FILL(ncid, NF_FILL_FLOAT)
 !     IF (ierr.NE.NF_NOERR) STOP 'write fill-mode'

  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'Conventions',con)
  IF (ierr.NE.NF90_NOERR) stop 'error def conventions'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'format',for)
  IF (ierr.NE.NF90_NOERR) stop 'error def format'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'filename',fname)
  IF (ierr.NE.NF90_NOERR) stop 'error def filename'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'filetype',ftype)
  IF (ierr.NE.NF90_NOERR) stop 'error def filetype'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'timestamp',time)
  IF (ierr.NE.NF90_NOERR) stop 'error def timestamp'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'history',his)
  IF (ierr.NE.NF90_NOERR) stop 'error def history'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'institution',ins)
  IF (ierr.NE.NF90_NOERR) stop 'error def institution'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'title',title)
  IF (ierr.NE.NF90_NOERR) stop 'error def title'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'source',source)
  IF (ierr.NE.NF90_NOERR) stop 'error def source'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'sat_series',sat)
  IF (ierr.NE.NF90_NOERR) stop 'error def sat_series'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'sat_id',satid)
  IF (ierr.NE.NF90_NOERR) stop 'error def sat_id'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'instrument',instr)
  IF (ierr.NE.NF90_NOERR) stop 'error def instrument'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'level',lev)
  IF (ierr.NE.NF90_NOERR) stop 'error def level'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'selection',sel)
  IF (ierr.NE.NF90_NOERR) stop 'error def selection'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'parameter',par)
  IF (ierr.NE.NF90_NOERR) stop 'error def parameter'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'validity',valid)
  IF (ierr.NE.NF90_NOERR) stop 'error def validity'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'processor',proc)
  IF (ierr.NE.NF90_NOERR) stop 'error def processor'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'projection',proj)
  IF (ierr.NE.NF90_NOERR) stop 'error def projection'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'spatial',spa)
  IF (ierr.NE.NF90_NOERR) stop 'error def spatial'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'temporal',tem)
  IF (ierr.NE.NF90_NOERR) stop 'error def temporal'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'type',type)
  IF (ierr.NE.NF90_NOERR) stop 'error def type'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'version',ver)
  IF (ierr.NE.NF90_NOERR) stop 'error def version'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'references',ref)
  IF (ierr.NE.NF90_NOERR) stop 'error def references'
  ierr = NF90_PUT_ATT(ncid, NF90_GLOBAL, 'comment',com)
  IF (ierr.NE.NF90_NOERR) stop 'error def comment'



  ierr = NF90_ENDDEF(ncid)
  IF (ierr.NE.NF90_NOERR) stop 'error enddef '


  IF (wo.EQ.1) THEN
     write(*,*) ''
     write(*,*) 'defined global attibutes: ',trim(fname)
  ENDIF
  
  
  RETURN
  
END SUBROUTINE nc_global_att_L3



