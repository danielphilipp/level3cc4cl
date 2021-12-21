! Name: vartypes.f90
!
!
! Purpose: F90 Module file which declares variable types and some parameters.
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
!2013/01/10 Matthias Jerg makes minor changes to two constants.
!2013/06/11 MJ implemented code to enable writing of netcdf-4 output files.
!2015/02/27 SST and OS added various new variables, some clean up
!2015/03/20 CP adds cloud albedo
!2015/07/10 OS added stint_fill_value + d2r
!2016/02/18 OS added variable defining time reference date
!2016/03/04 OS added COT histogram bin
!2016/03/18 OS stint_fill_value changed to -32767

! $Id$
!
! Bugs:
!
!none known

module vartypes

  implicit none

  integer, parameter :: currentlength=512
  integer, parameter :: cpathlength=256
  integer, parameter :: filelength=512!128
  integer, parameter :: commandlength=64
  integer, parameter :: cfilelength=32
  integer, parameter :: paramlength=16
  integer, parameter :: dummylength=128
  integer, parameter :: varlength=64
  integer, parameter :: unitlength=75
  integer, parameter :: inlength=10
  integer, parameter :: uuid_length=36
  integer, parameter :: attribute_length=75
  integer, parameter :: l3_outputpath_and_file_length=512
  integer, parameter :: description_length=2048
  integer, parameter :: MaxStateVar = 5  ! Max. number of state variables

  integer, parameter :: sint=1
  integer, parameter :: stint=2
  integer, parameter :: lint=4

  integer, parameter :: sreal=4
  integer, parameter :: dreal=8

  real(kind=sreal), parameter :: real_fill_value=-999.0, filter_thres=-500.0, filter_micro=0.00, &
       & dither=1.0E-5, norm_factor=1.0E-3,dither3=1.0E-3
  integer(kind=lint), parameter :: long_int_fill_value=-999, long_int_zero=0,min_num_values=1,min_num_values_closure=50
  integer(kind=sint), parameter :: short_int_fill_value=-99
  integer(kind=stint), parameter :: nint_fill_value=-999, n_int_zero=0,stint_fill_value=-32767
  real(kind=dreal), parameter :: double_fill_value=-999.0

  real(kind=sreal), parameter :: ctp_high=440.0, ctp_middle=680.0

  integer(kind=lint), parameter :: n_features=10,n_error_features=1,n_variables=10

  integer(kind=stint), parameter :: n_hist_phase=2,n_hist_cot=14,n_hist_ctp=16

  integer(kind=stint), parameter :: n_cot_bins=14,n_ctp_bins=15, n_ctt_bins=16, n_cwp_bins=14, n_ref_bins=11
   integer(kind=stint), parameter :: n_cloud_albedo1_bins=13,n_cloud_albedo2_bins=13

! old changed now to cm saf definition
!   integer(kind=stint), parameter :: n_hist_phase=2,n_hist_cot=7,n_hist_ctp=8
! 
!   integer(kind=stint), parameter :: n_cot_bins=11,n_ctp_bins=10, n_ctt_bins=12
   
  integer(kind=stint) :: n_remedian_base, n_remedian_exponent

  real(kind=sreal), parameter :: e_radius=6371.0

  real(kind=dreal), parameter :: pi = 4 * atan(1.0d0)
  real(kind=dreal), parameter :: d2r = pi/180.0d0

  real(kind=sreal), parameter :: min_range=12.0

!  integer(kind=stint), parameter :: n_remedian_base=101, n_remedian_exponent=3 !1.0 deg
!  integer(kind=stint), parameter :: n_remedian_base=37, n_remedian_exponent=3 !0.5 deg???
!  integer(kind=stint), parameter :: n_remedian_base=7, n_remedian_exponent=3 !0.1 deg???
!  integer(kind=stint), parameter :: n_remedian_base=17, n_remedian_exponent=3 !0.2 deg
!  integer(kind=stint), parameter :: n_remedian_base=43, n_remedian_exponent=3 !0.25 deg. res

  !some netcdf4 related parameters
  !compression levels for variables of different type (0:none,9:maximum)
!!$  integer(kind=lint), parameter :: compress_level_float=7
!!$  integer(kind=lint), parameter :: compress_level_double=7
!!$  integer(kind=lint), parameter :: compress_level_lint=7
!!$  integer(kind=lint), parameter :: compress_level_nint=7
!!$  integer(kind=lint), parameter :: compress_level_stint=7
!!$  integer(kind=lint), parameter :: compress_level_byte=7
!!$  integer(kind=lint), parameter :: compress_level_stint_flag=7

!  integer(kind=lint), parameter :: compress_level_float=0
!  integer(kind=lint), parameter :: compress_level_double=0
!  integer(kind=lint), parameter :: compress_level_lint=0
!  integer(kind=lint), parameter :: compress_level_nint=0
!  integer(kind=lint), parameter :: compress_level_stint=0
!  integer(kind=lint), parameter :: compress_level_byte=0
!  integer(kind=lint), parameter :: compress_level_stint_flag=0

!!$  integer(kind=lint), parameter :: compress_level_float=9
!!$  integer(kind=lint), parameter :: compress_level_double=9
!!$  integer(kind=lint), parameter :: compress_level_lint=9
!!$  integer(kind=lint), parameter :: compress_level_nint=9
!!$  integer(kind=lint), parameter :: compress_level_stint=9
!!$  integer(kind=lint), parameter :: compress_level_byte=0
!!$  integer(kind=lint), parameter :: compress_level_stint_flag=0

  integer(kind=lint), parameter :: compress_level_float=9
  integer(kind=lint), parameter :: compress_level_double=9
  integer(kind=lint), parameter :: compress_level_lint=9
  integer(kind=lint), parameter :: compress_level_nint=9
  integer(kind=lint), parameter :: compress_level_stint=9
  integer(kind=lint), parameter :: compress_level_byte=9
  integer(kind=lint), parameter :: compress_level_stint_flag=9
!!$
  !turn on shuffling to improve compression
  logical, parameter :: shuffle_float=.TRUE.
  logical, parameter :: shuffle_double=.TRUE.
  logical, parameter :: shuffle_lint=.TRUE.
  logical, parameter :: shuffle_nint=.TRUE.
  logical, parameter :: shuffle_stint=.TRUE.
  logical, parameter :: shuffle_byte=.TRUE.
  logical, parameter :: shuffle_stint_flag=.TRUE.

!  logical, parameter :: shuffle_float=.FALSE.
!  logical, parameter :: shuffle_double=.FALSE.
!  logical, parameter :: shuffle_lint=.FALSE.
!  logical, parameter :: shuffle_nint=.FALSE.
!  logical, parameter :: shuffle_stint=.FALSE.
!  logical, parameter :: shuffle_byte=.FALSE.
!  logical, parameter :: shuffle_stint_flag=.FALSE.


  !chunking array for internal file partitioning
  !integer(kind=lint), parameter, dimension(3) :: chunksize3d=(/180,90,1/)
  !for the 2D histogram
  integer(kind=lint),dimension(2) :: chunksize1d_profile
  integer(kind=lint),dimension(6) :: chunksize6d

  real(kind=dreal) :: jul_start = 2440587.5d0 ! julian day from 01.01.1970 00:00:00

end module vartypes
