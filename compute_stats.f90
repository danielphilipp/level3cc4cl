! Name: compute_stats.f90
!
!
! Purpose: File contains several subroutines to compute statistics
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
!2012/02/10 Matthias Jerg produces first version
!2014/11/19 Oliver Sus: minor editing
!2015/02/27 S. Stapelberg: removed weighted means and errors, some clean up
!2015/03/30 CP added uncertainty information
!2015/07/10 SteSta: avoid sqrt of negative numbers
!2015/12/16 OS: removed mean_error2; now only calculates correlated uncertainty 
!               when std and std_error are not fill values

! $Id$
!
! Bugs:
!
!none known

!------------------------------------
!------------------------------------
! subroutine compute_stats_stage1(value,error,mean,std,mean_error,std_error,wmean,wstd,v1,v2)
subroutine compute_stats_stage1(value,error,mean,std,mean_error,std_error)
  !------------------------------------
  !------------------------------------

  use vartypes

  use structures

  implicit none

  real(kind=sreal) :: value,error
  real(kind=sreal) :: mean,mean_error,std_error!,wmean,wstd,v1,v2,dummy_sreal,dummy_sreal2
  real(kind=sreal) :: std

  !just build sums for means and standard deviations
  mean=mean+value
  std=std+value**2.0
  mean_error=mean_error+error
  std_error=std_error+error**2.0

end subroutine compute_stats_stage1

!------------------------------------
!------------------------------------

! subroutine compute_stats_stage2(mean,std,mean_error,std_error,wmean,wstd,v1,v2,npoints,mean_error_sq)
subroutine compute_stats_stage2(mean,std,mean_error,std_error,prop_uncertainty,correlated_uncertainty,npoints)

  !------------------------------------
  !------------------------------------

  use vartypes

  use structures

  implicit none

  !   real(kind=sreal) :: mean,std,mean_error,std_error,wmean,wstd,v1,v2,mean_error_sq
  real(kind=sreal) :: mean,std,mean_error,std_error,prop_uncertainty,mean_error2,correlation,correlated_uncertainty,mean_error_sq

  integer(kind=lint) :: npoints

  !This is currenyly a guestimate
  !
  correlation=0.1

  !f
  !form the unweighted averages, std and std_error are so far the means of the squares
  if(npoints .gt. 0) then
     mean=mean/npoints
     std=std/npoints
     !     mean_error=mean_error/npoints
     !mean_error2=sqrt( max( mean_error, 0. ) ) 

     mean_error = mean_error/npoints

     !prop_uncertainty is the propagated uncertainty should tend to zero for many observations: output this
     prop_uncertainty = sqrt( max( std_error, 0. ) ) / npoints

     std_error=std_error/npoints

     !now form standard deviation according to std=sqrt(<x^2>-<x>^2)
     if(std-mean**2.0 .ge. 0.00) then
        std=sqrt(std-mean**2.0)
     else
        std=real_fill_value
     endif
     
     !same as above
     if(std_error-mean_error**2.0 .ge. 0.00) then
        std_error=sqrt(std_error-mean_error**2.0)
     else
        std_error=real_fill_value
     endif

     ! Calculate the likely case that we have correlated errors: output this.
     ! now calculate true (correlated) uncertainty including (at the moment) 
     ! arbitrary correlation until properly analysed
     ! in Bennartz note 0.1 is used, so we use that for now
     if ((std .ne. real_fill_value) .and. (std_error .ne. real_fill_value)) then
        correlated_uncertainty = sqrt( max( std**2. - ( 1.-correlation) * std_error, 0. ) )
     else
        correlated_uncertainty = real_fill_value
     endif

  else
     mean=real_fill_value
     std=real_fill_value
     mean_error=real_fill_value
     std_error=real_fill_value
     ! mean_error_sq=real_fill_value
     prop_uncertainty=real_fill_value
     correlated_uncertainty = real_fill_value
     !      mean_error_sq=real_fill_value
  endif

end subroutine compute_stats_stage2
