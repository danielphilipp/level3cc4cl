! locate.90
!
! Purpose:
!
!
! Algorithm
!    From Numerical Recipes in Fortran 90 [Press, Flannery]
!    
! History
!    2011/02/03 - Adapted for L3 software by M. Jerg
!
! Bugs
!    None known
!
!---------------------------------------------------------------------

subroutine locate(xx,n,x,j)
  
  use vartypes

  implicit none

  integer(kind=stint) :: j,n
  real(kind=sreal) :: x,xx(n)
  integer(kind=stint) :: jl,jm,ju
  jl=0
  ju=n+1
10 if(ju-jl.gt.1)then
     jm=(ju+jl)/2
     if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
        jl=jm
     else
        ju=jm
     endif
     goto 10
  endif
  if(x.eq.xx(1))then
     j=1
  else if(x.eq.xx(n))then
     j=n-1
  else
     j=jl
  endif
  return
end subroutine locate
