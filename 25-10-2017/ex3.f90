<<<<<<< HEAD
program
implicit none

  integer ( kind = 4 ) mx

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) xd(mx+1)
  real ( kind = 8 ) xi
  real ( kind = 8 ) yi

  yi = 1.0D+00

  if ( xi /= xd(i) ) then
    do j = 1, mx + 1
      if ( j /= i ) then
        yi = yi * ( xi - xd(j) ) / ( xd(i) - xd(j) )
      end if
    end do
  end if
=======
program ex3
use ex2
implicit none
  
  integer,parameter :: mx=4
  integer,parameter :: my=4
  integer,parameter :: ni=1

  integer :: i
  integer :: j
  integer :: k
  integer :: l!,c
  real(8) :: lx,temp
  real (8):: ly
  real (8):: xd_1d(mx+1),xd(mx+1)
  real(8) :: xi!(ni)
  real (8):: yd_1d(my+1),yd(my+1)
  real(8) :: yi!(ni)
  real (8):: zd(mx+1,my+1)
  real (8):: zi(ni)

!xd_1d=(/-1.0 , -0.5,  0.0 ,  0.5,1.0/)
!xd_1d=xd
!yd_1d=(/0.03846154,  0.13793103,  1.0,  0.13793103,0.03846154/)
!yd_1d=yd
xd=(/0.03846154,  0.13793103,  1.0,  0.13793103,0.03846154/)

xi=0.02


!call lagrange_interp_2d ( mx, my, xd_1d, yd_1d, zd, ni, xi, yi, zi )
call lagrange_basis_function_1d ( mx, xd, i, xi, yi ) 

print *,xi, yi
!print *, zi






end program
>>>>>>> 6ec7f52fac502325394c2ad61d450a15a120badb
