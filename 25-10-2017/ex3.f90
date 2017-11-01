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
