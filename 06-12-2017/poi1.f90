!for drichilet boundary conditions only
!with 2nd order central finite difference scheme
!compile with
!gfortran modtrisolve.o poi1.f90 -o poi1.exe -fcheck=all
program poisson
use modtrisolve

integer, parameter :: n=10
real(8) :: x(n),y(n),bsolve(n-1),ysolve(n-1),asolve(n-1,n-1),a(n-1),b(n-1),c(n-1)
real(8) :: h,lb,ub,lbc,ubc

!range of x
lb=0.0
ub=1.0

!boundary conditions
lbc=0.0
ubc=1.0

h=(ub-lb)/n


do i=1,n
  if (i==1) then
    x(i)=0
  else
    x(i)=i*h
  end if
end do
!print *,x

!write function here
y=-1*x
!print *, y

bsolve(1)=y(2)-lbc
do i=2,n-2
  bsolve(i)=y(i+1)
end do
bsolve(n-1)=y(n)-ubc
bsolve=h*h*bsolve

!print *, bsolve/(h*h)

! a is the lower diagonal of the tridiagonal matrix
! b is the main diagonal of the tridiagonal matrix
! c is the upper diagonal of the tridiagonal matrix
a=1.0d0
b=-2.0d0			
c=1.0d0
!print*, a,b,c

!solve for ysolve
call trisolve(a,b,c,bsolve,ysolve,n-1)

print *, ysolve

end program
 
 
!tested on GNU Fortran (Ubuntu 4.8.4-2ubuntu1~14.04.3) 4.8.4 
!output for the values mentioned above
!2.2000000000000006E-002   4.2000000000000010E-002   5.9000000000000011E-002   7.2000000000000022E-002   8.0000000000000016E-002   8.2000000000000017E-002   7.7000000000000013E-002   6.4000000000000001E-002   4.2000000000000010E-002
