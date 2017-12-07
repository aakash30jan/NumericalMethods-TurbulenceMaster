!for mixed boundary conditions 
!with 2nd order central finite difference scheme
!compile with
!gfortran modtrisolve.o poi2.f90 -o poi2.exe -fcheck=all
program poisson
use modtrisolve

implicit none
integer, parameter :: n=5000
real(8) :: x(n),y(n),bsolve(n),ysolve(n),a(n),b(n),c(n), yexact(n), error(n)!,asolve(n,n) print asolve if needed
real(8) :: h,lb,ub,lbc,ubc
integer :: i

!range of x
lb=1.d0 
ub=0.d0

!boundary conditions
!lbc=0.d0 !Newman boundary condition u'(a)=alpha in my notes
!ubc=0.d0 !Drichilet boundary condition u(b)=ub in my notes
lbc=5.817014 !Newman boundary condition u'(a)=alpha in my notes
ubc=1.d0 !Drichilet boundary condition u(b)=ub in my notes

h=(ub-lb)/n


do i=1,n,1
  if (i==1) then
    x(i)=lb
  else
    x(i)=lb+((i-1)*h)
  end if
end do
!print *,x   !some problem with accuracy of x


!write function here
!y=-1.0d0*x
y=2.0d0*exp(sqrt(2.00)*x)
!print *, y

bsolve(1)=(y(1)/2.0d0)+(lbc/h) !artistically brewed 
do i=2,n-1
  bsolve(i)=y(i+1)
end do
bsolve(n)=y(n)-(ubc/h*h) !brewed to get high 

bsolve=h*h*bsolve

!print *, bsolve/(h*h)

! a is the lower diagonal of the tridiagonal matrix
! b is the main diagonal of the tridiagonal matrix
! c is the upper diagonal of the tridiagonal matrix
a=-1.0d0
b=2.0d0	
b(1)=1.0d0		
c=-1.0d0
!print*, a,b,c

!solve for ysolve
call trisolve(a,b,c,bsolve,ysolve,n)

ysolve=-1.0d0*ysolve !my solve is returning negative values of the actual numerical value 
!I have figured out the problem which is in my reverse way of dealing with the range of x
!print *, ysolve

!yexact=((-x**3.0d0)/6.0d0)+(x/2.0d0)
yexact=exp(1.4142136*x)
!print *, yexact
error=yexact-ysolve

do i=1,size(ysolve)
        print *, x(i), yexact(i), ysolve(i), error(i)
end do

error=abs(sum(error)/size(error))
!print *, error
print *, size(error), error(1)


end program
 
 
!tested on GNU Fortran (Ubuntu 4.8.4-2ubuntu1~14.04.3) 4.8.4 
