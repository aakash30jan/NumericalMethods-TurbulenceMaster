!for mixed boundary conditions 
!with 1st order non-central finite difference scheme
!compile with
!gfortran modtrisolve.o poi3.f90 -o poi3.exe -fcheck=all
program poisson
use modtrisolve

implicit none
integer, parameter :: n=5
real(8) :: x(n),y(n),bsolve(n),ysolve(n),a(n),b(n),c(n), yexact(n), error(n)!,asolve(n,n) print asolve if needed
real(8) :: h,lb,ub,lbc,ubc
integer :: i

!range of x
lb=1.d0 
ub=0.d0

!boundary conditions
lbc=0.d0 !Newman boundary condition u'(a)=alpha in my notes
ubc=0.d0 !Drichilet boundary condition u(b)=ub in my notes

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
y=-1.0d0*x
!print *, y

bsolve(1)=lbc/h !simply brewed 
do i=2,n-1
  bsolve(i)=y(i+1)
end do
bsolve(n)=y(n)-(ubc/h*h) !brewed to get high 

bsolve=h*h*bsolve

!print *, bsolve/(h*h)

! a is the lower diagonal of the tridiagonal matrix
! b is the main diagonal of the tridiagonal matrix
! c is the upper diagonal of the tridiagonal matrix
a=0.0d0
b=1.0d0	
b(1)=0.0d0		
c=-1.0d0
!print*, a,b,c

!solve for ysolve
call trisolve(a,b,c,bsolve,ysolve,n)

ysolve=-1.0d0*ysolve !my solve is returning negative values of the actual numerical value 
!I have figured out the problem which is in my reverse way of dealing with the range of x
print *, ysolve

yexact=((-x**2.0d0)/2.0d0)+(1.0d0/2.0d0)
print *, yexact
error=yexact-ysolve
error=abs(sum(error)/size(error))
!print *, error
print *, size(error), error(1)


end program
 
 
!tested on GNU Fortran (Ubuntu 4.8.4-2ubuntu1~14.04.3) 4.8.4 
