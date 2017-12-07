!for mixed boundary conditions 
!with 2nd order central finite difference scheme
!compile with
!gfortran modtrisolve.o poi2.f90 -o poi2.exe -fcheck=all
program poisson
use modtrisolve

implicit none
integer :: m(10)
real(8),allocatable :: x(:),y(:),bsolve(:),ysolve(:),a(:),b(:),c(:), yexact(:), error(:)
real(8) :: h,lb,ub,lbc,ubc
integer :: i,n,j

do i=1,10
m(i)=10**i
end do

!range of x
lb=1.d0 
ub=0.d0

!boundary conditions
lbc=0.d0 !Newman boundary condition u'(a)=alpha in my notes
ubc=0.d0 !Drichilet boundary condition u(b)=ub in my notes

do j=1,size(m)
n=m(j)

allocate(x(n),y(n),bsolve(n),ysolve(n),a(n),b(n),c(n), yexact(n), error(n))


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

yexact=((-x**3.0d0)/6.0d0)+(x/2.0d0)
!print *, yexact
error=yexact-ysolve

!do i=1,size(ysolve)
!        print *, x(i), yexact(i), ysolve(i), error(i)
!end do

error=abs(sum(error)/size(error))
!print *, error
print *, size(error), error(1)

deallocate(x,y,bsolve,ysolve,a,b,c, yexact, error)
end do

end program
 
 
!tested on GNU Fortran (Ubuntu 4.8.4-2ubuntu1~14.04.3) 4.8.4 
!aero@aero-dell-3542:~/workdir/ClassWork-TurbulenceMaster/06-12-2017$ ./poi4.exe 
!          10   3.1083333333333279E-002
!         100   3.3235833333334101E-003
!        1000   3.3324858333332958E-004
!       10000   3.3332498582873371E-005
 !     100000   3.3333249982917387E-006
!     1000000   3.3333324805877817E-007
!    10000000   3.3333318779705587E-008
!   100000000   3.3333073432486560E-009
!^C

