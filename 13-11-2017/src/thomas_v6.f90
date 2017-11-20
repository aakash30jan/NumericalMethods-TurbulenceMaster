program tridiagonal_matrix
use module_triSolve
	implicit none
	integer :: i,j,k,p,n
	!define the size of array of number of discritizations
  integer::m(1000)
  real(8):: error
  real(8), allocatable:: a(:),b(:),c(:),d(:),x(:),aBig(:,:),xRand(:)

	! a is lower diagonal of the tridiagonal matrix
	! b is main diagonal of the tridiagonal matrix
	! c is upper diagonal of the tridiagonal matrix
	! d is right hand side of the equations of the tridiagonal matrix
	! x is numerical value calculated from the thomas algorithm
	! m is array of number of discritizations
	
  do i=1,size(m)
  !defining the style of number of discritizations
  m(i)=10*i 
  end do
  !print *, m
  
  
  do p=1,size(m)
  n=m(p)
  allocate (a(n),b(n),c(n),d(n),x(n),aBig(n,n),xRand(n))
	a=-1.0d0
	a(1)=0.0d0	
	b=2.0d0			
	c=-1.0d0
	c(n)=0.0d0	
  !d=(/2.0,4.0,6.0,13.0/)  
  
	aBig=0.0d0
  do i=2,n
		aBig(i-1,i-1)=b(1)
		aBig(i-1,i)=c(1)
		aBig(i,i-1)=a(2)
	end do 
	
  !call  triSolve(a,b,c,d,x,n)
  !print*,x
 
  call random_number(xRand)
  !compute matrix multiplication of matrix A with random number matrix x i.e Ax=d
  d=matmul(aBig,xRand)	
	!calling the subroutine to compute the x values from Ax=d system through Thomas Algorithm
	call  triSolve(a,b,c,d,x,n) 
	!print*,x
	
	!error between the xRand and the x computed through Thomas Algorithm
	error=(sum(x(1:n-1))/(size(x)-1))-(sum(xRand(1:n-1))/(size(x)-1))
	error=abs(error)
	!print*,"Error=", error
	print*,n, error
	!do i=1,n-1
	!	print* , xRand(i),x(i)
	!end do 
 
  deallocate(a,b,c,d,x,aBig,xRand)
  end do !for p loop




end program
		
