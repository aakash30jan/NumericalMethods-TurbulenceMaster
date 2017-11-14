program tridiagonal_matrix
	implicit none
	integer :: i,j,k,n,l
	real(8) ::error,m(1000)
  real(8), allocatable:: a(:),b(:),c(:),d(:),x(:),xexact(:),A_Main(:,:),x_ran(:)
	! a is the lower diagonal of the tridiagonal matrix
	! b is the main diagonal of the tridiagonal matrix
	! c is the uppar diagonal of the tridiagonal matrix
	! d is the right hand side of the equations of the tridiagonal matrix
  !x is the numerical value calculated from the thomas algorithm.
  
  do i=1,size(m)	
    !m(i)=10**i
    m(i)=10*i
  end do
  
  do l=1,size(m)
  n=m(l)
  allocate(a(n),b(n),c(n),d(n),x(n),xexact(n),A_Main(n,n),x_ran(n))

	a=-1.0d0
	a(1)=0.0d0	!first element of the a shold be zero
	b=4.0d0			! all elements of b are identical
	c=-1.0d0
	c(n)=0.0d0	!last element of c must be zero
  
  !from the class example
  !d=(/3.0d0,4.0d0,6.0d0,13.0d0/)
	!d(1)=2.0d0 
	!d(2)=4.0d0 
	!d(3)=6.0d0 
	!d(n)=13.0d0

	call random_number(x_ran) !calling random variables in to array x_ran
	!print*,x_ran
	A_Main=0.0d0
	!do i=1,n
	!	A_Main(i,i)=b(1)
	!	A_Main(i,i+1)=c(1)
	!	A_Main(i+1,i)=a(2)
	!End do 	

  do i=2,n
		A_Main(i-1,i-1)=b(1)
		A_Main(i-1,i)=c(1)
		A_Main(i,i-1)=a(2)
	End do 


	d=matmul(A_main,x_ran)	!Ax=b=d......Matrix multiplication of Matrix A with random x
	xexact(1)=1.0d0  ! xexact is the exact value of x for the class exercise
	xexact(2)=2.0d0
	xexact(3)=3.0d0	
	xexact(n)=4.0d0	
	call  triSolve(a,b,c,d,x,xexact,n) 
	error=0.0d0
	do i=1,n
		error=error+dabs(x(i)-x_ran(i)) !error between the random x and the x calculated through Thomas Algorithm
	end do
	!print*,"Error=", error
  print*,n,"    ",error
	!print*,x
  !open(unit=25,file="xRan_xNum.dat")
	!do i=1,n
	!	write(25,*) x_ran(i),x(i)
	!end do 
	!close(25)


	

 deallocate(a,b,c,d,x,xexact,A_Main,x_ran)

 end do !for l loop 

contains
  !subroutine to caluculate the x values from AX=d system
  !to compute the new coeficients b,d, and numerical value of x
	subroutine triSolve(a,b,c,d,x,xexact,n) 
		implicit none
		integer::i
		integer,intent(IN) ::n
		real(8) ::a(n),b(n),c(n),d(n),x(n),xexact(n)
		do i=2,n
			b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
			d(i)=d(i)-((a(i)/b(i-1))*d(i-1))
		end do
			x(n)=d(n)/b(n)
		do i=n-1,1,-1
			x(i)=(d(i)-(c(i)*x(i+1)))/b(i) !Numerical values calculated using thomas algoritham
		end do
	end subroutine triSolve
end program
