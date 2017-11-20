program tridiagonal_matrix
	implicit none
	integer :: i,j,k
	integer,parameter ::n=90
	real(8) ::a(n),b(n),c(n),d(n),x(n),xexact(n),error,A_Main(n,n),x_ran(n)
	
	a=-1.0d0
	a(1)=0.0d0	
	b=4.0d0			
	c=-1.0d0
	c(n)=0.0d0	
  
  
	call random_number(x_ran) 
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


	d=matmul(A_main,x_ran)	
	
	call  triSolve(a,b,c,d,x,xexact,n) 
	error=0.0d0
	do i=1,n
		error=error+dabs(x(i)-x_ran(i)) 
	end do
	print*,"Error=", error
	
contains
  !
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
			x(i)=(d(i)-(c(i)*x(i+1)))/b(i) 
		end do
	end subroutine triSolve
	
end program
