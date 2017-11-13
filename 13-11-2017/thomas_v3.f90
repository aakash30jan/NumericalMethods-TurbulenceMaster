program tridiagonal_matrix
	implicit none
	integer :: i,j,k
	integer,parameter ::n=4
	real(8) ::a(n),b(n),c(n),d(n),x(n),xexact(n),error,A_Main(n,n),x_ran(n)

	a=-1.0d0
	a(1)=0.0d0	
	b=4.0d0			
	c=-1.0d0
	c(n)=0.0d0	
  
  d=(/2.0,4.0,6.0,13.0/) 
	!d(1)=2.0d0 
	!d(2)=4.0d0 
	!d(3)=6.0d0 
	d(n)=13.0d0	 

	A_Main=0.0d0
	do i=1,n
		A_Main(i,i)=b(1)
		A_Main(i,i+1)=c(1)
		A_Main(i+1,i)=a(2)
	End do 	
	
  call  triSolve(a,b,c,d,x,n)
  print*,x
 
!d=matmul(A_main,x)
	

contains
	subroutine triSolve(a,b,c,d,x,n) 
		implicit none
		integer::i
		integer,intent(IN) ::n
		real(8) ::a(n),b(n),c(n),d(n),x(n)
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
		
