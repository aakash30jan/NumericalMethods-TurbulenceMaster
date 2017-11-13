program main
use calculations

implicit none
	integer :: i,j,k
	integer,parameter ::n=4
	real(8) ::a(n),b(n),c(n),d(n),x(n),A_Main(n,n)

	a=-1.0d0
	a(1)=0.0d0	
	b=4.0d0			
	c=-1.0d0
	c(n)=0.0d0	

	d(1)=2.0d0 
	d(2)=4.0d0 
	d(3)=6.0d0 
	d(n)=13.0d0	 

	A_Main=0.0d0
	do i=1,n
		A_Main(i,i)=b(1)
		A_Main(i,i+1)=c(1)
		A_Main(i+1,i)=a(2)
	End do 	
	
	call  triSolve(a,b,c,d,x,n)

	print*,x

end program
