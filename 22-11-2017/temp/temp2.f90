program Poiseuille
	
	implicit none
  integer,parameter :: n=10
	real(8),parameter :: pi=3.1415926535
	real(8),parameter :: lb=0.0d0
	real(8),parameter :: rb=pi
	real(8),parameter :: lbu=1.0d0
	real(8),parameter :: rbu=-1.0d0
	integer :: i,j,k
	real(8)	::x(n),h,u(n),a(n-2),b(n-2),c(n-2),d(n-2),uexact(n),error
	x(1)=lb;  x(n)=rb
	h=(x(n)-x(1))/n
    !print*,"h=",h
	open(unit=25,file="h_Vs_Error.dat",status='old',action='write',form='formatted',position="append")
	do i=2,n-1
		x(i)=h*(i-1)	
	end do
	!print*,x
	u(1)=lbu; u(n)=rbu
	d(1)=((h**2)*(-(cos(x(2)))))-u(1)
	do i=2,n-3
		d(i)=(h**2)*(-(cos(x(i+1))))
	end do
	d(n-2)=((h**2)*(-(cos(x(n-1)))))-u(n)
	b=-2.0d0
	a(1)=0.0d0
	a=1.0d0
	c=1.0d0
	c(n-2)=0.0d0
	call  fw_el(a,b,c,d,u,uexact,n,error)
    do i=1,n
        print *, u(i),uexact(i)
    end do
	
	write(25,"(2ES16.8)"),h,error

	close(25)
contains
	subroutine fw_el(a,b,c,d,u,uexact,n,error) !to compute the new coeficients b,d, and numerical value of x
		implicit none
		integer::i
		integer,intent(IN) ::n
		real(8) ::a(n-2),b(n-2),c(n-2),d(n-2),u(n),uexact(n),error
		do i=2,n-2
			b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
			d(i)=d(i)-((a(i)/b(i-1))*d(i-1))
		end do
			!u(n)=-1.0d0
			u(n-1)=d(n-2)/b(n-2) !back substitution
		do i=n-2,2,-1
			u(i)=(d(i-1)-(c(i-1)*u(i+1)))/b(i-1) !Numerical values calculated using thomas algoritham
		end do
		do i=1,n
			uexact(i)=cos(x(i))
		end do 
		error=0.0d0
		do i=1,n
			error=error+((u(i)-uexact(i))**2)
		end do
		error=dsqrt(h*error)
		print*,"Error=", Error
	end subroutine fw_el


end program Poiseuille
