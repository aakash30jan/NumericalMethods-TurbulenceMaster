program poisson
!use module_triSolve
	implicit none
  integer,parameter :: n=50  
	real(8),parameter :: lb=0.0d0
	real(8),parameter :: rb=1.0d0
	real(8),parameter :: lbu=0.0d0
	real(8),parameter :: rbu=0.0d0
	integer :: i,j,k
	real(8)	::x(n),h,u(n),a(n-2),b(n-2),c(n-2),d(n-2),uexact(n),error(n)


  !rb=right limit
  !lb=left limit

  !rbu=right boundary condn in u
  !lbu=left boundary condn in u

  ! a is lower diagonal of the tridiagonal matrix
	! b is main diagonal of the tridiagonal matrix
	! c is upper diagonal of the tridiagonal matrix
	! d is right hand side of the equations of the tridiagonal matrix

	! u is numerical value calculated from the thomas algorithm


	x(1)=lb;  x(n)=rb  
	h=(x(n)-x(1))/n
    print*,"h=",h
		do i=2,n-1
		x(i)=h*(i-1)	
	end do
  !range of x
	!print*,x

	u(1)=lbu
  u(n)=rbu

	d(1)=((h**2)*(-1*(x(2))))-u(1)
	do i=2,n-3
		d(i)=(h**2)*(-1*(x(i+1)))
	end do
	d(n-2)=((h**2)*(-1*(x(n-1))))-u(n)
  !print *,d

	b=-2.0d0
	a(1)=0.0d0
	a=1.0d0
	c=1.0d0
	c(n-2)=0.0d0

    
  
  

	call  triSolve(a,b,c,d,u,n)
  !print *,a
  !print *,b
  !print *,c
  !print *,d
  !print *,x
	!print*,u
	
	do i=1,n
			uexact(i)=-1*(x(i))
		end do 
		error=0.0d0
		do i=1,n
			error=error+((u(i)-uexact(i))**2)
		end do
		error=dsqrt(h*error)
		print*,"n=",n,"h=",h,"        Error=", error(n)
    print*,"u","               uexact"
    do i=1,n
        print *, u(i),uexact(i)
    end do

contains
	subroutine triSolve(a,b,c,d,u,n) 
		implicit none
		integer::i
		integer,intent(IN) ::n
		real(8) ::a(n-2),b(n-2),c(n-2),d(n-2),u(n)
		do i=2,n-2
			b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
			d(i)=d(i)-((a(i)/b(i-1))*d(i-1))
  	end do
			!u(n)=0.0d0
			u(n-1)=d(n-2)/b(n-2)
		do i=n-2,2,-1
			u(i)=(d(i-1)-(c(i-1)*u(i+1)))/b(i-1) 
		end do
		
	end subroutine triSolve


end program poisson
