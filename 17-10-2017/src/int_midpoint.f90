program main

	implicit none
	real (8), allocatable :: x(:),y(:),a(:)
  real (8) :: h,u,l,er,M,K
	integer ::  i,N
	
	N=20 !Number of rectangular strips
	l=0.0 !lower limit 
	u=3.14 !upper limit
  h=(u-l)/N  !strip size

	allocate(x(N),y(N),a(N))
  do i=1,N
    x(i)=((2*l)+(i*h))/2 
  end do

  y=sin(x)
  K=1
  M=1
  !y=1/x
  !y=exp(-x**2)
  !y=2+sin(2*x**2)
  !y=exp(10-x)
  !print *,y

  do i=1,N
    a(i)=y(i)*h  
  end do
  
  
  print *,"The Approximate Value of Integration is: ", sum(a)
  er=abs((K*((u-l)**3))/(24*N*N))
  print *,"The Absolute Error of Integration is: ", er
  
 
  deallocate(x,y,a)
end program main
