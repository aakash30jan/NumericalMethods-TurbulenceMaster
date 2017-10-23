program main

	implicit none
	real (8), allocatable :: x(:),y(:),a(:)
  real (8) :: h,u,l,er,M,K
	integer ::  i,N
	
	N=2000 !Number of rectangular strips
	l=0.0 !lower limit 
	u=10 !upper limit
  h=(u-l)/N  !strip size

  
	allocate(x(N+1),y(N+1),a(N))

  do i=1,N+1
    if (i==1) then
      x(i)=l
    else 
      x(i)=x(i-1)+h
    end if
  end do  

  !y=sin(x)
  !K=1
  !M=1

  !y=1/x
  !y=exp(-x**2)
  !y=2+sin(2*x**2)
  y=exp(10-x)
  !print *,y
  do i=1,N
    a(i)=0.5*h*(y(i)+y(i+1))  
  end do
  
  print *,"The Approximate Value of Integration is: ", sum(a)
  
  !er=abs((K*((u-l)**3))/(12*N*N))
  er=abs(sum(a)-22025)
  print *,"The Absolute Error of Integration is: ", er
  
  deallocate(x,y,a)



end program main
