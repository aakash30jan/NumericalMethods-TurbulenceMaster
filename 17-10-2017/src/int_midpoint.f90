program main

	implicit none
	real (8), allocatable :: x(:),y(:),a(:),xm(:)
  real (8) :: h,u,l,er,M,K
	integer ::  i,N
	
	N=100 !Number of rectangular strips
	l=0.0 !lower limit 
	u=10.0 !upper limit
  h=(u-l)/N  !strip size

	allocate(x(N+1),y(N+1),a(N),xm(N))
  !do i=1,N
    !x(i)=((2*l)+(i*h))/2 
  !end do
  do i=1,N+1
    if (i==1) then
      x(i)=l
    else 
      x(i)=x(i-1)+h
    end if
  end do  
  !print *, x

  do i=1,N
    xm(i)=(x(i)+x(i+1))/2
  end do   
  !y=sin(x)
  !K=1
  !M=1
  !y=1/x
  !y=exp(-x**2)
  !y=2+sin(2*x**2)
  y=exp(10-xm)
  !print *,y

  do i=1,N
    a(i)=y(i)*h  
  end do
  
  !print *, a
  print *,"The Approximate Value of Integration is: ", sum(a)
  !er=abs((K*((u-l)**3))/(24*N*N))
  er=abs(sum(a)-22025)
  print *,"The Absolute Error of Integration is: ", er
  
 
  deallocate(x,y,a,xm)
end program main
