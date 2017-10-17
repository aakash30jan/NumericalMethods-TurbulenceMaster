program main

	implicit none
	real (8), allocatable :: x(:),y(:),a(:)
  real (8) :: h,u,l,er
	integer ::  i,N
	
	N=100000
	l=0.0
	u=10.0
  h=(u-l)/N

  
	allocate(x(N+1),y(N+1),a(N+1))

  do i=1,N+1
    if (i==1) then
      x(i)=l
    else
      x(i)=x(i-1)+h
    endif
  !print*, x(i)
  end do
  
  !y=1/x
  !y=exp(-x**2)
  !y=2+sin(2*x**2)
  y=exp(10-x)
  
  !print *,x  
  !print *,y
  
  do i=1,N
    !a(i)=h*(y(i+1)-y(i)) !!CHECK THIS LATER!
    a(i)=h*y(i+1) !!not working for infinity!! rectangular form!
    !print * ,a(i)
  end do
    
  print *,"value of integration is " , sum(a)
  !print '(F3.5)', sum(a)
  er= (maxval(abs(a))*(h**2))/(N*2.0)
  print *,"value of error is " , er
  
  deallocate(x,y,a)



end program main
