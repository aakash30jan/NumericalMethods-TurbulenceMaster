program main

	implicit none
	real (8), allocatable :: x(:),y(:),a(:)
  real (8) :: h,u,l, ana, er_m(20), asum_m(20),er_t(20), asum_t(20),er_s(20), asum_s(20) 
	integer ::  j,i,N,NS(20)
	
	NS(1)=1
	do i=2, 20
	  NS(i)=2**(i-1)
	end do
	
	do j=1,20
	N=NS(j) !Number of rectangular strips
	l=0.0 !lower limit 
	u=3.14 !upper limit
  h=(u-l)/N !strip size
  
  !!Midpoint
  allocate(x(N),y(N),a(N))
  do i=1,N
    x(i)=((2*l)+(i*h))/2 
  end do
  !!functions!!
  !y=1/x
  !y=exp(-x**2)
  !ana=0.746824
  !y=2+sin(2*x**2)
  !ana=9.98452
  y=exp(10-x)
  ana=22025
  do i=1,N
    a(i)=y(i)*h  
  end do
  !print *,"The Approximate Value of Integration is: ", sum(a)
  asum_m(j)=sum(a)
  er_m(j)=abs(sum(a)-ana)
  !print *,"The Absolute Error of Integration is: ", er
  deallocate(x,y,a)
  
  !!Trapezoidal
  allocate(x(N+1),y(N+1),a(N))
  do i=1,N+1
    if (i==1) then
      x(i)=l
    else 
      x(i)=x(i-1)+h
    end if
  end do  
  !!functions!!
  !y=1/x
  !y=exp(-x**2)
  !ana=0.746824
  !y=2+sin(2*x**2)
  !ana=9.98452
  y=exp(10-x)
  ana=22025
  do i=1,N
    a(i)=0.5*h*(y(i)+y(i+1))  
  end do
  !print *,"The Approximate Value of Integration is: ", sum(a)
  asum_t(j)=sum(a)
  er_t(j)=abs(sum(a)-ana)
  !print *,"The Absolute Error of Integration is: ", er
  deallocate(x,y,a)


  
  !!Simpson1/3
  allocate(x(N+1),y(N+1),a(N))
  do i=1,N+1
    if (i==1) then
      x(i)=l
    else 
      x(i)=x(i-1)+h
    end if
  end do  
  !!functions!!
  !y=1/x
  !y=exp(-x**2)
  !ana=0.746824
  !y=2+sin(2*x**2)
  !ana=9.98452
  y=exp(10-x)
  ana=22025
  do i=2,N,2 !for two strips together
    a(i)=(h/3)*(y(i-1)+4*y(i)+y(i+1))  
  end do
  !print *,"The Approximate Value of Integration is: ", sum(a)
  asum_s(j)=sum(a)
  er_s(j)=abs(sum(a)-ana)
  !print *,"The Absolute Error of Integration is: ", er  
  deallocate(x,y,a)
  end do
  
 
  !!write to file if needed
  open(99, file='prob_4data.dat')!, status='old', access='append')
  write(99,*) asum_m,er_m!,asum_t,er_t,asum_s,er_s
  !print *, NS
  close(99)
  print *, "done" 
  
  
  
end program main
