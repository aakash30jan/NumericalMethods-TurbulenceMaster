program poisson

  real(8) :: a
  ! a is lower diagonal of the tridiagonal matrix
	! b is main diagonal of the tridiagonal matrix
	! c is upper diagonal of the tridiagonal matrix
	! d is right hand side of the equations of the tridiagonal matrix
	! u is numerical value calculated from the thomas algorithm
	! m is array of number of discritizations

  a=1.0d0
	a(1)=0.0d0	
	b=-2.0d0			
	c=1.0d0
	c(n)=0.0d0	


  !d=f(xi)  ! [f(x1), .,.,.,.,.,., f(xn)]x (h**2)  !delta x=h
  !f(x)=x**3
   
  !discritizing x
  x0=0.0d0
  xn=1.0d0
  h=(xn-x0)/n  ! delta x
  do i=1,n+1 !n for n-1 points  !which makes size of x as n+1
    if (i=1) then
      x(1)=x0
    else
      x(i)=x(i-1)+h
    end if
  end do
 
  do i=2,n-1 !again, size of d as n+1
   d(i)=( -1*x(i) )*(h**2)
  end do
  
  d(1)=0 !beta of drichlet!from problem 1
  !d(n)=0 !sigma of neuman !from problem 1
  d(n)=0 !sigma of drichlet !from problem 1
  

  
 
	aBig=0.0d0
  do i=2,n
		aBig(i-1,i-1)=b(1)
		aBig(i-1,i)=c(1)
		aBig(i,i-1)=a(2)
	end do 
  
  aBig(1,1)=1   !check for this
  
 

end program
