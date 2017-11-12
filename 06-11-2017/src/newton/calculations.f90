module calculations
	implicit none 
	contains
	  ! This subroutine will calculate the x and y values for n number of points 
		subroutine calc_xy(x,y,delta_x,n) 
		implicit none
		integer, intent(IN)::n
		integer :: i
		real, intent(out)::x(n),y(n) !arrays of points for setting up the polynomial
		real, intent(IN) ::delta_x
		
		do i=2,n-1
			x(i)=x(i-1)+delta_x !calculatin x coordintes for setting up the polynomial
		end do
		do i=1,n
			!y(i)=1.0d0/(1.0d0+25.0d0*x(i)*x(i)) !calculating y coordinates for setting up the polynomical from exact solution
		   y(i)=cos(x(i)*3.141592654)
		end do

		do i=1,n
			write(25,*) x(i),y(i)
		end do	
		end subroutine calc_xy

		!to calculate x and y values for m number of points from the exact solution 
		subroutine calc_xf_yf(x_f,y_f,delta_x_f,m)
		implicit none
		integer, intent(IN)::m
		integer :: i
		real, intent(out)::x_f(m),y_f(m) !arrays of points for plotting the polynomial at m points
		real, intent(IN) ::delta_x_f
		
		do i=2,m-1
			x_f(i)=x_f(i-1)+delta_x_f !calculatin x coordintes for setting up the polynomial
		end do
		!calculating f(x) from the exact solution for the corrosponding values of x(n)
		do i=1,m
			!y_f(i)=1.0d0/(1.0d0+25.0d0*x_f(i)*x_f(i)) !calculating y coordinates for setting up the polynomical from exact solution
	    y_f(i)=cos(x_f(i)*3.141592654)	
		end do

		do i=1,m
			write(16,*) x_f(i),y_f(i)
		end do

		end subroutine calc_xf_yf

		!to calculate the coeficients for newton interpolation
		subroutine calc_alpha(x,y,alpha,n)
		implicit none
		integer,intent(IN):: n
		integer ::i ,j 
		real,intent(INOUT) ::x(n),y(n),alpha(n)

		alpha(1)=y(1)
		Do i= 1,n-1
			Do j=1, n-i
			
				y(j)=(y(j+1)-y(j))/(x(j+i)-x(j))

			end do 
			alpha(i+1)=y(1)
		end do
	  end subroutine calc_alpha
	  
	  
		!to calculate y=f(x) from nth value polynomial for x_f(m) number of points 		
		subroutine calc_poly(x,x_f,alpha,p,n,m)
		implicit none
		integer,intent(IN):: n,m
		integer ::i,j
		real,intent(IN) ::x(n),x_f(m),alpha(n)
		real ::p
		p=alpha(n)
	
		do j=1,m
		p=alpha(n)
		
			do i=n,2,-1 		
			
				p=(p*(x_f(j)-(x(i-1))))+alpha(i-1)
		
			end do
			write(43,*) x_f(j),p
		end do 
		end subroutine calc_poly

end module calculations
