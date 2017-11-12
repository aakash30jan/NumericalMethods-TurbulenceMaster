program newton
	use input_parameters
	use calculations
	implicit none
	integer :: i, j ,k
	real(4) :: x(n), y(n),alpha(n),delta_x,p,x_f(m),delta_x_f,y_f(m) 
	open(unit=25,file="xy.dat") !contains the x y values calculated from the given function for n pts
	open(unit=43,file="xy_poly.dat") !contains x y values calculated from the interpolated polynomial Pn(x)
	open(unit=16,file="xy_exact.dat") !contains x_f(m) and corrospondin f(x) from exact solution for m points
	x(1)=lb
	x(n)=rb
	x_f(1)=lb 
 	x_f(m)=rb
	delta_x=(x(n)-x(1))/(n-1) !calculating delta_x for n points for finding the required degree of polynomial
	delta_x_f=(x_f(m)-x_f(1))/(m-1) !calculatin delta_x for m points for ploting m points

	call calc_xy(x,y,delta_x,n) !calculating x & y coordinates for n number of points

	call calc_xf_yf(x_f,y_f,delta_x_f,m) !calculating x and y for m number of points 

  call calc_alpha(x,y,alpha,n) ! calculating the coeficients for newton interpolation   
		
	call calc_poly(x,x_f,alpha,p,n,m) !this subroutine will calculate y=f(x) from nth value polynomial for x_f(m) number of points 

	close(25)
	close(43)
	close(16)
	
	print*, "Interpolation Completed"

end program newton
