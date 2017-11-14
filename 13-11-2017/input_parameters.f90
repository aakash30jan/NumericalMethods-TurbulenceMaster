module input_parameters
	implicit none
	integer,parameter :: n=4 
  
	! d is the right hand side of the equations of the tridiagonal matrix
  !for theproblem mentioned in exercise
  real(8), parameter :: d(1:n)=(/2.0,4.0,6.0,13.0/) 
  
  !real(8), parameter :: a(1:n) = -1.0
	!real(8), parameter :: b(1:n)=4.0	
  !real(8), parameter :: c(1:n) = -1.0
  	
  
end module input_parameters
