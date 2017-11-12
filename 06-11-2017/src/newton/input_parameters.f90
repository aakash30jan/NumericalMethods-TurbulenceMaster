module input_parameters
	implicit none
	integer,parameter :: n=10 !value of DEGREE of POLYNOMIAL -1 for eg. n=5 will generate 4th order polynomial 
	integer,parameter :: m=100 !number of discritisation points  
	real(4),parameter :: lb=-1.0d0 !lower limit for x
	real(4),parameter :: rb=1.0d0 !upper limit for x
	
end module input_parameters
