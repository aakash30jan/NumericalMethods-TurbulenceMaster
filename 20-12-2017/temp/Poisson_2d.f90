program poisson_2d
	use take_data
	use calc_subroutine
	implicit none
	integer :: i,j,k,k_j,k_gs,k_gsr
	real(8) :: u(n,n),x(n,n),y(n,n),h,u_new(n,n),f(n,n),u_exact(n,n)
	real(8)	:: error,error_1
	open(unit=77,file="h_Vs_Iteration.dat",status='old',action='write',form='formatted',position="append")
	open(unit=78,file="h_Vs_Error_Jaco.dat",status='old',action='write',form='formatted',position="append")
	open(unit=79,file="h_Vs_Error_GS.dat",status='old',action='write',form='formatted',position="append")
	open(unit=80,file="h_Vs_Error_GSR.dat",status='old',action='write',form='formatted',position="append")
	h=1.0d0/dfloat(n-1) !step size
	print*,"h=",h
	
	
	 DO i=1,n
    	DO j=1,n
    		x(i,j)=(j-1)*h 		!define grid for X coordinates
    		!print*,x(i,j)
    	END DO
    END DO
    DO j=1,n
    	DO i=1,n
    		y(i,j)=(i-1)*h		!define grid for Y coordinates
			!print*,y(i,j)
    	END DO
    END DO
	
	do i=1,n
		do j=1,n
			f(i,j)=-(sin(pi*x(i,j))*sin(pi*y(i,j))*(pi**2))*(2.0d0) 		!exact solution of the  given differential equation
			u_exact(i,j)=(sin(pi*x(i,j)))*(sin(pi*y(i,j)))
			!print*,u_exact(i,j)
			!f(i,j)=(-(pi**2)*sin(pi*x(i,j))*sin(pi*y(i,j)))
			!write(22,*),x(i,j),y(i,j),u(i,j),f(i,j)
		end do
	end do
	call jacobi(x,y,u,u_new,u_exact,h,f,error,n,k_j)
	
	call Gauss_Siedel(x,y,u,h,f,error,n,k_gs,u_exact)
	
	call Gauss_Siedel_Relax(x,y,u,h,f,error,n,pi,k_gsr,u_exact)

	write(77,"(4I6)") n-1,k_j,k_gs,k_gsr
	
	close(77)
		
end program
