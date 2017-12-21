program poisson_2D
	use module_gaussSiedelRelaxed
	use module_gaussSiedel
	use module_jacobi
	implicit none
	integer,parameter :: n=129 !h=1/(n-1) (simply do 2^3 +1, 2^4 +1,..., 2^7 +1)
	real(8),parameter ::pi=4.0d0*datan(1.0d0)
	integer :: i,j,k,k_j,k_gs,k_gsr
	real(8) :: u(n,n),x(n,n),y(n,n),h,u_new(n,n),f(n,n),u_exact(n,n)
	real(8)	:: error,error_1
	
	!"     n		Itr_jacobi		 Itr_Gauss		Itr_Gauss_with Relaxation" 
	open(10,file="h_Vs_Iteration.dat")
        close(10)
	open(unit=11,file="h_Vs_Error_Jaco.dat")
	close(11)
	open(unit=12,file="h_Vs_Error_GS.dat")
	close(12)
	open(unit=13,file="h_Vs_Error_GSR.dat")
	close(13)
	
	open(unit=77,file="h_Vs_Iteration.dat",status='old',action='write',form='formatted',position="append")
	open(unit=78,file="h_Vs_Error_Jaco.dat",status='old',action='write',form='formatted',position="append")
	open(unit=79,file="h_Vs_Error_GS.dat",status='old',action='write',form='formatted',position="append")
	open(unit=80,file="h_Vs_Error_GSR.dat",status='old',action='write',form='formatted',position="append")
	
	
	h=1.0d0/dfloat(n-1) !step size
	print*,"h=",h
	
	 do i=1,n
    	 do j=1,n
    		x(i,j)=(j-1)*h 		!define grid for x coordinates
    		!print*,x(i,j)
    	 end do
         end do
        do j=1,n
    	do i=1,n
    		y(i,j)=(i-1)*h		!define grid for y coordinates
			!print*,y(i,j)
    	end do
        end do
	
	do i=1,n
		do j=1,n
			f(i,j)=-(sin(pi*x(i,j))*sin(pi*y(i,j))*(pi**2))*(2.0d0) 		
			!exact solution of the  given differential equation
			u_exact(i,j)=(sin(pi*x(i,j)))*(sin(pi*y(i,j)))
			!print*,u_exact(i,j)
			!f(i,j)=(-(pi**2)*sin(pi*x(i,j))*sin(pi*y(i,j)))
			!write(22,*),x(i,j),y(i,j),u(i,j),f(i,j)
		end do
	end do
	
	
	call gaussSiedel(x,y,u,h,f,error,n,k_gs,u_exact)
	
	call gaussSiedelRelaxed(x,y,u,h,f,error,n,pi,k_gsr,u_exact)
	
	call jacobi(x,y,u,u_new,u_exact,h,f,error,n,k_j)

	write(77,"(4I6)") n-1,k_j,k_gs,k_gsr
	
	close(77)
		
end program
