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
	
	!"     n        Itr_jacobi     Itr_gaussSiedel     Itr_gaussSiedelRelaxed" 
	open(10,file="h_Itr.dat")
        close(10)
	open(unit=11,file="h_Err_jacobian.dat")
	close(11)
	open(unit=12,file="h_Err_gaussSiedel.dat")
	close(12)
	open(unit=13,file="h_Err_gaussSiedelRelaxed.dat")
	close(13)

	
	h=1.0d0/dfloat(n-1) 
	print*,"h=",h
	
	 do i=1,n
    	 do j=1,n
    		x(i,j)=(j-1)*h 		!grid for x coordinates
    		!print*,x(i,j)
    	 end do
         end do
        do j=1,n
    	do i=1,n
    		y(i,j)=(i-1)*h		!grid for y coordinates
			!print*,y(i,j)
    	end do
        end do
	
	do i=1,n
		do j=1,n
			f(i,j)=-(sin(pi*x(i,j))*cos(pi*y(i,j))*(pi**2))*(2.0d0) 		
			u_exact(i,j)=(sin(pi*x(i,j)))*(cos(pi*y(i,j))) !exact solution
			!print*,u_exact(i,j)
			
		end do
	end do
	
	open(unit=79,file="h_Err_gaussSiedel.dat",status='old',action='write',form='formatted',position="append")
	call gaussSiedel(x,y,u,h,f,error,n,k_gs,u_exact)
	open(unit=80,file="h_Err_gaussSiedelRelaxed.dat",status='old',action='write',form='formatted',position="append")
	call gaussSiedelRelaxed(x,y,u,h,f,error,n,pi,k_gsr,u_exact)
	open(unit=78,file="h_Err_jacobian.dat",status='old',action='write',form='formatted',position="append")
	call jacobi(x,y,u,u_new,u_exact,h,f,error,n,k_j)

        open(unit=77,file="h_Itr.dat",status='old',action='write',form='formatted',position="append")
	write(77,"(4I6)") n-1,k_j,k_gs,k_gsr
	close(78)
	close(79)
	close(80)
	
	
end program
