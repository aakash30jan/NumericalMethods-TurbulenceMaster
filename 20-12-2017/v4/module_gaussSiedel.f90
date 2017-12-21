module module_gaussSiedel
	implicit none
	
	contains
		! subroutine to solve the linear system of equations by Gauss Siedel (without relaxation)
	subroutine gaussSiedel(x,y,u,h,f,error,n,k_gs,u_exact)
	        implicit none
		integer :: i,j,k_gs
		integer,intent(IN) :: n
		real(8) :: u_new(n,n),error,u(n,n),x(n,n),y(n,n),err_max,err_norm1(n),err_norm2
		real(8),intent(IN):: h,f(n,n),u_exact(n,n)
		
		u=0.0d0 ! always initialise all the solution vecotors to zero
		u_new=0.0d0	! always initialise all the solution vecotors to zero
		call random_number(u)	!starting from a random guess is always a good choice
		DO i=1,n
    		u(i,1)=0.0d0
			u(i,n)=0.0d0   	!specifying all the boundary conditions 
			u(1,i)=0.0d0
    		u(n,i)=0.0d0
    	        end do
		error=1.0d0		!Specify error=1 so that for the first iteration the IF loop should get executed
		k_gs=0
		do 
		
	  		IF(error>1E-14)THEN
			error=0.0d0		!always initialize error=0.0d0 for each iteration 
			k_gs=k_gs+1			!k is the iteration counter
			!$OMP PARALLEL DO 
			do i=2,n-1		!excluding all boundary points becz of BCs
			do j=2,n-1		!excluding all boundary points becoz of BCs

				u_new(i,j)=(1.0d0/4.0d0)*(u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1)-((h**2)*(f(i,j)))) 
				!new value of u from old value
				error=error+abs((u_new(i,j)-u(i,j))**2) 				
				!calculating error at each point and summing it over whole domain
				u(i,j)=u_new(i,j)
			
			end do
			end do
			!$OMP END PARALLEL DO
			
			error=sqrt(h*h*error)
			!print*,"Error=",error
			!u=u_new			!Replacing the old and new solution vectors
	  		ELSEIF(error<1E-14)THEN
  	 		 EXIT
  	  		ENDIF
		end do
		print*,"Total Number of iterations=",k_gs
		err_norm1=0.0d0
		err_norm2=0.0d0
		do j=1,n
			do i=1,n
				err_norm1(j)=err_norm1(j)+abs(u_exact(i,j)-u(i,j))
				err_norm2=err_norm2+((u_exact(i,j)-u(i,j))**2)
			end do
		end do
		err_norm2=sqrt(h*h*err_norm2)
		
		write(79,"(3ES16.8)"),h,maxval(err_norm1),err_norm2
		close(79)
	        open(26,file="data_for_GS.dat")
	        do i=1,n
		do j=1,n
				write(26,"(3ES16.8)"),x(i,j),y(i,j),u(i,j) 	!writing all data in to file 
		end do 
	        end do
	        close(26)
	end subroutine 
 

end module module_gaussSiedel
