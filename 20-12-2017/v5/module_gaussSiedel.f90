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
		
		u=0.0d0 
		u_new=0.0d0	
		call random_number(u)	
		DO i=1,n
    		u(i,1)=0.0d0
			u(i,n)=0.0d0   	
			u(1,i)=0.0d0
    		u(n,i)=0.0d0
    	        end do
		error=1.0d0		
		k_gs=0
		do 
		
	  		IF(error>1E-14)THEN
			error=0.0d0		
			k_gs=k_gs+1			!k is iteration counter
			do i=2,n-1		
			do j=2,n-1		

				u_new(i,j)=(1.0d0/4.0d0)*(u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1)-((h**2)*(f(i,j)))) 
				
				error=error+abs((u_new(i,j)-u(i,j))**2) 				
				
				u(i,j)=u_new(i,j)
			
			end do
			end do
			error=sqrt(h*h*error)
						
	  		ELSEIF(error<1E-14)THEN
  	 		 EXIT
  	  		ENDIF
		end do
		print*,"Total Number of iterations (Gauss Siedel)=",k_gs
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
	        open(26,file="computations_gaussSiedel.dat")
	        do i=1,n
		do j=1,n
				write(26,"(3ES16.8)"),x(i,j),y(i,j),u(i,j) 	
		end do 
	        end do
	        close(26)
	end subroutine 
 

end module module_gaussSiedel
