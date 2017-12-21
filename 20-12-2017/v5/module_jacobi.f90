module module_jacobi
	implicit none
	
	contains
	 !this subroutine solve the linear system of equations by Jacobi
		subroutine jacobi(x,y,u,u_new,u_exact,h,f,error,n,k_j)
		implicit none
		integer :: i,j,k_j
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
   		 END DO
		error=1.0d0		
		k_j=0
		do 
		k_j=k_j+1			
	  	IF(error>1E-14)THEN
		error=0.0d0		
		
		do i=2,n-1		
		do j=2,n-1		

			u_new(i,j)=(1.0d0/4.0d0)*(u(i-1,j)+u(i+1,j)+u(i,j-1)+u(i,j+1)-((h**2)*(f(i,j)))) 
			error=error+abs((u_new(i,j)-u(i,j))**2) 	
		
			
		end do
		end do
		error=sqrt(h*h*error)
		u=u_new			
	  ELSEIF(error<1E-14)THEN
  	  EXIT
  	  ENDIF
	END DO
	err_norm1=0.0d0
	err_norm2=0.0d0
	do j=1,n
		do i=1,n
			err_norm1(j)=err_norm1(j)+abs(u_exact(i,j)-u(i,j))
			err_norm2=err_norm2+((u_exact(i,j)-u(i,j))**2)
		end do
	end do
	err_norm2=sqrt(h*h*err_norm2)
	write(78,"(3ES16.8)"),h,maxval(err_norm1),err_norm2
	close(78)
	print*,"Total Number of iterations (Jacobi)=",k_j
	open(25,file="computations_jacobi.dat")
	do i=1,n
	do j=1,n
				write(25,"(3ES16.8)"),x(i,j),y(i,j),u(i,j) 	
	end do 
	end do
	close(25)
	end subroutine 
	
end module module_jacobi
