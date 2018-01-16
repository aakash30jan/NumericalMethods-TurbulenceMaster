module schemes
	implicit none
	
	contains 
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	subroutine Euler_Explicit(n,m,h,dx,dt,t,x,alpha,beta)
	implicit none
	integer ::i,j,k,time
	integer,intent(IN)::n,m
	real(8) :: u_new(n),err_norm2,u(n),u_exact(n)
	real(8),intent(IN) :: dt,dx,h,x(n),beta,alpha,t
	open(22,file="dataCond.dat")
	!open(unit=77,file="h_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	open(unit=78,file="k_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	u=0.0d0
	u_exact=0.0d0 					
	u(1)=0.0d0
	u(n)=0.0d0					
	do i=2,n-1
		u(i)=sin(x(i))						
		u_exact(i)=(exp(-t))*(sin(x(i)))	
		
	end do 
	do time=1,m
		do i=2,n-1
			
			u_new(i)=(beta*(u(i-1)+u(i+1)))+((1.0d0-(2.0d0*beta))*u(i))
				
		end do
		u_new(1)=0.0d0
		u_new(n)=0.0d0			
		u=u_new
	end do
	err_norm2=0.0d0
	do i=1,n
		!err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 			
		err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 			
			
	end do
	!err_norm2=sqrt(dx*err_norm2) 		
	err_norm2=sqrt(err_norm2) 		
	
	print*,"err_norm2=",err_norm2
	DO i=1,n
  		write(22,"(2ES16.8)")x(i),u(i)
 	END DO
        !write(77,"(2ES16.8)"),dx,err_norm2
	write(78,"(2ES16.8)"),dt,err_norm2
	close(22)
	!close(77)
	close(78)
	
	end subroutine Euler_Explicit
	
	
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	subroutine Euler_Implicit(n,m,h,dx,dt,t,x,alpha,beta)
	implicit none
	integer ::i,j,k,time
	integer,intent(IN)::n,m
	real(8) :: u_new(n),err_norm2,u(n),u_exact(n)
	real(8),intent(IN) :: dt,dx,h,x(n),beta,alpha,t
	real(8)	:: a(n-2),b(n-2),c(n-2),d(n-2) 
	open(22,file="dataCond.dat")
	!open(unit=77,file="h_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	open(unit=78,file="k_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	u=0.0d0
	u_exact=0.0d0 					
	u(1)=0.0d0
	u(n)=0.0d0					
	do i=2,n-1
		u(i)=sin(x(i))						
		u_exact(i)=(exp(-t))*(sin(x(i)))	
		
	end do 
	a=0.0d0;b=0.0d0;c=00d0;d=0.0d0
	!b=1.0d0+(2.0d0*beta)	 
	a=-beta;a(1)=0.0d0		
	c=-beta;c(n-2)=0.0d0		
	do time=1,m
		b=1.0d0+(2.0d0*beta)	
		d(1)=u(2)+(beta*u(1))
		!print*,d(1)
		do i=2,n-3
			d(i)=u(i+1)
		end do
		d(n-2)=u(n-1)+(beta*u(n))
		do i=2,n-2
			b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
			d(i)=d(i)-((a(i)/b(i-1))*d(i-1))
		end do
		u(n-1)=d(n-2)/b(n-2) 
		do i=n-2,2,-1
			u(i)=(d(i-1)-(c(i-1)*u(i+1)))/b(i-1) 
		end do
		!u(1)=0.0d0;	u(n)=0.0d0	
	end do 
	err_norm2=0.0d0
	do i=1,n
		!err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 		
		err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 			
	end do
	!err_norm2=sqrt(dx*err_norm2) 		
	err_norm2=sqrt(err_norm2) 		
	
	print*,"err_norm2=",err_norm2
	DO i=1,n
  		write(22,"(2ES16.8)")x(i),u(i)
 	END DO
        !write(77,"(2ES16.8)"),dx,err_norm2
	write(78,"(2ES16.8)"),dt,err_norm2
	close(22)
	!close(77)
	close(78)

	end subroutine Euler_Implicit
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	
	subroutine Crank_Nicolsan(n,m,h,dx,dt,t,x,alpha,beta)
	implicit none
	integer ::i,j,k,time
	integer,intent(IN)::n,m
	real(8) :: u_new(n),err_norm2,u(n),u_exact(n)
	real(8),intent(IN) :: dt,dx,h,x(n),beta,alpha,t
	real(8)	:: a(n-2),b(n-2),c(n-2),d(n-2) 
	open(22,file="dataCond.dat")
	!open(unit=77,file="h_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	open(unit=78,file="k_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	u=0.0d0
	u_exact=0.0d0 					
	u(1)=0.0d0
	u(n)=0.0d0					
	do i=2,n-1
		u(i)=sin(x(i))						
		u_exact(i)=(exp(-t))*(sin(x(i)))	
		
	end do 
	a=0.0d0;b=0.0d0;c=00d0;d=0.0d0
	!b=1.0d0+(2.0d0*beta)	
	a=-beta/2.0d0;a(1)=0.0d0		
	c=-beta/2.0d0;c(n-2)=0.0d0		
	do time=1,m
		b=1.0d0+(beta)	
		d(1)=((1.0d0-beta)*u(2))+((beta/2.0d0)*(u(3)+(2.0d0*u(1))))
		!print*,d(1)
		do i=2,n-3
			d(i)=((1.0d0-beta)*u(i+1))+((beta/2.0d0)*(u(i+2)+u(i)))
		end do
		d(n-2)=((1.0d0-beta)*(u(n-1)))+((beta/2.0d0)*((2.0d0*u(n))+u(n-2)))
		do i=2,n-2
			b(i)=b(i)-((a(i)/b(i-1))*c(i-1))
			d(i)=d(i)-((a(i)/b(i-1))*d(i-1))
		end do
		u(n-1)=d(n-2)/b(n-2) 
		do i=n-2,2,-1
			u(i)=(d(i-1)-(c(i-1)*u(i+1)))/b(i-1) 
		end do
		!u(1)=0.0d0;	u(n)=0.0d0	
	end do 

	err_norm2=0.0d0
	do i=1,n
		!err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 			
		err_norm2=err_norm2+(abs(u_exact(i)-u(i))**2) 			
	end do
	!err_norm2=sqrt(dx*err_norm2) 		
	err_norm2=sqrt(err_norm2) 		
	
	print*,"err_norm2=",err_norm2
	DO i=1,n
  		write(22,"(2ES16.8)")x(i),u(i)
 	END DO
        !write(77,"(2ES16.8)"),dx,err_norm2
	write(78,"(2ES16.8)"),dt,err_norm2
	close(22)
	!close(77)
	close(78)

	end subroutine Crank_Nicolsan
	
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
end module schemes
