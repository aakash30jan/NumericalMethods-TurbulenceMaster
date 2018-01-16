program heat
	use schemes
	implicit none
	integer,parameter :: n=9   	! EITHER n or h can be defined and commented - n is the number of points in the x domain
	integer,parameter :: m=10	!m is the number of time steps
	real(8),parameter :: pi=4.0d0*datan(1.0d0)  
	real(8),parameter :: lb=0.0d0		!left boundary of x 
	real(8),parameter :: rb=pi		!right boundary of x
	real(8),parameter :: t_initial=0.0d0	!initial time 
	real(8),parameter :: t_final=1.0d0	!final time
	real(8),parameter :: t=t_final-t_initial	!total actual time 
	real(8),parameter :: alpha=1.0d0	! the value of alpha
	integer :: i,j,k,time,l
	real(8) :: u(n),u_new(n),error,dt,dx,dy,h,x(n),u_exact(n),err_norm2,beta
	
	dt=(t_final-t_initial)/dfloat(m)!time step
	dx=(rb-lb)/dfloat(n-1)	!step size in x from n defined above
	!dx=0.02 !defined manually and overwritten if necessary
	h=dx	!step size
	x(1)=lb;x(n)=rb	!boundary  for x 							
	do i=2,n-1
		x(i)=(i-1)*dx	!grid in x direction
	end do 
	!print*,x
	beta=(alpha*dt)/(dx**2)
	
	!creating blank files
	!open(unit=21,file="h_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	!open(unit=22,file="k_Vs_error.dat",status='old',action='write',form='formatted',position="append")
	!close(21)
	!close(22)
	
	
	!call Euler_Explicit(n,m,h,dx,dt,t,x,alpha,beta)
	!call Euler_Implicit(n,m,h,dx,dt,t,x,alpha,beta)
        call Crank_Nicolsan(n,m,h,dx,dt,t,x,alpha,beta)
	


end program heat
