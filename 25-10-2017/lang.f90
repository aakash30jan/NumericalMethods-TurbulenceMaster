program lang 
  implicit none
  real(8), allocatable:: x(:),y(:),L(:)!,xg(:),yg(:)
  !real(8):: x(5),y(5),L(5)
  real(8):: xg,yg ,xu,xl,delX , temp
  integer :: i,j,k, N, M

  N=4  !order of polynomial
  M=100 !number of points in x to discritize to interpolate
  
  allocate(x(N+1),y(N+1),L(N+1))    !xg(N),yg(N),)
  
  !x=(/-1.0 , -0.5,  0.0 ,  0.5,1.0/)
  !y=(/0.03846154,  0.13793103,  1.0,  0.13793103,0.03846154/)  
  xg= 0.2  !at this 
  
  xl=-1.0
  xu=1.0
  delX=(xu-xl)/N
  
  do i=1,N+1
    if (i==1) then
      x(i)=xl
    else 
      x(i)=x(i-1)+delX
    end if
    y(i)=1/(1+25*(x(i)**2))
  end do  
   
  !print *,x
  !print *,y
  yg=0
  do i=1,N+1,1
    L(i)=1
    do j=1,N+1,1
      if (j/=1) then
        L(i)=L(i)*(xg-x(j))/(x(i)-x(j));
        print *,L(i),x(i)-x(j),i,j
      end if
    end do
  !print *, L
  yg=yg+L(i)*y(i)
  !print *,yg
  end do
  
  print *, "Value of y at x=",xg, "is",yg

  temp=20.0
  temp=sin(temp)
  print *, "sin(x)", temp

  deallocate(x,y,L)

end program lang

