program ex4

real(8),allocatable :: x(:),y(:),L(:)
real(8):: xg(),yg()
real(8):: xl,xu,delX
integer :: i,j,n

!x=(/0.013,1.998,4.085/)
!y=(/3.946,0.0,4.351/)
n=3
allocate(x(n+1),y(n+1),L(n))
 
  xl=0.013
  xu=4.085
  delX=(xu-xl)/n
   
 do i=1,n+1
    if (i==1) then
      x(i)=xl
    else 
      x(i)=x(i-1)+delX
    end if
    y(i)=(x(i)**2)-(4*x(i))+4
    !print *,x(i)
    !print *,y(i)
  end do 

!y=x**2+(4*x)+4

!print *, x 
!print *, y
!n=size(x);
xg=1.5
yg=0
do i=1,n
    L(i)=1
        do j=1,n
        if (j.ne.i) then
            L(i)=L(i)*(xg-x(j))/(x(i)-x(j))
        end if
        end do
!print *,i,L(i)
yg=yg+L(i)*y(i)
end do
print *, "Value of y at", xg, "is",yg    

end program
