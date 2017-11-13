program ex4

real(8),allocatable :: x(:),y(:),L(:),xvg(:),yvg(:)
!real(8):: xvg(2),yvg(2)
real(8):: xg,yg,xl,xu,delX
integer :: i,j,n,k

!x=(/0.013,1.998,4.085/)
!y=(/3.946,0.0,4.351/)
n=4
allocate(x(n+1),y(n+1),L(n),xvg(n-1),yvg(n-1))
 
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

!y=(x*x)+(4*x)+4
!print *, x 
!print *, y
!n=size(x);
!xvg=(/1.5, 3.5/)
!yvg=(/0.0,0.0/)

do i=2,n
 xvg(i-1)=x(i)
end do
!print *, xvg
!xvg=x(2,n)
yvg=0.0
do k=1,size(xvg)
xg=xvg(k)
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
end do

!print *, "Value of y at", xg, "is",yg    

end program
