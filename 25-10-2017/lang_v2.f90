program lang


real(8) :: x(3),y(3),L(3),xvg(2),yvg(2)
!real(8):: xvg(2),yvg(2)
real(8):: xg,yg,xl,xu,delX
integer :: i,j,n,k

x=(/0.013,1.998,4.085/)
y=(/3.946,0.0,4.351/)
n=3

xvg=(/1.5,4.00/)
!yvg=(/0.0,0.0/)

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

end program
