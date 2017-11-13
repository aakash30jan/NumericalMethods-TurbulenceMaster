program lagrange_main

implicit none
real(8),allocatable :: x(:),y(:),L(:),xvg(:),yvg(:),xpl(:),ypl(:),DP(:),er(:)
real(8)::xu,xl,delX, xg, yg,era
integer :: i,j,k,n,m,p

n=5 !(n-1) is the order of polynomial (this means that there should atleast be n+1 values to create a polynomial)
!m=100 !number of values to interpolate after creating the polynomial

allocate(x(n+1),y(n+1),L(n+1),xvg(m),yvg(m),xpl(m),ypl(m),DP(5),er(5))

do i=1,5
  DP(i)=10**i
end do

do p=1,5
m=DP(p)  !number of values to interpolate after creating the polynomial

  xl=-1.0
  xu=+1.0
  delX=(xu-xl)/(n+1)
   
 do i=1,n+1
    if (i==1) then
      x(i)=xl
    else 
      x(i)=x(i-1)+delX
    end if
    !y(i)=(x(i)**2)-(4*x(i))+4
    !print *,x(i)
    !print *,y(i)
  end do 

!enter the equation here
!y=(x**2)-(4*x)+4
!y=1/(1+(25*(x**2)))
y=cos(x*3.14159265359)
!print *, x
!print *, y

delX=(xu-xl)/(m+1)
  do i=1,m
    if (i==1) then
      xvg(i)=xl
    else 
      xvg(i)=xvg(i-1)+delX
    end if
    !print *,xvg(i)
  end do 
!print *, xvg
!print *,yvg
xpl=xvg

!enter the equation here again
!ypl=(xpl**2)-(4*xpl)+4
!ypl=1/(1+(25*(xpl**2)))
ypl=cos(xpl*3.14159265359)

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
!print *, "Value of y at", xg, "is",yg
yvg(k)=yg  
yg=0.0  !!for error not to propagate
end do
era=abs((sum(yvg)/size(yvg))-(sum(ypl)/size(ypl)))

er(p)=era
era=0.0

print*, DP(p),er(p)
end do  !!for the p loop




!open(99, file='eqn1.dat')
!do i=1,m
!  write(99,*) xpl(i),ypl(i),xvg(i),yvg(i)
  !print *, xpl(i),ypl(i),xvg(i),yvg(i)
!end do
close(99)

deallocate(x,y,L,xvg,yvg,xpl,ypl,DP,er)


end program
