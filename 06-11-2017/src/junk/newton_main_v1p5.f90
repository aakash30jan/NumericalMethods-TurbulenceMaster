program newton_main

implicit none
real(8),allocatable :: x(:),y(:),d(:,:),xvg(:),yvg(:),xpl(:),ypl(:),DP(:),er(:)
real(8)::xu,xl,delX, xg, yg,era,pn
integer :: i,j,k,n,m,p

n=4 ! is the order of polynomial (this means that there should atleast be n+1 values to create a polynomial)
!m=100!number of values to interpolate after creating the polynomial

allocate(x(n+1),y(n+1),d(n+1,n+1),xvg(m),yvg(m),xpl(m),ypl(m),DP(5),er(5))

do i=1,5
 DP(i)=10**i
end do

do p=1,5
m=DP(p)  !number of values to interpolate after creating the polynomial

  xl=-1.0
  xu=1.0
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
y=1/(1+(25*(x**2)))
!y=cos(x*3.14159265359)
!y=(5.144600*x*x)  + (4.137894*x) -33.164012
!print *,"x values", x
!print *,"y values", y

delX=(xu-xl)/(m+1)
  do i=1,m
    if (i==1) then
      xvg(i)=xl
    else 
      xvg(i)=xvg(i-1)+delX
    end if
    !print *,xvg(i)
  end do 
!print *,"xvg values", xvg
!print *,"yvg values", yvg
xpl=xvg

!enter the equation here again
!ypl=(xpl**2)-(4*xpl)+4
ypl=1/(1+(25*(xpl**2)))
!ypl=cos(xpl*3.14159265359)
!ypl=(5.144600*xpl*xpl)  + (4.137894*xpl) -33.164012

!x=(/0.0,0.5,1.0,2.0/)
!y=(/1.0,1.8987,3.7183,11.3891/) !at xvg=1.5 yvg=6.7760251760
!xvg=1.5

!yvg=0.0
do k=1,size(xvg)
  xg=xvg(k)
  yg=y(1)
  do i=1,n-1
    do j=1,n-i
      y(j)=(y(j+1)-y(j))/(x(j+i)-x(j))
      d(i,j)=y(j)
    enddo
  enddo

  do i=1,n-1
    pn=1
    do j=1,i
      pn=pn*(xg-x(j))
    enddo
    yg=yg+d(i,1)*pn
  enddo

  !print*,'At xg=', xg,'yg=',yg
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
!  print *, xpl(i),ypl(i),xvg(i),yvg(i)
!end do
!close(99)

deallocate(x,y,d,xvg,yvg,xpl,ypl,DP,er)


end program
