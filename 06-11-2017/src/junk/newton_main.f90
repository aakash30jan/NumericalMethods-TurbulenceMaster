
program newton_main
implicit none
real(8),allocatable :: x(:),y(:),d(:,:),xvg(:),yvg(:),ypl(:),xpl(:)
real(8) :: xg,yg,pn,delX,xu,xl
integer :: i,j,k,n,m

n=10 !polynomial order-1
m=10 ! number of values to interpolate


allocate(x(n+1),y(n+1),d(n+1,n+1),xvg(m),yvg(m),xpl(m),ypl(m))

!x=(/1,2,3,4,5/) 
!y=(/0,1,4,6,10/) 


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
!xg=1.5
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
yvg(k)=yg
!print*,'At xg=', xg,'yg=',yg
end do

do i= 1,m
print *, xpl(i),ypl(i),xvg(i),yvg(i)
end do
end program
