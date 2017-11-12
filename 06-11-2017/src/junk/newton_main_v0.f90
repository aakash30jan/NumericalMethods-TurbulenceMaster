
program newton_main

real(8) :: x(20),y(20),d(20,20)
real(8) :: xg,yg
n=5 !polynomial order-1
xg=1.5

data (x(i), i=1,5)/1,2,3,4,5/ 
data (y(i), i=1,5)/0,1,4,6,10/ 

yg=y(1)
do i=1,n-1
  do j=1,n-i
     y(j)=(y(j+1)-y(j))/(x(j+i)-x(j))
     d(i,j)=y(j)
  enddo
enddo

do i=1,n-1
  p=1
  do j=1,i
    p=p*(xg-x(j))
  enddo
  yg=yg+d(i,1)*p
enddo

print*,'At xg=', xg,'yg=',yg



end program
