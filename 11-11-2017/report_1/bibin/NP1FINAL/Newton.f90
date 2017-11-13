module Newton
  implicit none

contains
!N=number of data point=+1 the order of the polynomial used for interpolation


  function order4(X,Y,N,XX)
  real(8):: totala(0:N),yint(0:N),R(0:N,0:N)
  real(8)::X(0:N),Y(0:N)
  real(8)::total,order4,XX,yint2,multiplier
  real(8)::xterm
  integer::N,i,j,k,l,m,order,p



   do l=0,N
    R(0,l)=Y(l)
   end do


 do i=1,N  
    do j=0,N-i  
    R(i,j)=(R(i-1,j+1)-R(i-1,j))/(x(i+j)-x(j))
    end do
 end do

   totala(0)=R(0,0)
 do k=1,N
   totala(k)=R(k,0)
 do i=1,k
  totala(k)=totala(k)*(XX-X(i-1))
  end do
  end do

order4=sum(totala(0:N))
  
end function

end module Newton
