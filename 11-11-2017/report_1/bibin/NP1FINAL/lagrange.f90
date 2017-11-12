module lagrange
  implicit none

contains

function lgrnge(X,Y,N,XX)
  real(8),dimension(0:N)::X,Y,Multiply !X: datapoints at which interpolation needs to be perform, Y: function values at the datapoints
  integer::i,j,N !i,j are do loop integers and N is the order of the input.
  real(8)::lgrnge,total,XX !interpolated value of the xx input to the function
 
 total=0

  do i=0,N
 Multiply(i)=Y(i)
  
  do j=0,N
     if (j==i) cycle 
     Multiply(i)=Multiply(i)*(XX-X(j))/(X(i)-X(j))
  end do
  total=total+Multiply(i)
  end do
  lgrnge=total
end function lgrnge

 end module lagrange
