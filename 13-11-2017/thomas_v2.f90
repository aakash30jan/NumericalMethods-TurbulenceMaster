program thomasAlgo
 implicit none
 real(8):: a(4,4),b(4),factor,x(4),sumN
 integer :: n,m,i,j,k
	
  a=transpose(reshape((/4,-1,0,0,-1,4,-1,0,0,-1,4,-1,0,0,-1,4/),(/4,4/)))
  !b=transpose(reshape((/2,4,6,13/),(/1,4/)))
  b=(/2,4,6,13/)

	n=4
  m=4

do i=2,n
    factor=a(i,i-1)/a(i-1,i-1)
    do j=1,n
        a(i-1,j)=a(i-1,j)*factor
    end do
    b(i-1)=b(i-1)*factor
    do k=1,n
        a(i,k)=a(i,k)-a(i-1,k)
    end do
    b(i)=b(i)-b(i-1)
end do
do i=n,1,-1 
    sumN=b(i)
    do j=i+1,n
        sumN=sumN-a(i,j)*x(j)
    end do
    x(i)=sumN/a(i,i)
end do


print * , x

print * , "yes, this code works!"

	end program thomasAlgo
