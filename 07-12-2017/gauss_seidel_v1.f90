program seidel
implicit none
integer, parameter :: n=3
real(8):: a(n,n),b(n),x(n)
integer :: m,itr,i,j,k
real(8) :: s

! Problem Matrix A
a=reshape( (/1.0,1.0,-1.0,1.0,-2.0,3.0,2.0,3.0,1.0/), (/ 3,3/))
! Solution Matrix B
b=(/4.0,-6.0,7.0/)




m= 10!number of iterations

do itr=1,m
    do i=1,n
        s=0.0
		    do j=1,n
            if (i.ne.j) then
                s=s+a(i,j)*x(j)
            end if
        end do !j
        x(i)=(b(i)-s)/a(i,i)
     end do !i 
     print *, "iter = ",itr
     do k=1,n
        print *,"x=", x(k)
     end do !k
end do !itr

end program
