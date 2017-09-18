program loops
implicit none

!compile with -g -Wall -fcheck=all
!example gfortran  -g -Wall -fcheck=all loops.f90 -o loops.exe

real :: a(5)
real, allocatable :: b(:)
integer :: i
a=(/1,2,3,4,5/)

allocate(b(4))
b=(/9,8,7,6/)

my1stloop: do i=1, 4
	print *,a(i)
end do my1stloop


my2ndloop: do i=1, 9
	print *,b(i)
end do my2ndloop

deallocate(b)
end program loops
