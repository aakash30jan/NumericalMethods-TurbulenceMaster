program array
implicit none


!p1
!real(8) :: a(3), b(7), C(3)
!character(len= 10) :: k(4)
!a(1)=1213
!a(2)=3.2
!a(3)=4324
!b=(/1,2,3,4,5,6,7/)
!print *, a
!print *, b
!print *, b(3)
!print *, "enter 3 elements of array C"
!read *, C
!print *, "the second element of array C is :  ", C(2)
!print *, "enter 4 elements of array k"
!read *, k
!print *, "your middle name is:   ", k(2)


!p2
!integer, dimension(2:5) :: ar1
!real, dimension(4) :: ar2
!real(8), dimension(-5:-3) :: ar3
!read *, ar1, ar2, ar3
!print *, ar1, ar2, ar3
!read *, ar3
!print *, ar3(-4)
!print *,"size", size(ar1),size(ar2),size(ar3)
!print *, "lb", lbound(ar1),lbound(ar2),lbound(ar3)
!print *, "ub", ubound(ar1),ubound(ar2),ubound(ar3)

!!error in this part!!
!integer :: dim
!real, dimension(dim) :: arDim
!print *, "enter dimension :  "
!read *, dim
!print *, "enter the array of size ",dim " :  "
!read *, arDim
!print *,"the array is : " ,arDim


!p3

real(8), allocatable :: a(:), c (:), b(:,:)
integer :: k

!print *, "enter size of array a : "
!read *, k
!allocate (c(k))
!print *, "enter ",k, " elements of array c : "
!read *, c
!print *,"array c is : ",c

!allocate arrays
allocate (a(3))
allocate (b(3,3))
!use arrays

a=(/1,2,3/)
b(1,:)=(/4,5,6/)
b(2,:)=(/7,8,9/)
!!b(3,:)=a+b(1,:)
b(3,:)=(/10,11,12/)

print *, b(:,3)

!deallocate arrays
deallocate(a)
deallocate(b)
deallocate(c)


end program array
