program main
 
 use overloadSum
 implicit none

 type(field):: a,b,c
 a%u=1.0
 a%v=2.0
 a%w=3.0
 a%field_name='a'
 b%u=1.0
 b%v=2.0
 b%w=3.0
 b%field_name='b'
 
 !print*, a%u + b%u !read about operator overloading!
 c=a + b

 !call fieldsum(a,b)
 print *, c


end program main



! Output
!2.0000000000000000        4.0000000000000000        6.0000000000000000 
