!  matrix multiplication of 3*3 matrix as taught by Ilkay


program matrix_mul
implicit none

real (8) :: s
real (8) :: m1(3,3), m2(3,3), ans(3,3)
integer :: p,q,r

!get elements of matrix from user
!for now declare elements 
m1=transpose(reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/)))
m2=transpose(reshape((/1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5/),(/3,3/)))



!(row,column)

ans(1,1)=sum(m1(:,1)*m2(1,:))
ans(1,2)=sum(m1(:,1)*m2(2,:))
ans(1,3)=sum(m1(:,1)*m2(3,:))

ans(2,1)=sum(m1(:,2)*m2(1,:))
ans(2,2)=sum(m1(:,2)*m2(2,:))
ans(2,3)=sum(m1(:,2)*m2(3,:))

ans(3,1)=sum(m1(:,3)*m2(1,:))
ans(3,2)=sum(m1(:,3)*m2(2,:))
ans(3,3)=sum(m1(:,3)*m2(3,:))



print*,"THIS IS NOT A PROPER PROGRAM"
print*,"THIS IS NOT A PROPER PROGRAM"
print*,"THIS IS NOT A PROPER PROGRAM"
print*,"THIS IS NOT A PROPER PROGRAM"

print *,"By using loops:",ans

print*,"By using matmul", matmul(m1,m2)



end program matrix_mul
