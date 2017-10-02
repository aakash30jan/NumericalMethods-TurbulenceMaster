!  matrix multiplication of 3*3 matrix
!search for testDrivenDevelopment, cmake's ctest  


program matrix_mul
!implicit none

real (8) :: s
real (8), allocatable :: m1(:,:), m2(:,:), ans(:,:)
integer :: m_size,p,q,r


!enter size of square matrix
m_size=3

allocate(m1(m_size,m_size), m2(m_size,m_size),ans(m_size,m_size))


!get elements of matrix from user
!for now declare elements 
m1=transpose(reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/)))
m2=transpose(reshape((/1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5/),(/3,3/)))


!remove 3 later to use shape(m1)
do p=1,3
 	do q=1,3
		s=0 		
		do r=1,3
		s=s+m1(p,r)*m2(r,q)
		end do
		ans(p,q)=s	
	end do
end do

print *,"By using loops:",ans

print*,"By using matmul", matmul(m1,m2)

WRITE (*, FMT='(ES25.15)') ' Det = ', ans

deallocate(ans,m1,m2)


end program matrix_mul
