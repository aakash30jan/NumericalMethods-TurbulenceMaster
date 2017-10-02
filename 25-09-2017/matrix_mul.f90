!search for testDrivenDevelopment, cmake's ctest  
!compile with -fdefault-real-8 compiler option
!example gfortran -fdefault-real-8 fancyCalc.f90 -o fancyCalc.exe
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!gfortran -fopenmp  matrix_mul.f90 -o matrix_mul_parallel.exe!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-std=f2008  use this compiler option to write codes in standard format 
!example gfortran  -g -Wall -fcheck=all loops.f90 -o loops.exe

program matrix_mul
implicit none

real (8) :: s
real (8), allocatable :: m1(:,:), m2(:,:), ans(:,:), ans2(:,:)
integer :: m_size,p,q,r,i,j

print *, "Enter the size of square matrix: "
read *, m_size

allocate(m1(m_size,m_size), m2(m_size,m_size),ans(m_size,m_size),ans2(m_size,m_size))


!get elements of matrix from user and check if it is a square matrix or not or 
!generate an array of random numbers
CALL RANDOM_NUMBER(m1)
CALL RANDOM_NUMBER(m2)

!or for now declare elements !!aghhhh
!m1=transpose(reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/)))
!m2=transpose(reshape((/1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5/),(/3,3/)))

do p=1,m_size
 	do q=1,m_size
		s=0 		
		do r=1,m_size
		s=s+m1(p,r)*m2(r,q)
		end do
		ans(p,q)=s	
	end do
end do

print *,"By using loops:",ans
ans2=matmul(m1,m2)
print*,"By using matmul", ans2

!WRITE (*, FMT='(ES25.15)') ' Det = ', ans

!do i=1,m_size
!  do j=1,m_size
!	if (ans (i,j)==ans2(i,j)) then
!	!print *, "The matrix multiplication is verified" 
!	print *, "element",ans(i,j),"is equal to",ans2(i,j) 
!	else
!	print *, "The matrix multiplication has some problem(s)" 
!	end if
!  end do
!end do


deallocate(ans,m1,m2,ans2)

end program matrix_mul
