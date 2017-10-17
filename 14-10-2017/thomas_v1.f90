program thomasAlgo
	
	implicit none
	real (8), allocatable :: matrix(:,:), postMatrix(:,:), sol(:,:)
	integer :: m_size,p,q,r,i,j

	
	m_size=3

	allocate(matrix(m_size,m_size), postMatrix(m_size,m_size),sol(m_size,m_size))
	!get elements of matrix from user and check if it is a square matrix or 
	!generate a matrix of random numbers
	!CALL RANDOM_NUMBER(matrix)
	matrix=transpose(reshape((/1,2,0,4,2,6,4,8,4/),(/3,3/)))
	postMatrix=matrix
	!do j=1,m_size-1,-1 !number of columns=m_size !in decreasing stepsize
	do j=1,m_size-1,1	
		do i=2,m_size,1 !number of rows=m_size !in increasing stepsize
		!postMatrix(i,:)=matrix(i,:)-((matrix(i,1)/matrix(1,1))*matrix(1,:))
		postMatrix(i,:)=postMatrix(i,:)-((postMatrix(i,j)/postMatrix(i-1,j))*postMatrix(j,:))
		end do
	end do
	
	print*,matrix
	
	print*, postMatrix
	
	deallocate(matrix,postMatrix,sol)
end program thomasAlgo
