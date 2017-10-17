program arrayPlay

	real(8):: m1(3,3), row(3)
	
	m1=transpose(reshape((/1,2,3,4,5,6,7,8,9/),(/3,3/)))

	print *, m1(1,:)
	!print *, m1(:,1)*2
	row=m1(1,:)
	print *,row(1)
	
	
end program
