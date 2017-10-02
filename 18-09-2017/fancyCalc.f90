program fancyCalc


!compile with -fdefault-real-8 compiler option
!example gfortran -fdefault-real-8 fancyCalc.f90 -o fancyCalc.exe


!-std=f2008  use this compiler option to write codes in standard format 
implicit none
character(len=20) :: op, con	
real(8), allocatable :: num(:)
integer :: i,cont

allocate(num(2))

print *, "enter any",size(num),"numbers: "
read *, num(1),num(2)


do i=1,5
	cont=i
	if (cont == i) then
	print *, "enter an operation"
	read *, op
	select case (op)
   	case ('+') 
    	 	print*,"ans ", num(1)+num(2) 
   	case ('-')
     		print*, "ans ", num(1)-num(2)
   	case ('*') 
     		 print*, "ans", num(1)*num(2) 
   	case ('/') 
      		print*, "ans", num(1)/num(2) 
   	case default
      		print*, "Invalid operation"
	end select
	end if
end do

deallocate(num)

end program fancyCalc
