program genFile
  implicit none
  character(len=10) :: fileBase='testCase_'
  integer :: n=10
  character(len=12) :: full, nChar


write(nChar,'(i2.0)') n
full=fileBase//nChar
print*,full

end program

	
