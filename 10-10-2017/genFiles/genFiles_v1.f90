program genFiles_v1
!generates files in the execution directory
!stores a random number in each file
    character(len=100) :: fileBase,fileExt,fCount,fileName
    integer :: i
    real(8) :: rN

    fileBase="testCase_"
    fileExt=".dat"

    do i=1, 20 
        if (i < 10) then !change this as required valid for numbers below 99!
            fCount = "(A9,I1)"
        else
            fCount = "(A9,I2)"
        endif
	write (fileBase,fCount) fileBase,i
        fileName=trim(fileBase) // fileExt  
        print *, "Successfully Generated ", fileName   !if required
	open(1, file=fileName)
	CALL RANDOM_NUMBER(rN)
	write(1,*) rN
	close(1)
    enddo

end program genFiles_v1


!compile using "gfortran genFiles_v1.f90 -o genFiles_v1.exe"
!execute using "./genFiles_v1.exe"
