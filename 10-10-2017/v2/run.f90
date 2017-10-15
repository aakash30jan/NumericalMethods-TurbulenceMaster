program genFiles_v1

    character(len=100) :: fileBase,fileExt,fString,fileName
    integer :: i
    real(8) :: rN
    fileBase="testCase_"
    fileExt=".dat"
    do i=1, 20 
        if (i < 10) then !change this as required!
            fString = "(A9,I1)"
        else
            fString = "(A9,I2)"
        endif
	write (fileBase,fString) fileBase,i
        fileName=trim(fileBase) // fileExt  
        !print *, fileName   !if required
	open(1, file=fileName)
	CALL RANDOM_NUMBER(rN)
	write(1,*) rN
	close(1)
    enddo

end program genFiles_v1

