program foo

    character(len=100) :: filename,ext
    character(len=100) :: format_string
    integer :: i

    do i=1, 20
        if (i < 20) then
            format_string = "(A5,I2)"
        else
            format_string = "(A5,I3)"
        endif

        !write (filename,format_string,ext) "test_",i,".dat"
	write (filename,format_string) "test_",i
        !print *, trim(filename)
        !print *, filename
	open(99, file=fileName)
	write(99,*) 5.666346
	close(99)
    enddo

end program

