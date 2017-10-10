program foo

    character(len=100) :: filename
    character(len=100) :: format_string
    integer :: i

    do i=1, 10
        if (i < 10) then
            format_string = "(A5,I1)"
        else
            format_string = "(A5,I2)"
        endif

        write (filename,format_string) "hello", i
        print *, trim(filename)
    enddo

end program

