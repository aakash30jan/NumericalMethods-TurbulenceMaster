program test_dtime
              integer(8) :: i, j
              real, dimension(2) :: tarray
              real :: result
              !call dtime(tarray, result)
            
              !print *, tarray(1), tarray(2)
              do i=1,900000000    ! Just a delay
                  j = i * i - i
              end do
              call dtime(tarray, result)
              print *, result
              !print *, tarray(1)
              !print *, tarray(2)
          end program test_dtime
