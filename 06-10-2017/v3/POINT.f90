module point
        type point_t
          real :: x
          real :: y
          real :: z
        end type point_t

        ! step 2: extends binary "+"
        interface operator (+)
          procedure add_values
          !PROCEDURE add_point_to_point     ! newly added
        end interface operator (+)

      contains

        ! step 1: provides interpretation of defined operator
        function add_values(op1, op2)
          type(point_t), intent(in) :: op1
          real, intent(in)          :: op2
          type(point_t)             :: add_values

          add_values%x = op1%x + op2
          add_values%y = op1%y + op2
          add_values%z = op1%z + op2

          print *, "adding real to point"

        end function add_values
        
        
        
       !FUNCTION add_point_to_point(op1, op2)
          !TYPE(point_t), INTENT(IN) :: op1, op2
          !TYPE(point_t)             :: add_point_to_point
          ! Perform the addition
        !END FUNCTION add_point_to_point

      end module point
