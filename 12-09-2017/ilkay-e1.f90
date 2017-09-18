program calc_chrctr
  implicit none   !forces the programmer to declare all variables
  integer :: a,b
  character(len=5) :: myName

  myName = 'H   ello World'     !Define the value of 'name'

  !> It should print only first 5 character, not the whole word
  print*, myName             !Print on the terminal

end program calc_chrctr

