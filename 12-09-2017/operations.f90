program calc_integer
  implicit none   !forces the programmer to declare all variables
  real :: w,l
  real :: prmtr, area, diag1, diag2 
  real :: usq

  w = 3                    !Define a
  l = 4                    !Define b
  prmtr = 2*(w + l)        !Calculate the perimeter of rectangle
  area  = w*l              !Calculate the area of rectangle
   
  usq=w**2+l**2
  !> Should give error on compilation
  diag1  = sqrt(usq) !Calculate diagonal
  diag2 =(w**2+l**2)**0.5  !Calculate diagonal

  print*, 'Perimeter = ',prmtr
  print*, 'Area = ' ,area
  print*, 'diag1 = ',diag1
  print*, 'diag2 = ',diag2

end program calc_integer
