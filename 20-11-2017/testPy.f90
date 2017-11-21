program SystemTest
  print *, "FORTRAN: BLOCKING"
  call system("python pythonsleep.py")
  print *, "FORTRAN: NONBLOCKING"
  call system("python pythonsleep.py &")
  print *, "FORTRAN: EXIT"
end program SystemTest
