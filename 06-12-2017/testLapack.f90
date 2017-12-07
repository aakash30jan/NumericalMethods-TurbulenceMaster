!sudo apt-get install liblapack-dev
!whereis liblapack
!liblapack: /usr/lib/liblapack.a /usr/lib/liblapack.so

!gfortran testLapack.f90 -lblas -llapack -o testLapack.exe  !this works
!gfortran testLapack.f90 -L/usr/lib -llapack -L/usr/lib -lblas -o testLapack.exe

program LinearEquations
  ! solving the matrix equation A*x=b using LAPACK
  implicit none

  ! declarations
  double precision :: A(3,3), b(3)
  integer :: i, pivot(3), ok
  
  ! matrix A
  A(1,:)=(/3, 1, 3/)
  A(2,:)=(/1, 5, 9/)
  A(3,:)=(/2, 6, 5/)
  
  ! vector b
  b(:)=(/-1, 3, -3/)
  !b(:)=(/2, 2, 9/)

  ! find the solution using the LAPACK routine DGESV
  call DGESV(3, 1, A, 3, pivot, b, 3, ok)

  ! print the solution x
  do i=1, 3
    write(*,9) i, b(i)
  end do  

9 format('x[', i1, ']= ', f5.2)  
end program LinearEquations


