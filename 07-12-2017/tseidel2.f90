!*********************************************************************
!*               Test program for Gauss Seidel method                *
!* ----------------------------------------------------------------- *
!* SAMPLE RUN:                                                       *
!* (Solve a linear system by Gauss Seidel iterative method).         *
!*                                                                   *
!* Input file tseidel2.dat contains:                                 *
!*                                                                   *
!* 16                                                                *
!*  4 -1  0  0  -1  0  0  0   0  0  0  0   0  0  0  0                *
!* -1  4 -1  0   0 -1  0  0   0  0  0  0   0  0  0  0                *
!*  0 -1  4 -1   0  0 -1  0   0  0  0  0   0  0  0  0                *
!*  0  0 -1  4   0  0  0 -1   0  0  0  0   0  0  0  0                *
!* -1  0  0  0   4 -1  0  0  -1  0  0  0   0  0  0  0                *
!*  0 -1  0  0  -1  4 -1  0   0 -1  0  0   0  0  0  0                *
!*  0  0 -1  0   0 -1  4 -1   0  0 -1  0   0  0  0  0                *
!*  0  0  0 -1   0  0 -1  4   0  0  0 -1   0  0  0  0                *
!*  0  0  0  0  -1  0  0  0   4 -1  0  0  -1  0  0  0                *
!*  0  0  0  0   0 -1  0  0  -1  4 -1  0   0 -1  0  0                *
!*  0  0  0  0   0  0 -1  0   0 -1  4 -1   0  0 -1  0                *
!*  0  0  0  0   0  0  0 -1   0  0 -1  4   0  0  0 -1                *
!*  0  0  0  0   0  0  0  0  -1  0  0  0   4 -1  0  0                *
!*  0  0  0  0   0  0  0  0   0 -1  0  0  -1  4 -1  0                *
!*  0  0  0  0   0  0  0  0   0  0 -1  0   0 -1  4 -1                *
!*  0  0  0  0   0  0  0  0   0  0  0 -1   0  0 -1  4                *
!*  2                                                                *
!*  1                                                                *
!*  1                                                                *
!*  2                                                                *
!*  1                                                                *
!*  0                                                                *
!*  0                                                                *
!*  1                                                                *
!*  1                                                                *
!*  0                                                                *
!*  0                                                                *
!*  1                                                                *
!*  2                                                                *
!*  1                                                                *
!*  1                                                                *
!*  2                                                                *
!*                                                                   *
!* See full results in output file tseidel2.lst.                     *
!*                                                                   *
!*  Number of iterations: 37                                         *
!*                                                                   *
!*  Solution Vector:                                                 *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*   1.000000                                                        *
!*                                                                   *
!* ----------------------------------------------------------------- *
!* Ref.: "Numerical algorithms with C, By Gisela Engeln-Muellges and *
!*        Frank Uhlig, Springer-Verlag, 1996" [BIBLI 11].            *
!*                                                                   *
!*                                 F90 Release By J-P Moreau, Paris. *
!*                                         (www.jpmoreau.fr)         *
!* ----------------------------------------------------------------- *
!* To link with fseidel.f90.                                         *
!*********************************************************************
! Note: this release provides dynamic allocations.
PROGRAM TSEIDEL

Use modseidel

  parameter(ONE=1.d0,ZERO=0.d0)

  REAL*8, POINTER :: a(:,:), b(:), x(:), residu(:)

  integer n, i, j, rc, krit, iter
  REAL*8 omega
   
  !open input file         
  open(unit=1,file='tseidel2.dat',status='old')         
  
  read(1,*) n

  if (n < 1) then
    print *,' Error: dimension must be > 0'
    stop
  end if

! dynamic allocations
  allocate(a(1:n,1:n),stat=ialloc)
  allocate(b(1:n),stat=ialloc)
  allocate(x(1:n),stat=ialloc)
  allocate(residu(1:n),stat=ialloc)

  read(1,*) ((a(i,j),j=1,n),i=1,n)
  read(1,*) (b(i),i=1,n)

  close(unit=1)
                           
  !open output file                         
  open(unit=2,file='tseidel2.lst',status='unknown')  

  write(2,*) ' '  
  write(2,*) '-------------------------------------------------------------------------------'
  write(2,*) '   Gauss Seidel iterative method'
  write(2,*) '-------------------------------------------------------------------------------'    
  write(2,*) ' Dimension of the input matrix =', n
  write(2,*) ' ' 
  write(2,*) ' Input matrix:'
  write(2,10) ((a(i,j),j=1,n),i=1,n)
  write(2,*) ' '
  write(2,*) ' Second member:'
  write(2,15) (b(i),i=1,n)
  
  krit=0         !no special criterion
  omega=1.5d0    !Relaxation coefficient (must be > 0 and < 2)

  write(2,*) ' '
  write(2,*) ' Solution vector:'

  x=ZERO

  !call Gauss Seidel routine
  call seidel (krit, n, a, b, omega, x, residu, iter, rc)

  if (rc.ne.ZERO) then
    print *,' Error in SEIDEL: return code:', rc
	stop
  end if   
  write(2,20) (x(i),i=1,n)

  write(2,*) ' '
  write(2,*) ' Residual vector (must be near zero):'
  write(2,20) (residu(i),i=1,n)
  write(2,*) ' '
  write(2,*) ' Number of iterations:', iter
  write(2,*) ' '
  write(2,*) '-------------------------------------------------------------------------------'
  
  close(unit=2)  
  print *,' '  
  print *,' Results in tseidel2.lst...'
  print *,' '  
  stop
10 format(16F6.2)
15 format(1F6.2)
20 format(8F10.6)
end program



! J-P Moreau April, 2005
