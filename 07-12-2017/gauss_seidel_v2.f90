program seidelTest
Use modseidel
!implicit none
parameter(ONE=1.d0,ZERO=0.d0)
integer, parameter :: n=3
real(8):: a(n,n),b(n),x(n),residu(n)
integer :: i, j, rc, krit, iter
real(8) :: omega

! Problem Matrix A
a=reshape( (/4.0,-1.0,-1.0,-2.0,6.0,1.0,-1.0,1.0,7.0/), (/ 3,3/))
! Solution Matrix B
b=(/3.0,9.0,-6.0/)

krit=0.0
omega=1.00
x=1.00

 call seidel (krit, n, a, b, omega, x, residu, iter, rc)

print *,x
print *, iter
print *, residu


end program



!*====================================================================*
!*                                                                    *
!*   Input parameters:                                                *
!*   ================                                                 *
!*      crit     integer crit                                         *
!*               select criterion                                     *
!*               =1 : row sum criterion                               *
!*               =2 : column sum criterion                            *
!*               =3 : criterion of Schmidt-v.Mises                    *
!*               other : no check                                     *
!*      n        integer n ( n > 0 )                                  *
!*               size of mat, b and x                                 *
!*      mat      REAL*8   mat(n,n)                                    *
!*               Matrix of the linear system                           *
!*      b        REAL*8 b(n)                                          *
!*               Right hand side                                      *
!*      omega    REAL*8 omega; (0 < omega < 2)                        *
!*               Relaxation coefficient.                              *
!*      x        REAL*8  x(n)                                         *
!*               Starting vector for iteration                        *
!*                                                                    *
!*   Output parameters:                                               *
!*   ==================                                               *
!*      x        REAL*8  x(n)                                         *
!*               solution vector                                      *
!*      residu   REAL*8   residu(n)                                   *
!*               residual vector  b - mat * x; close to zero vector   *
!*      iter     integer iter                                         *
!*               Number of iterations performed                       *
!*      rc       integer return code                                  *
!*               =  0     solution has been found                     *
!*               =  1     n < 1  or omega <= 0 or omega >= 2          *
!*               =  2     improper mat or b or x (not used here)      *
!*               =  3     one diagonal element of mat vanishes        *
!*               =  4     Iteration number exceeded                   *
!*               = 11     column sum criterion violated               *
!*               = 12     row sum criterion violated                  *
!*               = 13     Schmidt-v.Mises criterion violated          *
!*                                                                    *
!*====================================================================*
