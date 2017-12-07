module modseidel
! ---------------------- MODULE fseidel.f90 ---------------------------
!Gauss Seidel Method with relaxation
contains

subroutine seidel(crit,n,mat,b,omega,x,residu,iter,rc)
parameter(ITERMAX=500)            ! Maximal number of iterations
parameter(ONE=1.d0,TWO=2.d0,ZERO=0.d0)
  integer crit, n, iter, rc
  REAL*8 mat(n,n),b(n),omega
  REAL*8 x(n),residu(n)
!*====================================================================*
!*                                                                    *
!*  seidel solves the linear system  mat * x = b  iteratively.        *
!*  Here  mat  is a nonsingular  n x n  matrix, b is the right hand   *
!*  side for the linear system and x is the solution.                 *
!*                                                                    *
!*  seidel uses the Gauss Seidel Method with relaxation for a given   *
!*  relaxation coefficient 0 < omega < 2.                             *
!*  If  omega = 1, the standard Gauss Seidel method (without          *
!*  relaxation) is performed.                                         *
!*                                                                    *
!*====================================================================*
!*                                                                    *
!*   Applications:                                                    *
!*   =============                                                    *
!*      Solve linear systems with nonsingular system matrices that    *
!*      satisfy one of the following criteria: row sum criterion,     *
!*      column sum criterion or the criterion of Schmidt and v. Mises.*
!*      Only if at least one of these criteria is satisfied for mat,  *
!*      convergence of the scheme is guaranteed [See BIBLI 11].       *
!*                                                                    *
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
  REAL*8 tmp, eps;

  iter = 0                        !Initialize iteration counter
  rc = 0

  if (n<1.or.omega<=ZERO.or.omega>=TWO) then
    rc=1
    return
  end if

  eps = 1.d-10

  do i=1, n                       !transform mat so that all
                                          !diagonals equal 1
    if (mat(i,i) == ZERO) then
      rc=3
      return
    end if
    tmp = ONE / mat(i,i)
    do j=1, n
      mat(i,j)= mat(i,j)*tmp
    end do
    b(i) = b(i)*tmp               !adjust right hand side b
  
  end do


  !check convergence criteria
  if (crit==1) then
     do i = 1, n                  !row sum criterion
       tmp=ZERO
       do j=1,n
         tmp = tmp + dabs(mat(i,j))
       end do
       if (tmp >= TWO) then
         rc=11
         return
       end if 
     end do
  else if (crit==2) then  
     do j=1, n                    !column sum criterion
	   tmp=ZERO
       do i=1,n
         tmp = tmp + dabs(mat(i,j))
       end do
       if (tmp >= TWO) then
         rc=12
	 return
       end if
     end do
  else if (crit==3) then
     tmp=ZERO
     do i=1, n
       do j=1, n                  !criterion of Schmidt
         tmp = tmp + mat(i,j)**2  !von Mises
       end do
     end do
     tmp = DSQRT(tmp - ONE)
     if (tmp >= ONE) then
       rc=13
       return
     end if
  end if

  do i=1, n 
    residu(i) = x(i)              !store x in residu
  end do

  do while (iter <= ITERMAX)      !Begin iteration
  
    iter=iter+1

    do i=1, n
      tmp=b(i)
      do j=1, n
        tmp =  tmp - mat(i,j) * residu(j)
      end do 
      residu(i) = residu(i) + omega * tmp
    end do

    do i=1, n                     !check break-off criterion
      tmp = x(i) - residu(i)
      if (DABS (tmp) <= eps) then
        x(i) = residu(i)          !If rc = 0 at end of loop
        rc = 0                    !  -> stop iteration
      else
        do j=1, n 
          x(j) = residu(j)
        end do
        rc = 4
        goto 10
      end if
    end do
    if (rc == 0) goto 20          !solution found
10 end do                         !End iteration

20 do i=1, n                      !find residual vector
     tmp=b(i)
     do j=1, n
       tmp = tmp - mat(i,j) * x(j)
     end do
     residu(i) = tmp
   end do

  return

end subroutine

! ----------------------- END fseidel.f90 ----------------------------
end module
