!****************************************************
!*  Program to demonstrate Lagrange interpolation   *
!*     of Function SIN(X) in double precision       *
!* ------------------------------------------------ *
!* Reference: BASIC Scientific Subroutines, Vol. II *
!* By F.R. Ruckdeschel, BYTE/McGRAWW-HILL, 1981 [1].*
!*                                                  *
!*             F90 Version by J.-P. Moreau, Paris.  *
!*                      (www.jpmoreau.fr)           *
!* ------------------------------------------------ *
!* SAMPLE RUN:                                      *
!*                                                  *
!*  Lagrange interpolation of SIN(X):               *
!*                                                  *
!*    Input X: 0.5                                  *
!*    Order of interpolation: 4                     *
!*                                                  *
!*    SIN(X) = 0.47942552 (exact value: 0.47942554) *
!*                                                  *   
!****************************************************
PROGRAM Lagrange

! Label: 100

real*8  X(0:4), Y(0:4)
real*8  xx,yy
integer iv,n

  print *,' '
  print *,'Lagrange interpolation of f(X):'
  iv=5
  ! Input sine table
  !-----------------------------------------------------------------
  ! Sine table values from  Handbook of mathematical functions
  ! by M. Abramowitz and I.A. Stegun, NBS, june 1964
  !-----------------------------------------------------------------
  !X(1)=0.000d0; Y(1)=0.00000000d0; X(2)=0.125d0; Y(2)=0.12467473d0
  !X(3)=0.217d0; Y(3)=0.21530095d0; X(4)=0.299d0; Y(4)=0.29456472d0
  !X(5)=0.376d0; Y(5)=0.36720285d0; X(6)=0.450d0; Y(6)=0.43496553d0
  !X(7)=0.520d0; Y(7)=0.49688014d0; X(8)=0.589d0; Y(8)=0.55552980d0
  !X(9)=0.656d0; Y(9)=0.60995199d0;X(10)=0.721d0; Y(10)=0.66013615d0
  !X(11)=0.7853981634d0; Y(11)=0.7071067812d0; X(12)=0.849d0; 
  !Y(12)=0.75062005d0; X(13)=0.911d0; Y(13)=0.79011709d0
  !X(14)=0.972d0; Y(14)=0.82601466d0
  !-----------------------------------------------------------------
  
  !
  X=(/-1.0 , -0.5,  0.0 ,  0.5,1.0/)
  Y=(/0.03846154,  0.13793103,  1.0,  0.13793103,0.03846154/)

  !

  ! Input interpolation point
100 continue
  print *,' '
  write(*,"('   Input X: ')",advance='no')
  read *,xx
  write(*,"('   Order of interpolation: ')",advance='no')
  read *, n 

  call Interpol_Lagrange(iv,n,xx,yy,X,Y)

  if (n.ne.0) then
    write(*,50) yy, dsin(xx)
    goto 100
    print *,' '
  end if
  print *,' '
50 format(/'   f(X) = ',f10.8,'  (exact value: ',f10.8,')')
  stop

end


!********************************************************
!*          Lagrange interpolation subroutine           *
!* ---------------------------------------------------- *
!* n is the level of the interpolation ( Ex. n=2 is     *
!* quadratic ). v is the total number of table values.  *
!* X(i), Y(i) are the coordinate table values, Y(i)     *
!* being the dependant variable. The X(i) may be arbi-  *
!* trarily spaced.  x is the interpolation point which  *
!* is assumed to be in the interval  with at least one  *
!* table value to the left, and n to the right. If this *
!* is violated, n will be set to zero. It is assumed    *
!* that the table values are in ascending X(i) order.   *
!********************************************************
Subroutine Interpol_Lagrange(iv,n,xx,yy,X,Y)  
  !Labels: 100,200,300
  integer i,iv,j,k,n
  real*8  X(0:4), Y(0:4)
  real*8  xx,yy
  real*8  XL(0:9)
  ! Check to see if interpolation point is correct
  if (xx < X(1)) goto 100 
  if (xx <= X(iv-n)) goto 200
  ! An error has been encountered
100 n=0 ; return
  ! Find the relevant table interval
200 i=0
300 i = i + 1
  if (xx > X(i)) goto 300
  i = i - 1
  ! Begin interpolation
  do j = 0, n
    XL(j)=1.d0
  end do
  yy=0.d0
  do k = 0, n
    do j = 0, n
      if (j.eq.k) goto 400
      XL(k)=XL(k)*(xx-X(j+i))/(X(i+k)-X(j+i))
400 end do
    yy=yy+XL(k)*Y(i+k)
  end do
  return
end


! End of file lagrange.f90

