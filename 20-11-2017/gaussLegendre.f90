!downloaded from https://rosettacode.org/wiki/Numerical_integration/Gauss-Legendre_Quadrature#Fortran

program gauss
  implicit none
  integer, parameter :: p = 8 ! quadruple precision
  integer            :: n = 10, k  !The n  evaluation points for a n-point rule also called 'nodes'
  real(kind=p), allocatable :: r(:,:)
  real(kind=p)       :: z, a, b, exact
  do n = 1,20 !!number of iterations to obtain nearly exact integration
    a = -3; b = 3
    r = gaussquad(n)
    z = (b-a)/2*dot_product(r(2,:),exp((a+b)/2+r(1,:)*(b-a)/2))
    exact = exp(3.0_p)-exp(-3.0_p)
    print "(i0,1x,g0,1x,g10.2)",n, z, z-exact
  end do
 
  contains 
 
  function gaussquad(n) result(r)
  integer                 :: n
  real(kind=p), parameter :: pi = 4*atan(1._p)
  real(kind=p)            :: r(2, n), x, f, df, dx
  integer                 :: i,  iter
  real(kind = p), allocatable :: p0(:), p1(:), tmp(:)
 
  p0 = [1._p]
  p1 = [1._p, 0._p]
 
  do k = 2, n
     tmp = ((2*k-1)*[p1,0._p]-(k-1)*[0._p, 0._p,p0])/k
     p0 = p1; p1 = tmp
  end do
  do i = 1, n
    x = cos(pi*(i-0.25_p)/(n+0.5_p))
    do iter = 1, 10
      f = p1(1); df = 0._p
      do k = 2, size(p1)
        df = f + x*df
        f  = p1(k) + x * f
      end do
      dx =  f / df
      x = x - dx
      if (abs(dx)<10*epsilon(dx)) exit
    end do
    r(1,i) = x
    r(2,i) = 2/((1-x**2)*df**2)
  end do
  end function
end program


!!Output
!n numerical integral                       error
!--------------------------------------------------
!1 6.00000000000000000000000000000000   -14.    
!2 17.4874646410555689643606840462449   -2.5    
!3 19.8536919968055821921309108927158   -.18    
!4 20.0286883952907008527738054439858   -.71E-02
!5 20.0355777183855621539285357252751   -.17E-03
!6 20.0357469750923438830654575585499   -.29E-05
!.
!.
!.
!.
!.
!20 20.0357498548198037979491872388495   -.82E-28
