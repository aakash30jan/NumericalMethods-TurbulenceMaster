program
implicit none

  integer ( kind = 4 ) mx

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) xd(mx+1)
  real ( kind = 8 ) xi
  real ( kind = 8 ) yi

  yi = 1.0D+00

  if ( xi /= xd(i) ) then
    do j = 1, mx + 1
      if ( j /= i ) then
        yi = yi * ( xi - xd(j) ) / ( xd(i) - xd(j) )
      end if
    end do
  end if
