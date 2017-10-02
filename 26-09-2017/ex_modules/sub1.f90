module sub1
 implicit none
 private
 public mySum2, mySum1

 contains 
 subroutine mySum1(a,b,c)
  implicit none
  real(8) :: a,b,c
  c=a+b
 end subroutine mySum1

 subroutine mySum2(a,b,c)
  implicit none
  real(8), intent(in) :: a,b
  real(8), intent(out) :: c
  c=a-b
 end subroutine mySum2
end module sub1
