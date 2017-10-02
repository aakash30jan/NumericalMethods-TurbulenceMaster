program learnSubs
 implicit none
 real(8) :: x1,x2,x3
 
 x1=2.0
 x2=1.5

 call mySum1(x1,x2,x3)
 print *, x3
 call mySum2(x1,x2,x3)
 print *, x3

end program learnSubs

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
