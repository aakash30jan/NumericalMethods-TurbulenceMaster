program learnSubs
 use sub1, only: mySum2, mySum1
 implicit none
 real(8) :: x1,x2,x3
 
 x1=2.0
 x2=1.5

 call mySum1(x1,x2,x3)
 print *, x3
 call mySum2(x1,x2,x3)
 print *, x3

end program learnSubs


