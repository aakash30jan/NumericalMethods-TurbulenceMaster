program main
 use myModule, only: func2!, func1 !switch as required
 implicit none
 real(8):: a,b
 a=3.00
 b=4.00	
 print*, func2(a,b)

end program main
