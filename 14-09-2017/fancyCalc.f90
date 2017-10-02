program fancyCalc

implicit none
character(len=20) :: op	

print *, "enter an operation"
read *, op
select case (op)
   case ('+') 
     print*,"ans ", a+b 
   case ('-')
     print*, "ans ", a-b
   case ('*') 
      print*, "ans", a*b 
   case ('/') 
      print*, "ans", a/b 
   case default
      print*, "Invalid operation"
end select



end program fancyCalc
