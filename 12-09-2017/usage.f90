program usage

type myType
   !integer:: i
   real:: i
   !real*8 :: a(3)
end type myType



type(myType) :: data1, data2

data1%i=2.6464
data2%i=5
print *, data1%i,data2%i


end program usage




  
