module myModule
 implicit none
 public func2
 contains

 !define func1
 !function func1(a,b)
 !implicit none
 !real(8):: a(:,:),b(:,:)
 !real(8):: result
 !call a,b
 !call result
 !result=matmul(a,b)
 !end function func1



 real function func2(a,b)
 implicit none
 real(8):: a,b
 !real(8):: func2 !if not using 'real function' 
 func2=a+b
 end function func2
end module myModule
