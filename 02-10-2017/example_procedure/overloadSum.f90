module overloadSum
 implicit none
 
 type field
  real(8):: u,v,w
  character(len=1):: field_name
 end type field 
 
 interface operator(+)
 procedure fieldsum
 end interface operator(+)

 contains
 
 function fieldsum(field1,field2)
  implicit none	
  type(field), intent(in)::field1,field2
  type(field) ::fieldsum
  
  fieldsum%u=field2%u+field1%u
  fieldsum%v=field2%v+field1%v
  fieldsum%w=field2%w+field1%w

 end function fieldsum

end module overloadSum
