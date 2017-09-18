program ifstat
!implicit none
integer :: count
print *,'enter number'
read*, count
if (count == 1) then
  print*, 'First element'
else if (count == 10) then
  print*, 'Last element'
else 
  print*, 'not defined'
end if


end program ifstat
