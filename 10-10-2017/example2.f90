program writedata
  implicit none
  real, allocatable :: myData(:,:)
  real              :: myLine
  integer           :: i, j, myRow, myColumn
  character(len=30) :: myFileName
  character(len=30) :: myFormat

  myFileName='data2.dat'

  !open(99, file=myFileName)
  open(99, file=myFileName, status='old', access='append')
  write(*,*)'open data file'
  
  write(99,*) 5.666346
  allocate(myData(2,2))
  myData(1,1)=0.242
  myData(1,2)=0.225
  myData(2,1)=0.25
  myData(2,2)=0.225
  write(99,*) myData
  close(99)

end program writedata

