program readdata
  implicit none
  real, allocatable :: myData(:,:)
  real              :: myLine
  integer           :: i, j, myRow, myColumn
  character(len=30) :: myFileName
  character(len=30) :: myFormat

  myFileName='data.dat'

  open(99, file=myFileName)
  write(*,*)'open data file'
  read(99, *) myRow
  read(99, *) myColumn

  allocate(myData(myRow,myColumn))
  !allocate(myData(myRow+1,myColumn))

  do i=1,myRow
    !read(99,*) myData(i,:)
    read(99,'(6(E11.4,X))') myData(i,:)
    print '(6(E11.4,X))', myData(i,:)
    !print *, myData(i,:)
  enddo


  close(99)

end program readdata

