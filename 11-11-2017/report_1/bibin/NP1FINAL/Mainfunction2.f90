program Mainfunction2 !program start
use Newton
use lagrange  !to call the module in the main program
  implicit none!to force the programmer to define all the variables

  real(8),dimension(0:4)::X4,Y24 !(X4:Datapoints for 4 order,Y14: Function values at datapoints for 1 problem,Y24: Function values at datapoints for 2 problem )
  real(8),dimension(0:9)::X9,Lagint9,Lagint4,xx,Lagrangey2,error24,&
error29,Y29,Newint4,error14n,error24n,Newint9,error19n,error29n !(X9:Datapoints for 9 points,Y19: Function values at datapoints for 1 problem,Y29: Function values at datapoints for 2 problem, Lagint9:inerpolated value for 9 order,Lagint4:interpolated value for 4 order,Lagrange:points at which interpolation is done,Lagrangey:function values at data points to calulate error,error14:error in 1 problem and 4 order,error19:error in 1 problem and 9 order,error24:error in 2 problem and 4 order,error29:error in 2 problem and 9 order )
  integer::i,j,N,k
  character(len=30)::myfilename
  real(8)::A,B,H
  real(8), parameter::pi=3.14

  B=1 !Max range
  A=-1!min range

  xx(:)=(/-0.93,-0.77,-0.64,-0.35,-0.23,0.0,0.13,0.26,0.53,0.76/)


  X4(:)=(/-1.0,-0.5,0.0,0.5,1.0/)                                !datapoints for 4th order
  X9(:)=(/-1.0,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8/)         !datapoints for 9th order
  Y24(:)=cos(X4(:)*pi)                                           !values defined at data points
  Y29(:)=cos(X9(:)*pi)                                           !values defined at data points
  N=9                                                            !number of points at which the interpolation needs to be done

 xx(:)=(/-1.0,-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8/)
 Lagrangey2(:)=cos(xx(:)*pi) 

do i=0,N
  Lagint4(i)=lgrnge(X4,Y24,4,xx(i))
 end do

do i=0,N
  Newint4(i)=order4(X4,Y24,4,xx(i))
 end do

do j=0,N
  Lagint9(j)=lgrnge(X9,Y29,9,xx(j))
 end do
do j=0,N
  Newint9(j)=order4(X9,Y29,9,xx(j))
 end do

myfilename='function2.dat'
open(99,file=myfilename)

write(99,*)'for LAGRANGE'
write(99,*)'for function f(x)=cos(x*pi), X VALUES'
write(99,*)xx
write(99,*)'for function f(x)=cos(x*pi), Y VALUES'
write(99,*)lagrangey2
write(99,*)'4th order'
write(99,*)Lagint4
write(99,*)'9th order'
write(99,*)Lagint9
write(99,*)'for NEWTON'
write(99,*)'for function f(x)=cos(x*pi)'
write(99,*)'4th order'
write(99,*)Newint4
write(99,*)'9th order'
write(99,*)Newint9
 close(99)


end Program Mainfunction2
