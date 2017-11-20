program Thomas
implicit none

real,allocatable::a(:), b(:), c(:), d(:), x(:), e(:), f(:), g(:), k(:), xx(:), xxy(:), exact(:), ddx, M4, segder
integer:: aa, bb, cc, dd, N, i, NN, j
real::acum, dx, error
character(len=20) :: nameprogr
 !print*, 'introduce the dimension of the system'
 !read*, N
 !allocate (a(N-1), b(N), c(N-1), d(N), x(N))
 !do i=1,N-1
 !print*, 'Now introduce the values of the lower diagonal'
 !read*,aa
 !a(i+1)=aa
 !end do
 !print*, 'Now introduce the values of the mains diagonal'
 !do i=1,N
 !read*,bb
 !b(i)=bb
 !end do
 !print*, 'Now introduce the values of the upper diagonal'
 !do i=1,N-1
 !read*,cc
 !c(i)=cc
 !end do
 !print*, 'Now introduce the values of the independent vector'
 !do i=1,N
 !read*,dd
 !d(i)=dd
 !end do


!!Forward elimination

!do i=1,N-1
!b(i+1)=b(i+1)-c(i)*a(i+1)/b(i)
!d(i+1)=d(i+1)-d(i)*a(i+1)/b(i)
!end do

 !!Backward substitution

 !x(N)=d(N)/b(N)
 !do i=N-1, 1, -1
 !x(i)=(d(i)-c(i)*x(i+1))/b(i)
 !end do

 !print*,'the solution of x is', x


 !!HASTA AQUI FUNCIONAAAAAAAAAAAAAA


 !Now we are going to check this algorithm with a scheme of second order with the function sen(pi*x)
 !We are going to study the interval [-1,1]
 !e is equivalent to a
 !f is equivalent to b
 !g is equivalent to c
 !k is equivalent to d
 !xxy is the equivalent of x

 !print*, 'introduce the number of points to discretice the scheme'
 !read*, NN

!VOY A HACER UN BUCLE CON PUNTOS PARA SACAR EL ERROR EN FUNCION DE LOS PUNTOS, UNA VEZ ACABADO DESCOMENTAR LAS DOS LINEAS DE ARRIBA.
 write (nameprogr,*) 'pointlist.dat'
 open(1,file=nameprogr) 
 NN=10
 do j=1, 1000
 NN=NN+10
 allocate(e(NN-1), f(NN), g(NN-1), xx(NN), k(NN), xxy(NN), exact(NN))

 xx(1)=-1
 dx=2.0/real(NN-1)
 do i=1,NN-1
 xx(i+1)=xx(i)+dx
 end do

 do i=1,NN
 f(i)=-2
 exact(i)=sin(xx(i)*3.14159265)
 k(i)=-(dx**2)*(3.14159265**2)*sin(xx(i)*3.14159265)
 end do

 do i=1,NN-1
 e(i+1)=1
 g(i)=1
 end do

   !do i=1,NN
   !print*, i, xx(i), xxy(i), k(i)
   !end do
 
!!Forward elimination

do i=1,NN-1
f(i+1)=f(i+1)-g(i)*e(i+1)/f(i)
k(i+1)=k(i+1)-k(i)*e(i+1)/f(i)
end do

 !!Backward substitution

 xxy(NN)=k(NN)/f(NN)
 do i=NN-1, 1, -1
 xxy(i)=(k(i)-g(i)*xxy(i+1))/f(i)
 end do

do i=1,NN
print*, exact(i), xxy(i)
end do


!NOW WE ARE GOING TO CALCULATE THE ERROR
error=0.0
do i=1,NN
error=error+abs(exact(i)-xxy(i))
end do
error=((error**2.0)*dx)**(0.5)


 print*, 'el resultado es', error, NN
 
 write(1,*) error, dx, NN
 print*,NN
 deallocate (e, f, g, xx, k, xxy, exact)
 end do
 close(1)

!I AM GOING TO TRY THE MINIMUM ERROR. FIRST CALCULATE E4
ddx=2.0/999.0
segder=-1.0
M4=0.0
do i=1,1000
M4=M4+abs((3.14159265**4.0)*sin(3.14159265*segder))
segder=segder+ddx
end do
M4=M4/1000.0
print*,'M4 es', M4
!ME DA 68.87, Y EN EL PROGRAMA 61.95, OJO¡¡¡


 end program

