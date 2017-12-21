gfortran -c module_gaussSiedel.f90 
gfortran -c module_gaussSiedelRelaxed.f90 
gfortran -c module_jacobi.f90 
gfortran module_gaussSiedel.o module_gaussSiedelRelaxed.o module_jacobi.o poisson_2D.f90 -o poisson_2D.exe
./poisson_2D.exe

