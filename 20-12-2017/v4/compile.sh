echo "parallel implementation"
gfortran -fopenmp -c module_gaussSiedel.f90 
gfortran -fopenmp -c module_gaussSiedelRelaxed.f90 
gfortran -fopenmp -c module_jacobi.f90 
gfortran module_gaussSiedel.o module_gaussSiedelRelaxed.o module_jacobi.o poisson_2D.f90 -fopenmp -o poisson_2D.exe
./poisson_2D.exe

