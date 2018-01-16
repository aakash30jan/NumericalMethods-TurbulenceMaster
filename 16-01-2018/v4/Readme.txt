main.f90 is the main program with declared parameters and function calls
module_schemes.f90 is the module which contains the following schemes .
1. Euler Explicit Scheme
2. Euler Implicit Scheme
3. Crank Nicolsan Scheme (not 100% sure about the implementation)

Using thomas algorithm as a linear solver (pasted code of thomas algorithm instead of calling function)

To compile:
uncomment the scheme function call in main.f90 which you want to use and comment the rest

for the first run, execute:
"touch h_Vs_error.dat" 
"touch k_Vs_error.dat" 
this will create blank files and not give error

then execute:
"gfortran -c module_schemes.f90"
"gfortran module_schemes.o main.f90 -o main.exe"
"./main.exe" to run the program



Repeat the following steps by changing h or k or other parameters as required:
"gfortran module_schemes.o main.f90 -o main.exe"
"./main.exe" to run the program


the result files are over written as I have used the same name for files 


#the results are located under results folder
it has on plotting script and png plots with all the data for 3 schemes
