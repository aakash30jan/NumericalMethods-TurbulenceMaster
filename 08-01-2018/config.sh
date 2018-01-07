#config v1
echo "Mesh Generation Framework"
echo "Checking for dependencies"
#check dependencies
        #gfortran #hdf5lib1.8 
echo "Installing dependencies"
#install dependencies
 

#build
echo "Building from source" 
rm -r ./bin
mkdir bin 
cd ./src

#cleaning
echo "Checking for previous buid(s), removing if any"
find . -type f -name '*.o' -delete
find . -type f -name '*.mod' -delete
find . -type f -name '*.exe' -delete

echo "Building . . . . "
mkdir temp
find . -type f -name 'module_*.f90' -exec cp -r -t ./temp/ {} +
cd temp 
gfortran -c module_precision.f90
gfortran -c module_*.f90
cp ../mainProg.f90 mainProg.f90
gfortran *.o mainProg.f90 -o mainProg.exe
cp mainProg.exe ../../bin/mainProgram.exe
cd ..
rm -r temp
cd ..

echo "Build complete successfully"

#automatic test
echo "Performing tests(s)"
cd ./bin
./mainProgram.exe
echo "Test was successfull !"

