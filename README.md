# Requirements
1. GNUFortran compiler
2. GNUPlot

# How to build and run examples (Linux)
1. gfortran -c fortplotlib.f90
2. gfortran -o example1 example1.f90 fortplotlib.o
3. gfortran -o example2 example2.f90 fortplotlib.o
4. ./example1
5. ./example2

# How to create archived library
1. ar -r fortplotlib.a *.o
