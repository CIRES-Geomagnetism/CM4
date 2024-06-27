# Warm up practice: creating the Python wrappers for Fortran 

## Set up the Git environment

1. Pull the git repository CM4(https://github.com/CIRES-Geomagnetism/CM4/tree/main) to local
2. Create the git branch
	- `git checkout -b <your branch name>`

## Fortran to Python 

1. Install numpy
1. Transform the Fortran code to Python module by f2py
	- `f2py -c math_example.f90 -m fib1`
		- This command tells f2py to compile (-c) the Fortran source file math_example.f90 and create a Python module named mymodule (-m mymodule).
2. Create the python file to call the Python module you obtained from previously
3. Print the input and ouput from Python codes.
4. Upload the python file to the git branch you created.

 
