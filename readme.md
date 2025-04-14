This package wraps CM4 fortran code in python 3.10, but can work with 3.9 with the command:
f2py -c -m cm4_py310 cm4field_.F
The file cm4field_.F contains two important functions. At the beginning of the file, there is the original function CM4_FIELD. The second function in the file, is the function wrapped by python. This function simply calls the CM4 field function. 

In call_cm4_from_F, there is a function set up to call the original CM4_FIELD function. 

cm4field_array.F contains a function wrapped in python to call CM4_FIELD with array input alt, lat, lon, f107 and dst. 