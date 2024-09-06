import numpy as np
#Fortran helloworld as proof of concept
import hello_module

hello_module.hello()

import CM4.lib.cm4field_module

print(dir(CM4.lib.cm4field_module))

import CM4.lib.interactive_cm4_module

print(dir(CM4.lib.interactive_cm4_module))