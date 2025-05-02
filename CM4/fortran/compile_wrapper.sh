#!/bin/bash

rm *.o
gfortran -c call_cm4field.f90 cm4field_.F
gfortran c_example_wrapper.c call_cm4field.o cm4field_.o -o c_example
./c_example
