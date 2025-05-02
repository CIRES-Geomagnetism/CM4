#!/bin/bash

out_exe="c_example"

rm *.o
rm $out_exe
gfortran -c call_cm4field_array.f90 cm4field_.F
gfortran c_example_wrapper.c call_cm4field_array.o cm4field_.o -o $out_exe
./c_example
