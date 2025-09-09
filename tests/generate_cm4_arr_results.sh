#!/bin/bash


set -e

exe_dir="/home/liamkilcommons/Projects/CM4/CM4"
out_exe="cm4_results"
while getopts "d:h" opt; do
  case $opt in
    d) exe_dir="$OPTARG" ;;
    h) echo "Usage: $0 -d <path_directory>"
       exit 0 ;;
    *) echo "Invalid option"
       exit 1 ;;
  esac
done

if [ -f ${out_exe} ]; then
  echo "Removing old executable ${out_exe}"
  rm -f ${out_exe} *.o
fi


#gfortran -c ${exe_dir}/call_cm4field.f90 ${exe_dir}/cm4field_.F

gfortran create_cm4_arr_results.c ${exe_dir}/call_cm4field_array.f90 ${exe_dir}/cm4field_.F  -o $out_exe
./${out_exe}
