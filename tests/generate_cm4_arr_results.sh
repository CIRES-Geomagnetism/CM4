#!/bin/bash


set -e

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]:-$0}")" &> /dev/null && pwd)

exe_dir=$(dirname ${SCRIPT_DIR})/CM4

cd "${SCRIPT_DIR}"


out_exe="${SCRIPT_DIR}/create_cm4_results"
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

# Workaround for GCC 14.2.0 broken include-fixed directory on macOS
# Compile C code with clang and Fortran with gfortran, then link together
clang -c ${SCRIPT_DIR}/create_cm4_arr_results.c -o ${SCRIPT_DIR}/create_cm4_arr_results.o
gfortran -c ${exe_dir}/call_cm4field_array.f90 -o ${SCRIPT_DIR}/call_cm4field_array.o
gfortran -c ${exe_dir}/cm4field_.F -o ${SCRIPT_DIR}/cm4field_.o
gfortran ${SCRIPT_DIR}/create_cm4_arr_results.o ${SCRIPT_DIR}/call_cm4field_array.o ${SCRIPT_DIR}/cm4field_.o -o $out_exe
CM4_COEFF_PATH="${SCRIPT_DIR}/umdl.CM4" "${out_exe}"
