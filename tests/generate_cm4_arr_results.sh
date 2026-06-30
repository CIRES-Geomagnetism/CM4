#!/bin/bash

set -e

# Gets the absolute path of the directory containing this script
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
TOP_DIR=$(dirname "$SCRIPT_DIR")
exe_dir="${TOP_DIR}/CM4"
test_dir="${TOP_DIR}/tests"
test_values_dir="${test_dir}/test_values"
echo $exe_dir
out_exe="create_cm4_results"
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

gfortran -I${exe_dir} -I${test_dir} \
    "${test_dir}/create_cm4_arr_results.c" \
    "${test_dir}/cm4_c_library.c" \
    "${exe_dir}/call_cm4field_array.f90" \
    "${exe_dir}/cm4field_.F" \
    -o "${out_exe}"

./${out_exe} -k "core" -i "${test_values_dir}/cm4_fortran_core_inputs.csv" -f "${test_values_dir}/cm4_core_TestValues.csv"