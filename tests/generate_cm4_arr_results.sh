#!/bin/bash

set -e

# Gets the absolute path of the directory containing this script
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
TOP_DIR=$(dirname "$SCRIPT_DIR")
exe_dir="${TOP_DIR}/CM4"
test_dir="${TOP_DIR}/tests"
test_values_dir="${test_dir}/test_values"
field=""

echo $exe_dir
out_exe="create_cm4_results"
while getopts "d:f:h" opt; do
  case $opt in
    d) exe_dir="$OPTARG" ;;
    f) field="$OPTARG" ;;
    h) echo "Usage: $0 -d <path_directory> -f <field name e.g. core, crust, iono, magneto>"
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

if [ -n "$field" ]; then
    echo "Running for field: $field"
    ./${out_exe} -k "$field" -i "${test_values_dir}/cm4_fortran_${field}_inputs.csv" -f "${test_values_dir}/cm4_${field}_TestValues.csv"
    exit 0
else
  echo "Running for core, crust, iono and magneto field."
  ./${out_exe} -k "core" -i "${test_values_dir}/cm4_fortran_core_inputs.csv" -f "${test_values_dir}/cm4_core_TestValues.csv"
  ./${out_exe} -k "crust" -i "${test_values_dir}/cm4_fortran_crust_inputs.csv" -f "${test_values_dir}/cm4_crust_TestValues.csv"
  ./${out_exe} -k "iono" -i "${test_values_dir}/cm4_fortran_iono_inputs.csv" -f "${test_values_dir}/cm4_iono_TestValues.csv"
  ./${out_exe} -k "magneto" -i "${test_values_dir}/cm4_fortran_magneto_inputs.csv" -f "${test_values_dir}/cm4_magneto_TestValues.csv"
fi
