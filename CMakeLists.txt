cmake_minimum_required(VERSION 3.18)
project(python_CM4 LANGUAGES Fortran)

find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)


# Ensure f2py is available and use it for Fortran to Python wrapping
#find_program(F2PY_EXECUTABLE NAMES f2py3 f2py)


# Get the Python extension suffix (e.g., .cpython-313-darwin.so)
execute_process(
  COMMAND ${Python3_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))"
  OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Define build directory for the .so file
set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build_lib)
file(MAKE_DIRECTORY ${CMAKE_BUILD_DIR})


# === Build cm4field ===


# Build shared library from your Fortran source
add_library(cm4field MODULE CM4/fortran/cm4field_.F)


set_target_properties(cm4field PROPERTIES
    PREFIX ""
    OUTPUT_NAME "cm4field_${PYTHON_EXT_SUFFIX}"
)




# === Build cm4field_arr ===
# Build shared library from your Fortran source
add_library(cm4field_arr MODULE CM4/fortran/cm4field_array.F)


set_target_properties(cm4field_arr PROPERTIES
    PREFIX ""
    OUTPUT_NAME "cm4field_arr${PYTHON_EXT_SUFFIX}"
)

# === Get site-packages path ===
execute_process(
    COMMAND ${Python_EXECUTABLE} -c "import site; print(site.getsitepackages()[0])"
     RESULT_VARIABLE _site_result
     OUTPUT_VARIABLE Python_SITE_PACKAGES
     OUTPUT_STRIP_TRAILING_WHITESPACE
)

# === Install targets ===
install(FILES ${CMAKE_BUILD_DIR}/cm4field_arr${PYTHON_EXT_SUFFIX}
        DESTINATION ${Python_SITE_PACKAGES}/CM4)

install(FILES ${CMAKE_BUILD_DIR}/cm4field${PYTHON_EXT_SUFFIX}
        DESTINATION ${Python_SITE_PACKAGES}/CM4)


# Install Python files
install(DIRECTORY CM4/ DESTINATION ${Python_SITE_PACKAGES}/CM4/)

