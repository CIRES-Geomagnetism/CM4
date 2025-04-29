cmake_minimum_required(VERSION 3.18)
project(python_CM4 LANGUAGES Fortran)

find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)


# Ensure f2py is available and use it for Fortran to Python wrapping
find_program(F2PY_EXECUTABLE NAMES f2py3 f2py)


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
# Build Fortran code into a shared object
add_custom_command(
  OUTPUT ${CMAKE_BUILD_DIR}/cm4field${PYTHON_EXT_SUFFIX}
  COMMAND ${F2PY_EXECUTABLE} -c -m cm4field CM4/fortran/cm4field_.F
  COMMAND ${CMAKE_COMMAND} -E rename
          cm4field${PYTHON_EXT_SUFFIX}
          ${CMAKE_BUILD_DIR}/cm4field${PYTHON_EXT_SUFFIX}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMENT "Building cm4field Fortran extension"
)



add_custom_target(build_cm4field ALL
  DEPENDS ${CMAKE_BUILD_DIR}/cm4field${PYTHON_EXT_SUFFIX}
)



# === Build cm4field_arr ===
add_custom_command(
  OUTPUT ${CMAKE_BUILD_DIR}/cm4field_arr${PYTHON_EXT_SUFFIX}
  COMMAND ${F2PY_EXECUTABLE} -c -m cm4field_arr CM4/fortran/cm4field_array.F
  COMMAND ${CMAKE_COMMAND} -E rename
          cm4field_arr${PYTHON_EXT_SUFFIX}
          ${CMAKE_BUILD_DIR}/cm4field_arr${PYTHON_EXT_SUFFIX}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMENT "Building cm4field_array Fortran extension"
)

add_custom_target(build_cm4field_arr ALL
  DEPENDS ${CMAKE_BUILD_DIR}/cm4field_arr${PYTHON_EXT_SUFFIX}
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

