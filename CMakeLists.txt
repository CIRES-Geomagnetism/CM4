cmake_minimum_required(VERSION 3.18)
project(python_CM4 LANGUAGES Fortran)

find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)


set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build_lib)
file(MAKE_DIRECTORY ${CMAKE_BUILD_DIR})

# Get the Python extension suffix (e.g., .cpython-313-darwin.so)
execute_process(
  COMMAND ${Python_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))"
  OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
  OUTPUT_STRIP_TRAILING_WHITESPACE
)


# === Build cm4field ===


# Output file tracking
set(F2PY_STAMP "${CMAKE_CURRENT_BINARY_DIR}/cm4field_build.stamp")

# Add custom command that calls the Python script
add_custom_command(
  OUTPUT ${F2PY_STAMP}
  COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/build_f2py_cm4.py
  COMMAND ${CMAKE_COMMAND} -E touch ${F2PY_STAMP}
  COMMENT "Building cm4field Fortran module using f2py"
  VERBATIM
)

# Make a target from the command
add_custom_target(build_f2py_module ALL DEPENDS ${F2PY_STAMP})

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

