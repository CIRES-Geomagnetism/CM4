cmake_minimum_required(VERSION 3.18)
find_package(Python3 REQUIRED COMPONENTS Interpreter Development)
project(python_CM4 LANGUAGES C Fortran)




# Get the Python extension suffix (e.g., .cpython-313-darwin.so)
execute_process(
  COMMAND ${Python3_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))"
  OUTPUT_VARIABLE PYTHON_EXT_SUFFIX
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Define build directory for the .so file
set(CMAKE_BUILD_DIR ${CMAKE_CURRENT_SOURCE_DIR}/build_lib)
file(MAKE_DIRECTORY ${CMAKE_BUILD_DIR})



# === Build cm4field_arr ===
add_library(cm4field_arr MODULE CM4/fortran/cm4field_.F CM4/fortran/call_cm4field_array.f90 CM4/fortran/ccm4.c CM4/fortran/c_wrapper.c)

target_include_directories(cm4field_arr PUBLIC ${Python3_INCLUDE_DIRS} CM4/fortran)
target_link_libraries(cm4field_arr PRIVATE ${Python3_LIBRARIES})

# === Get site-packages path ===
execute_process(
    COMMAND ${Python_EXECUTABLE} -c "import site; print(site.getsitepackages()[0])"
     RESULT_VARIABLE _site_result
     OUTPUT_VARIABLE Python_SITE_PACKAGES
     OUTPUT_STRIP_TRAILING_WHITESPACE
)

set_target_properties( cm4field_arr PROPERTIES
    PREFIX ""
    OUTPUT_NAME "cm4field_arr"
)

# === Install targets ===
install(FILES ${CMAKE_BUILD_DIR}/cm4field_arr${PYTHON_EXT_SUFFIX}
        DESTINATION ${Python_SITE_PACKAGES}/CM4)

# Install Python files
install(DIRECTORY CM4/ DESTINATION ${Python_SITE_PACKAGES}/CM4/)

