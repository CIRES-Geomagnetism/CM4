project('python_cm4', 'fortran', version : '0.1.0', meson_version: '>=1.0.0')

add_languages('fortran','c')

# Find the Fortran compiler and Python development libraries
fortran = meson.get_compiler('fortran')
python = import('python')

# Define the source files
src_dir = join_paths('CM4', 'fortran')
cm4field_fsrc = join_paths(src_dir, 'cm4field_.F')
call_cm4_fsrc = join_paths(src_dir, 'call_cm4field.f90')
c_src = join_paths(src_dir, 'c_wrapper.c')



# Create the extension module
py_inst = python.find_installation('python3')
extension = py_inst.extension_module('cm4field',
  sources: [cm4field_fsrc, call_cm4_fsrc, c_src],
  install: true
)

# Install the compiled extension
py_inst.install_sources(
  [join_paths('CM4', '__init__.py'), join_paths('CM4', 'callfpy.py')],
  subdir: 'CM4'  # This places them in the CM4 directory under site-packages
)