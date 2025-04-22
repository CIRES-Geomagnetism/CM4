project('python_cm4', 'fortran', version : '0.1.0', meson_version: '>=1.0.0')

add_languages('fortran')

python = import('python')
py_mod = python.find_installation('python')
py_dep = py_mod.dependency()
f2py = find_program('f2py', required: true)

incdir_numpy = run_command(py_mod,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py_mod,
    ['-c', 'import os; os.chdir(".."); import numpy.f2py; print(numpy.f2py.get_include())'],
    check : true
).stdout().strip()

inc_np = include_directories(incdir_numpy, incdir_f2py)

ext_suffix = py_mod.get_variable('EXT_SUFFIX')


# Build cm4filed 
src_dir = join_paths('CM4', 'fortran')
cm4field_file =  join_paths(src_dir, 'cm4field_.F')
py_mod.extension_module('cm4field',
  [cm4field_file],
  incdir_f2py / 'fortranobject.c',
  include_directories: inc_np,

  dependencies : py_dep,
  install : true
)

#Build cm4field_arr
cm4field_arr_file = join_paths(src_dir, 'cm4field_array.F')
py_mod.extension_module('cm4field_arr',
  [cm4field_arr_file],
  include_directories: inc_np,
  dependencies : py_dep,
  install : true
)

py_mod.install_sources(
  [join_paths('CM4', '__init__.py'), join_paths('CM4', 'callfpy.py')],  # list your Python files here
  subdir: 'CM4'  # this is relative to site-packages
)
