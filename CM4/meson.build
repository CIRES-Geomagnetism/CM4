python = import('python').find_installation('python3')

numpy_include = run_command(
  python,
  '-c',
  'import numpy; print(numpy.get_include())'
).stdout().strip()

np_include_dir = join_paths(python.get_install_dir(), 'numpy', '_core', 'include')


numpy_dep = declare_dependency(
  include_directories: include_directories(numpy_include)
)


# Build the Python extension module
pyext = python.extension_module(
  'cm4field_arr',
  ['ccm4.c', 'c_wrapper.c', 'call_cm4field_array.f90', 'cm4field_.F'],
  dependencies: [],
  install_dir: join_paths(python.get_install_dir(), 'cm4'),
  include_directories: include_directories(np_include_dir),
  install: true
)

# Install additional pure Python files
python.install_sources(
  ['__init__.py','callfpy.py', 'umdl.CM4'],
  subdir: 'cm4'   # Places this into site-packages/CM4/
)