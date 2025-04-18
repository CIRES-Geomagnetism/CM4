project('python_CM4', 'fortran')

py_mod = import('python')
python = py_mod.find_installation()



python.extension_module(
  'cm4field',
  sources : ['src/CM4/cm4field_.F'],
  install : true,
  subdir : 'CM4'
)

python.extension_module(
  'cm4field_array',
  sources : ['src/CM4/cm4field_array.F'],
  install : true,
  subdir : 'CM4'
)

# Install Python sources
python.install_sources('src/CM4/*.py', subdir : 'CM4')