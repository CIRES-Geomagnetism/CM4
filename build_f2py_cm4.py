import subprocess
import sysconfig
import shutil
import os

# Build cm4field_arr
mod_cm4arr = "cm4field_arr"
source = "CM4/fortran/cm4field_array.F"
ext_suffix = sysconfig.get_config_var("EXT_SUFFIX")
cm4_so_file = f"{mod_cm4arr}{ext_suffix}"

subprocess.run(["f2py", "-c", "-m", mod_cm4arr, source], check=True)

# Build cm4field
mod_cm4 = "cm4field"
source = "CM4/fortran/cm4field_.F"
ext_suffix = sysconfig.get_config_var("EXT_SUFFIX")
cm4arr_so_file = f"{mod_cm4}{ext_suffix}"

subprocess.run(["f2py", "-c", "-m", mod_cm4, source], check=True)

# Move to expected package dir (adjust if needed)
dest = os.path.join("build_lib", cm4_so_file)
print(f"Moving {cm4_so_file} → {dest}")
shutil.move(cm4_so_file, dest)

dest = os.path.join("build_lib", cm4arr_so_file)
print(f"Moving {cm4arr_so_file} → {dest}")
shutil.move(cm4arr_so_file, dest)
