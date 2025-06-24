from setuptools._distutils.core import setup, Extension
import os

src_dir = os.path.join("CM4", "fortran")

module = Extension("cm4.fortran",
                   sources=[os.path.join(src_dir, "cm4field_.F"), os.path.join(src_dir, "call_cm4field_array.f90"),
                            os.path.join(src_dir, "c_wrapper.c"), os.path.join(src_dir, "ccm4.c")],
                   extra_compile_args=["-O3", "-ffree-form", "-fno-range-check"],
                   extra_link_args=["-lgfortran"])

setup(
    name="python_cm4",
    packages = ["python_cm4"],
    ext_modules=[module],
    version = "0.1.0",
)