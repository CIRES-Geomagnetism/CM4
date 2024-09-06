from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext

# Define Fortran extension modules
cm4field_ext = Extension(
    name="CM4.lib.cm4field_module",
    sources=["CM4/lib/CM4venv/cm4field.f"],
    extra_compile_args=["-O3"],
)

hello_ext = Extension(
    name="CM4.lib.hello_module",
    sources=["CM4/lib/CM4venv/hello.f90"],
    extra_compile_args=["-O3"],
)

interactive_cm4_ext = Extension(
    name="CM4.lib.interactive_cm4_module",
    sources=["CM4/lib/CM4venv/interactive_cm4.f"],
    extra_compile_args=["-O3"],
)

setup(
    name="CM4",
    version="0.1.0",
    description="Python wrapper for CM4 model with Fortran integration",
    author="Your Name",
    author_email="your.email@example.com",
    packages=["CM4"],
    install_requires=["numpy"],
    ext_modules=[cm4field_ext, hello_ext, interactive_cm4_ext],
    cmdclass={"build_ext": build_ext},
)